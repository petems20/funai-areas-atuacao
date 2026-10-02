# ==============================================================================
# IMPUTAÇÃO DO NORTE -- versão DuckDB+spatial (usada pelo alvo df_imputacao_norte)
# ==============================================================================
# Versão alternativa de `calcular_imputacoes_norte()` (R/geoprocessamento.R), que
# NÃO é alterada por este arquivo. Usa DuckDB + extensão spatial para as operações
# geométricas que se mostraram instáveis/lentas em sf/GEOS (snapping
# por vizinho mais próximo + distância par-a-par; clustering de polígonos por
# proximidade), mantendo sfnetworks/igraph para o roteamento em grafo (que nunca foi
# o gargalo). É fonteada pelo `_targets.R` e usada no alvo `df_imputacao_norte`.
#
# Achados de viabilidade (medidos em benchmark contra a versão original):
# - Corretude: distâncias batem com sf::st_distance() a ~1e-9m (ruído de ponto
#   flutuante) na etapa de snapping.
# - Estabilidade: não houve nenhuma falha de segmentação no snapping em escala real
#   (66.188 pontos), diferente do bug original do sf (`by_element = TRUE` segfaultava
#   em lotes de ~6500+ pares).
# - Performance: mais LENTO que a versão sf já corrigida (chunked) para o snapping
#   nesta base -- a malha rodoviária é esparsa (a maioria dos pontos não tem estrada a
#   poucos km de distância), então o pré-filtro espacial (buffer + R-tree) do DuckDB
#   não poda muito, e falta ao DuckDB spatial um operador nativo de
#   k-vizinhos-mais-próximos (diferente do STRtree do GEOS usado por
#   `sf::st_nearest_feature()`, que já era rápido mesmo sem essa correção).

#' @title Vizinho mais próximo + distância via DuckDB spatial (substitui
#' `sf::st_nearest_feature()` + `st_distance(..., by_element = TRUE)`)
#' @description Para cada linha de `x`, encontra a linha de `y` geometricamente mais
#' próxima e a distância entre elas, via DuckDB + extensão spatial. Duas passadas:
#' (1) um join com predicado `ST_Intersects(ST_Buffer(x, raio_m), y)` -- só esse
#' predicado aciona o índice R-tree do DuckDB (`ST_Distance` sozinho não aciona);
#' (2) um `CROSS JOIN` sem filtro espacial só para as linhas de `x` que não acharam
#' nenhum candidato dentro do raio (fallback, sem limite de distância, replicando a
#' semântica de `sf::st_nearest_feature()`).
#' @param conn Conexão DuckDB já com a extensão spatial carregada.
#' @param x,y Objetos sf (podem ter tamanhos diferentes). O CRS não precisa ser
#' reconhecido pelo DuckDB (`ignore_unknown_crs`, setado por
#' `.duckdb_conn_imputacao()`) -- as coordenadas já devem estar num CRS métrico
#' projetado antes de chamar esta função.
#' @param id_x Nome da coluna em `x` usada como identificador estável de cada linha.
#' @param raio_m Raio inicial (metros) do pré-filtro espacial.
#' @return Data.frame com colunas `id_x`, `id_y` (posição 1-based da linha
#' correspondente em `y`), `dist` (metros).
.duckdb_nearest_com_distancia <- function(conn, x, y, id_x, raio_m = 20000) {
  geom_x <- attr(x, "sf_column")
  geom_y <- attr(y, "sf_column")

  x_df <- x |> dplyr::mutate(.id_x = .data[[id_x]])
  y_df <- y |> dplyr::mutate(.id_y = dplyr::row_number())

  duckspatial::ddbs_write_table(conn, x_df, "tbl_x", overwrite = TRUE, quiet = TRUE)
  duckspatial::ddbs_write_table(conn, y_df, "tbl_y", overwrite = TRUE, quiet = TRUE)

  q1 <- sprintf('
    WITH cand AS (
      SELECT x.".id_x" AS id_x, y.".id_y" AS id_y,
             ST_Distance(x.%s, y.%s) AS dist
      FROM tbl_x x
      JOIN tbl_y y
        ON ST_Intersects(ST_Buffer(x.%s, %f), y.%s)
    )
    SELECT id_x, id_y, dist
    FROM cand
    QUALIFY ROW_NUMBER() OVER (PARTITION BY id_x ORDER BY dist) = 1
  ', geom_x, geom_y, geom_x, raio_m, geom_y)
  res1 <- DBI::dbGetQuery(conn, q1)

  pendentes <- setdiff(x_df[[".id_x"]], res1$id_x)
  if (length(pendentes) == 0) return(res1)

  pendentes_str <- paste(pendentes, collapse = ",")
  q2 <- sprintf('
    WITH cand AS (
      SELECT x.".id_x" AS id_x, y.".id_y" AS id_y,
             ST_Distance(x.%s, y.%s) AS dist
      FROM tbl_x x
      JOIN tbl_y y ON x.".id_x" IN (%s)
    )
    SELECT id_x, id_y, dist
    FROM cand
    QUALIFY ROW_NUMBER() OVER (PARTITION BY id_x ORDER BY dist) = 1
  ', geom_x, geom_y, pendentes_str)
  res2 <- DBI::dbGetQuery(conn, q2)

  dplyr::bind_rows(res1, res2)
}

#' @title Lista de arestas "está a até `dist_m` metros" via DuckDB spatial (substitui
#' `sf::st_is_within_distance()`)
#' @param conn Conexão DuckDB já com a extensão spatial carregada.
#' @param sf_grupo Objeto sf (um grupo/CR por vez, como no `group_split()` original).
#' @param dist_m Distância de tolerância, em metros (CRS já projetado).
#' @return Data.frame com colunas `id_a`, `id_b` (1-based, `id_a < id_b`).
.duckdb_within_distance_edges <- function(conn, sf_grupo, dist_m = 200) {
  geom <- attr(sf_grupo, "sf_column")
  df <- sf_grupo |> dplyr::mutate(.id = dplyr::row_number())
  duckspatial::ddbs_write_table(conn, df, "tbl_grupo", overwrite = TRUE, quiet = TRUE)

  q <- sprintf('
    SELECT a.".id" AS id_a, b.".id" AS id_b
    FROM tbl_grupo a
    JOIN tbl_grupo b
      ON a.".id" < b.".id"
     AND ST_Intersects(ST_Buffer(a.%s, %f), b.%s)
  ', geom, dist_m, geom)
  DBI::dbGetQuery(conn, q)
}

#' @title Abre uma conexão DuckDB com a extensão spatial carregada, pronta para uso
#' pelas funções `.duckdb_*` deste arquivo.
#' @description Limita memória/threads explicitamente dado o histórico de pouca folga
#' de RAM nesta máquina (ver notas de depuração de `df_imputacao_norte`).
.duckdb_conn_imputacao <- function(memory_limit_gb = 6, threads = 3) {
  conn <- duckspatial::ddbs_create_conn(dbdir = "memory", threads = threads, memory_limit_gb = memory_limit_gb)
  duckspatial::ddbs_load(conn)
  DBI::dbExecute(conn, "SET ignore_unknown_crs = true;")
  conn
}

#' @title Limpa enclaves topológicos (versão DuckDB de `limpar_enclaves_funai()`)
#' @description ACHADO DE VIABILIDADE (mantida no arquivo por documentação, mas NÃO
#' usada por `calcular_imputacoes_norte_duckdb()` -- ver essa função, que usa a versão
#' original S2/sf em vez desta): a abordagem de fazer um self-join por grupo de CR no
#' DuckDB (uma chamada `ddbs_write_table()` + query por grupo, até ~30-40 grupos) se
#' mostrou MUITO mais lenta que a versão original em sf/S2 -- em teste com um
#' subconjunto pequeno (cd_uf=="12", 2215 setores), a função original completa (INCLUINDO
#' esta etapa) leva ~17s no total, enquanto só esta etapa em DuckDB não terminou em mais
#' de 7 minutos para o mesmo subconjunto. O overhead fixo por chamada (round-trip,
#' criação de tabela, conversão WKB) multiplicado por dezenas de grupos supera qualquer
#' ganho do índice R-tree para grupos deste tamanho. Ficaria viável só reescrita como
#' uma única query particionada por CR (`PARTITION BY cd_uorg` em vez de um loop R com
#' uma chamada DuckDB por grupo) -- não implementado aqui por não ser o objetivo
#' principal deste protótipo (a etapa de enclaves já era rápida/correta em S2).
#' Mesma lógica da função aninhada original em `calcular_imputacoes_norte()`
#' (`R/geoprocessamento.R`): agrupa por CR, encontra componentes conectados por
#' proximidade (`tolerancia_metros`), identifica qual componente contém a sede da CR
#' e reatribui qualquer componente "ilha" à CR do componente conectado mais próximo.
#' Only o predicado geométrico (`st_is_within_distance`) muda para DuckDB; o algoritmo
#' de componentes continua em igraph, como no original.
#' @param conn Conexão DuckDB já com a extensão spatial carregada.
#' @param df_setores Objeto sf já projetado num CRS métrico (ex.: `CRS_ALBERS_BR`).
#' @param sf_sedes Objeto sf com as sedes das CRs (coluna `cd_uorg`), mesmo CRS.
#' @param col_cr Nome da coluna de código da CR em `df_setores`.
.duckdb_limpar_enclaves_funai <- function(conn, df_setores, sf_sedes, col_cr = "cd_uorg", tolerancia_metros = 200) {
  message("Iniciando limpeza de enclaves (versão DuckDB)...")
  message("Calculando conectividade via DuckDB spatial...")

  lista_setores <- df_setores |> dplyr::group_split(!!rlang::sym(col_cr))

  df_processado <- lapply(lista_setores, function(sub_df) {
    n <- nrow(sub_df)
    if (n == 1) {
      sub_df$id_componente <- paste0(sub_df[[col_cr]][1], "_1")
      return(sub_df)
    }
    arestas <- .duckdb_within_distance_edges(conn, sub_df, dist_m = tolerancia_metros)
    g <- igraph::graph_from_data_frame(
      arestas, directed = FALSE,
      vertices = data.frame(name = seq_len(n))
    )
    sub_df$id_componente <- paste0(
      sub_df[[col_cr]][1], "_",
      igraph::components(g)$membership
    )
    sub_df
  }) |> dplyr::bind_rows()

  message("Identificando continente via sede...")

  continentes_validos <- c()

  for (i in seq_len(nrow(sf_sedes))) {
    cd_cr <- sf_sedes$cd_uorg[i]

    setores_cr <- df_processado |> dplyr::filter(.data[[col_cr]] == cd_cr)

    if (nrow(setores_cr) == 0) next
    idx_sede <- sf::st_nearest_feature(sf_sedes[i, ], setores_cr)
    continentes_validos <- c(
      continentes_validos,
      setores_cr$id_componente[idx_sede]
    )
  }

  df_processado$is_ilha <- !df_processado$id_componente %in% continentes_validos

  n_ilhas <- sum(df_processado$is_ilha)

  if (n_ilhas == 0) {
    message("Nenhuma ilha detectada.")
    return(df_processado |> dplyr::select(-id_componente, -is_ilha))
  }

  message(paste("Reatribuindo", n_ilhas, "ilhas..."))

  continentes <- df_processado |> dplyr::filter(!is_ilha)
  ilhas       <- df_processado |> dplyr::filter(is_ilha)

  idx <- sf::st_nearest_feature(ilhas, continentes)

  df_processado[[col_cr]][df_processado$is_ilha] <-
    continentes[[col_cr]][idx]

  df_processado |>
    dplyr::select(-id_componente, -is_ilha)
}

#' @title Limpa enclaves topológicos -- cópia verbatim da função aninhada original
#' (`limpar_enclaves_funai()` dentro de `calcular_imputacoes_norte()`,
#' `R/geoprocessamento.R`). Copiada (não importada/reexportada) porque a original é
#' uma função aninhada, inacessível fora do escopo de `calcular_imputacoes_norte()`,
#' e porque o plano deste protótipo é manter as duas implementações totalmente
#' independentes. Usa S2/WGS84 para `st_is_within_distance()`, confirmado mais rápido
#' que GEOS planar nesta base (ver nota em `R/geoprocessamento.R`).
limpar_enclaves_funai <- function(df_setores, sf_sedes, col_cr = "cd_uorg") {
  tolerancia_metros = 200
  library(sf)
  library(dplyr)
  library(igraph)

  df_setores_s2 <- sf::st_transform(df_setores, crs = 4326)
  sf_sedes_s2   <- sf::st_transform(sf_sedes, crs = 4326)

  message("Iniciando limpeza de enclaves (modo otimizado)...")
  message("Calculando conectividade com st_touches...")

  lista_setores <- df_setores_s2 |> dplyr::group_split(!!rlang::sym(col_cr))

  df_processado <- lapply(lista_setores, function(sub_df) {
    geom <- st_geometry(sub_df)
    v <- st_is_within_distance(st_geometry(sub_df), dist = tolerancia_metros)
    g <- igraph::graph_from_adj_list(v, mode = "all")

    sub_df$id_componente <- paste0(
      sub_df[[col_cr]][1], "_",
      igraph::components(g)$membership
    )
    sub_df
  }) |> bind_rows()

  message("Identificando continente via sede...")

  continentes_validos <- c()

  for(i in seq_len(nrow(sf_sedes))) {
    cd_cr <- sf_sedes$cd_uorg[i]

    setores_cr <- df_processado |> filter(.data[[col_cr]] == cd_cr)

    if(nrow(setores_cr) == 0) next
    idx_sede <- st_nearest_feature(sf_sedes[i, ], setores_cr)
    continentes_validos <- c(
      continentes_validos,
      setores_cr$id_componente[idx_sede]
    )
  }

  df_processado$is_ilha <- !df_processado$id_componente %in% continentes_validos

  n_ilhas <- sum(df_processado$is_ilha)

  if(n_ilhas == 0) {
    message("Nenhuma ilha detectada.")
    return(df_processado |> select(-id_componente, -is_ilha))
  }

  message(paste("Reatribuindo", n_ilhas, "ilhas..."))

  continentes <- df_processado |> filter(!is_ilha)
  ilhas       <- df_processado |> filter(is_ilha)

  idx <- st_nearest_feature(ilhas, continentes)

  df_processado[[col_cr]][df_processado$is_ilha] <-
    continentes[[col_cr]][idx]

  df_processado |>
    select(-id_componente, -is_ilha)
}

#' @title Calcula imputações multimodais (Rodoviário + Hidroviário) para a região
#' Norte -- versão híbrida DuckDB+spatial
#' @description Réplica de `calcular_imputacoes_norte()` (`R/geoprocessamento.R`),
#' mesma assinatura e mesmo contrato de retorno, para permitir comparação direta entre
#' as duas. A construção dos grafos e o roteamento
#' (`sfnetworks::st_network_cost()`), a lógica de decisão/penalização, o melt para
#' `df_combinacoes` e a escolha final de modal são copiados verbatim do original
#' (não fatorados num helper compartilhado, de propósito, para manter as duas funções
#' totalmente independentes). Só o snapping/distância por elemento e o clustering de
#' enclaves passam por DuckDB.
#' @inheritParams calcular_imputacoes_norte
calcular_imputacoes_norte_duckdb <- function(
    sf_setores,
    sf_crs,
    sf_rodovias,
    sf_hidrovias,
    sf_portos,
    vel_rod = 60.0,
    vel_hid = 20.0,
    vel_walk = 4.5,
    max_walk_km = 30.0,
    max_ratio = 3.0
) {

  crs_projeto <- CRS_ALBERS_BR

  conn <- .duckdb_conn_imputacao()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  cli::cli_alert_info("Preparando redes para processamento multimodal (DuckDB)...")

  sf_setores <- sf_setores |> sf::st_transform(crs_projeto)

  sf_setores_pt <- sf_setores |>
    sf::st_centroid() |>
    sf::st_zm(drop = TRUE) |>
    dplyr::mutate(.rowid = dplyr::row_number())

  sf_sedes <- sf_crs |>
    sf::st_zm(drop = TRUE) |>
    sf::st_transform(crs_projeto) |>
    dplyr::arrange(codigoUnidade) |>
    dplyr::mutate(.rowid = dplyr::row_number())

  sf_rodovias_proj <- sf_rodovias |> sf::st_transform(crs_projeto)
  rede_rod <- sfnetworks::as_sfnetwork(sf_rodovias_proj, directed = FALSE)

  sf_hidrovias_proj <- sf_hidrovias |> sf::st_transform(crs_projeto)
  rede_hid <- sfnetworks::as_sfnetwork(sf_hidrovias_proj, directed = FALSE)

  portos <- sf_portos |> sf::st_transform(crs_projeto) |> dplyr::mutate(.rowid = dplyr::row_number())

  #' ==============================================================================
  # 2. MATRIZES DE CUSTO
  #' ==============================================================================
  cli::cli_alert_info("Calculando matrizes de distância e snapping (DuckDB)...")

  edges_rod <- sf::st_as_sf(rede_rod, "edges") |> dplyr::mutate(.rowid = dplyr::row_number())

  # Setores -> edges (escala grande: ~66k linhas -- via DuckDB)
  nn_s_rod <- .duckdb_nearest_com_distancia(conn, sf_setores_pt, edges_rod, id_x = ".rowid")
  nn_s_rod <- nn_s_rod[order(nn_s_rod$id_x), ]
  idx_setores_edges <- nn_s_rod$id_y
  dist_s_rod <- nn_s_rod$dist / 1000

  # Sedes -> edges (só 42 linhas -- sf simples, nunca esteve implicado nos bugs)
  dist_cr_rod <- units::drop_units(sf::st_distance(sf_sedes, edges_rod[sf::st_nearest_feature(sf_sedes, edges_rod), ], by_element = TRUE)) / 1000

  # Setores -> portos (escala grande -- via DuckDB)
  nn_s_hid <- .duckdb_nearest_com_distancia(conn, sf_setores_pt, portos, id_x = ".rowid")
  nn_s_hid <- nn_s_hid[order(nn_s_hid$id_x), ]
  idx_setores_portos <- nn_s_hid$id_y
  dist_s_hid <- nn_s_hid$dist / 1000

  # Sedes -> portos (42 linhas -- sf simples)
  idx_sedes_portos <- sf::st_nearest_feature(sf_sedes, portos)
  dist_cr_hid <- units::drop_units(sf::st_distance(sf_sedes, portos[idx_sedes_portos, ], by_element = TRUE)) / 1000

  mat_rod_km  <- units::drop_units(sfnetworks::st_network_cost(rede_rod, from = sf_setores_pt, to = sf_sedes)) / 1000
  mat_hid_km  <- units::drop_units(sfnetworks::st_network_cost(rede_hid, from = portos[idx_setores_portos, ], to = portos[idx_sedes_portos, ])) / 1000
  mat_reta_km <- units::drop_units(sf::st_distance(sf_setores_pt, sf_sedes)) / 1000

  #' ==============================================================================
  # 3. LÓGICA DE DECISÃO (verbatim do original)
  #' ==============================================================================
  cli::cli_alert_info("Aplicando travas lógicas e otimizando modal...")

  T_ROD  <- sweep(mat_rod_km / vel_rod, 1, dist_s_rod / vel_walk, "+") |> sweep(2, dist_cr_rod / vel_walk, "+")
  T_HID  <- sweep(mat_hid_km / vel_hid, 1, dist_s_hid / vel_walk, "+") |> sweep(2, dist_cr_hid / vel_walk, "+")
  T_WALK <- mat_reta_km / vel_walk

  T_ROD[dist_s_rod > max_walk_km, ] <- Inf
  T_HID[dist_s_hid > max_walk_km, ] <- Inf

  D_ROD_TOTAL <- sweep(mat_rod_km, 1, dist_s_rod, "+") |> sweep(2, dist_cr_rod, "+")
  D_HID_TOTAL <- sweep(mat_hid_km, 1, dist_s_hid, "+") |> sweep(2, dist_cr_hid, "+")

  T_ROD[D_ROD_TOTAL > (mat_reta_km * max_ratio)] <- Inf
  T_HID[D_HID_TOTAL > (mat_reta_km * max_ratio)] <- Inf

  #' ==============================================================================
  # NOVO BLOCO: GERAR TODAS AS COMBINAÇÕES (SETOR x UORG) E ORDENAR (verbatim)
  #' ==============================================================================
  cli::cli_alert_info("Gerando dataframe com todas as combinações e ranqueamento...")

  T_MIN <- pmin(T_ROD, T_HID, T_WALK)

  MODAL_MAT <- matrix("Linha_Reta", nrow = nrow(T_MIN), ncol = ncol(T_MIN))
  MODAL_MAT[T_MIN == T_HID] <- "Hidrovia"
  MODAL_MAT[T_MIN == T_ROD] <- "Rodovia"

  dimnames(T_MIN) <- list(sf_setores_pt$cd_setor, sf_sedes$codigoUnidade)
  dimnames(MODAL_MAT) <- dimnames(T_MIN)

  df_long_tempo <- as.data.frame(as.table(T_MIN), stringsAsFactors = FALSE) |>
    dplyr::rename(cd_setor = Var1, cd_uorg = Var2, duracao_horas = Freq)

  df_long_modal <- as.data.frame(as.table(MODAL_MAT), stringsAsFactors = FALSE) |>
    dplyr::rename(cd_setor = Var1, cd_uorg = Var2, modal = Freq)

  df_combinacoes <- df_long_tempo |>
    dplyr::left_join(df_long_modal, by = c("cd_setor", "cd_uorg")) |>
    dplyr::mutate(
      cd_setor = as.numeric(cd_setor),
      cd_uorg = as.integer(cd_uorg),
      duracao = duracao_horas * 60
    ) |>
    dplyr::select(-duracao_horas) |>
    dplyr::group_by(cd_setor) |>
    dplyr::arrange(duracao, .by_group = TRUE) |>
    dplyr::mutate(ordem = dplyr::row_number()) |>
    dplyr::ungroup()
  #' ==============================================================================

  best_rod_idx  <- max.col(-T_ROD, ties.method = "first")
  best_hid_idx  <- max.col(-T_HID, ties.method = "first")
  best_walk_idx <- max.col(-T_WALK, ties.method = "first")

  df_tempos <- data.frame(
    Rodovia = T_ROD[cbind(1:nrow(T_ROD), best_rod_idx)],
    Hidrovia = T_HID[cbind(1:nrow(T_HID), best_hid_idx)],
    Linha_Reta = T_WALK[cbind(1:nrow(T_WALK), best_walk_idx)]
  )

  modal_venc_idx <- max.col(-df_tempos, ties.method = "first")

  sf_alocados <- sf_setores |>
    dplyr::mutate(
      modal = names(df_tempos)[modal_venc_idx],
      duracao_calc = df_tempos[cbind(1:nrow(df_tempos), modal_venc_idx)] * 60,
      cd_uorg = dplyr::case_when(
        modal == "Rodovia"    ~ sf_sedes$codigoUnidade[best_rod_idx],
        modal == "Hidrovia"   ~ sf_sedes$codigoUnidade[best_hid_idx],
        TRUE                  ~ sf_sedes$codigoUnidade[best_walk_idx]
      ),
      nome_uorg = dplyr::case_when(
        modal == "Rodovia"    ~ sf_sedes$nome[best_rod_idx],
        modal == "Hidrovia"   ~ sf_sedes$nome[best_hid_idx],
        TRUE                  ~ sf_sedes$nome[best_walk_idx]
      )
    )

  #' ==============================================================================
  # 4. LIMPEZA DE ENCLAVES -- usa a versão original em S2/sf (verbatim), não a versão
  # DuckDB (`.duckdb_limpar_enclaves_funai()`, definida acima neste arquivo): testada e
  # confirmada muito mais lenta nesta base (ver comentário na definição da função).
  # Esta etapa já era rápida/correta na versão original; não havia bug a corrigir aqui.
  #' ==============================================================================
  setores_ama_limpos <- limpar_enclaves_funai(
    sf_alocados, sf_crs |> dplyr::rename(cd_uorg = codigoUnidade)
  )

  #' ==============================================================================
  # 5. FORMATAÇÃO FINAL DO ARTEFATO (verbatim)
  #' ==============================================================================
  df_imputacao <- setores_ama_limpos |>
    sf::st_drop_geometry() |>
    dplyr::distinct(cd_setor, cd_uorg, .keep_all = TRUE) |>
    dplyr::transmute(
      cd_setor = as.numeric(cd_setor),
      codigoUnidadeImputado = as.integer(cd_uorg),
      duracao = duracao_calc,
      modal_imputado = modal,
      cd_uf = cd_uf
    )

  return(list(
    imputacao_principal = df_imputacao,
    combinacoes = df_combinacoes
  ))
}
