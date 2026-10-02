#' @title Requisita matriz de rotas à API OSRM
#' @description Envia um lote de coordenadas para a API do OSRM para calcular durações e distâncias,
#' implementando tratamento de erros e retentativas automáticas (circuit breaker) em caso de falha de
#' rede. Diferentemente de um loop infinito, possui um número máximo de tentativas com espera fixa
#' entre elas, e valida se a matriz retornada não veio inteiramente vazia (NA) antes de declarar
#' sucesso, evitando que uma indisponibilidade persistente da API trave o pipeline indefinidamente.
#' @param lote_destinos Data.frame com as coordenadas de destino do lote atual.
#' @param dados_origens Data.frame com as coordenadas de origem.
#' @param max_tentativas Inteiro. Número máximo de tentativas antes de abortar com erro. Padrão: 5.
#' @param espera_segundos Numérico. Tempo de espera (segundos) entre tentativas. Padrão: 10.
#' @return Uma lista contendo o lote de destinos original e o resultado bruto da API (tabela OSRM).
requisitar_api_osrm <- function(lote_destinos, dados_origens, max_tentativas = OSRM_MAX_TENTATIVAS, espera_segundos = OSRM_ESPERA_SEGUNDOS) {
  library(sf)
  library(osrm)
  library(tictoc)

  src_sf <- st_as_sf(dados_origens, coords = c("lon", "lat"), crs = 4326)
  dst_sf <- st_as_sf(lote_destinos, coords = c("lon", "lat"), crs = 4326)

  dir.create(DIR_LOGS, recursive = TRUE, showWarnings = FALSE)
  arq_log <- file.path(DIR_LOGS, "log_api_osrm.txt")

  sucesso <- FALSE
  tabela <- NULL
  tentativa <- 1

  while (!sucesso && tentativa <= max_tentativas) {
    tabela <- tryCatch({
      tic(msg = 'Requisição OSRM')
      res <- osrmTable(src = src_sf, dst = dst_sf, measure = c("duration", "distance"))
      msg <- toc()
      cat(msg$callback_msg, '\n', file = arq_log, append = TRUE)

      # Considera a resposta como falha se a matriz inteira veio vazia (NA), o que indica
      # uma falha silenciosa da API (diferente de NAs pontuais de rotas inalcançáveis,
      # que são tratados normalmente a jusante em resgatar_resultados_completo()).
      if (is.null(res$durations) || (all(is.na(res$durations)) && all(is.na(res$distances)))) {
        stop("Matriz retornada pela API OSRM veio inteiramente vazia (NA).")
      }

      sucesso <- TRUE
      res
    }, error = function(e) {
      msg <- sprintf(
        "Falha na API OSRM (Tentativa %d/%d). Erro: %s",
        tentativa, max_tentativas, e$message
      )
      cat(msg, '\n', file = arq_log, append = TRUE)
      if (tentativa < max_tentativas) {
        message(sprintf("%s Aguardando %d segundos para tentar de novo...", msg, espera_segundos))
        Sys.sleep(espera_segundos)
      }
      NULL
    })

    tentativa <- tentativa + 1
  }

  if (!sucesso) {
    rlang::abort(sprintf(
      "requisitar_api_osrm: Falha ao obter resposta válida da API OSRM após %d tentativas.",
      max_tentativas
    ))
  }

  Sys.sleep(1) # Respiro para a API

  # Retornamos uma lista empacotando o lote original e o resultado da API
  return(list(
    lote_destinos = lote_destinos,
    tabela = tabela
  ))
}

#' @title Corrige inconsistências topológicas (Enclaves Administrativos) via Grafos
#' @description Utiliza um grafo estático pré-calculado para identificar componentes desconexos 
#' (ilhas administrativas) e as reatribui para a UORG vizinha com a maior fronteira. 
#' Avalia enclaves como blocos unidos (st_union) para resolver setores internos isolados.
#' 
#' @param arq_entrada Caminho do GeoPackage com os setores censitários a serem avaliados.
#' @param sf_limite Caminho/Objeto do limite territorial (País/UF) para descartar ilhas geográficas.
#' @param lista_adjacencia_global Objeto sgbp contendo o retorno de st_touches(sf_setores_filtrados).
#' @param caminho_saida String. Caminho completo para salvar o GeoPackage corrigido.
#' @param coluna_alvo String. Nome da coluna que define os territórios (padrão: "cd_uorg").
#' @param arq_resultados_osrm Caminho para o cache OSRM (tabela_osrm_consolidada) para resgate.
#' @return String com o caminho do arquivo corrigido.
corrigindo_topologia <- function(
    arq_entrada, 
    sf_limite,
    lista_adjacencia_global,
    caminho_saida, 
    coluna_alvo = "cd_uorg",
    arq_resultados_osrm
) {
  
  # arq_entrada = targets::tar_read(arquivo_gpkg_imputado_final)
  # sf_limite = targets::tar_read(arq_limite_fisico)
  # lista_adjacencia_global = targets::tar_read(matriz_topologia_setores)
  # caminho_saida = "dados/processados/setores_cr_suavizado.gpkg"
  # coluna_alvo =  "cd_uorg"
  # arq_resultados_osrm = targets::tar_read(tabela_osrm_consolidada)
  
  tictoc::tic(msg = "Correção Topológica Completa")
  library(sf)
  library(dplyr)
  library(igraph)
  
  # IMPORTANDO DADOS
  df_setores <- st_read(arq_entrada, quiet = TRUE)

  # Garante CRS métrico/projetado antes de qualquer operação de buffer/área.
  # Mais abaixo, o buffer da borda das ilhas (st_buffer) usa dist = 10, valor em METROS,
  # que só é interpretado corretamente com um CRS projetado (ou geográfico com engine s2).
  # Guardamos o CRS original para devolver o arquivo de saída na mesma projeção de entrada.
  crs_original_setores <- sf::st_crs(df_setores)
  if (isTRUE(sf::st_is_longlat(df_setores)) || is.na(crs_original_setores)) {
    df_setores <- sf::st_transform(df_setores, CRS_ALBERS_BR)
  }

  # Preserva o índice original para mapear perfeitamente com a lista de adjacência
  df_setores$idx_original <- seq_len(nrow(df_setores))

  # Rótulo canônico (nome_uorg/sigla) por código de CR, capturado ANTES de qualquer reatribuição
  # (usa o mais frequente em df_setores). A reatribuição de ilhas abaixo só decide o cd_uorg
  # vencedor -- sem sincronizar nome_uorg/sigla com ele, o setor realocado ficaria com o cd_uorg da
  # CR nova mas o nome/sigla da CR antiga, produzindo linhas fantasma no relatório final (mesmo
  # cd_uorg aparecendo com dois nomes de CR diferentes).
  lookup_labels <- df_setores |>
    sf::st_drop_geometry() |>
    dplyr::count(.data[[coluna_alvo]], nome_uorg, sigla, sort = TRUE) |>
    dplyr::distinct(.data[[coluna_alvo]], .keep_all = TRUE) |>
    dplyr::select(-n)

  # PASSO 1: IDENTIFICAR ILHAS FÍSICAS (NÃO SOFREM CORREÇÃO) ----
  message("1. Identificando limites físicos e ilhas geográficas...")
  sf_limite <- st_make_valid(sf_limite)
  if (st_crs(df_setores) != st_crs(sf_limite)) {
    sf_limite <- st_transform(sf_limite, st_crs(df_setores))
  }
  
  sf_limite_poly <- suppressWarnings(st_cast(sf_limite, "POLYGON"))
  sf_limite_poly$area_km2 <- as.numeric(st_area(sf_limite_poly))
  continente_fisico <- sf_limite_poly |> filter(area_km2 == max(area_km2))
  
  pontos_setores <- suppressWarnings(st_point_on_surface(st_geometry(df_setores)))
  idx_no_continente <- lengths(st_intersects(pontos_setores, continente_fisico)) > 0
  
  # PASSO 2: GRAFO GLOBAL E QUEBRA DE ARESTAS (ALTA PERFORMANCE) ----
  message("2. Podando arestas administrativas no Grafo Global...")
  
  g_global <- graph_from_adj_list(lista_adjacencia_global, mode = "all")
  arestas <- as_edgelist(g_global, names = FALSE)
  
  # VETORIZAÇÃO: Identifica arestas que ligam UORGs diferentes
  uorgs <- df_setores[[coluna_alvo]]
  uorg_origem <- uorgs[arestas[, 1]]
  uorg_destino <- uorgs[arestas[, 2]]
  
  idx_arestas_fronteira <- which(uorg_origem != uorg_destino)
  g_adm <- delete_edges(g_global, idx_arestas_fronteira)
  
  # PASSO 3: DEFINIR BLOCOS PRINCIPAIS E ENCLAVES ----
  message("3. Calculando áreas e detectando enclaves...")
  
  df_setores$id_componente <- paste0(df_setores[[coluna_alvo]], "_", components(g_adm)$membership)
  df_setores$area_poly <- as.numeric(st_area(df_setores))
  
  resumo_comp <- df_setores |>
    st_drop_geometry() |>
    group_by(.data[[coluna_alvo]], id_componente) |>
    summarise(area_total = sum(area_poly), .groups = "drop") |>
    group_by(.data[[coluna_alvo]]) |>
    mutate(is_main = area_total == max(area_total)) |>
    ungroup()
  
  continentes_validos <- resumo_comp |> filter(is_main) |> pull(id_componente)
  
  # Preserva id_componente para podermos iterar por ilha, e não por setor
  df_setores <- df_setores |> 
    mutate(is_ilha = (!id_componente %in% continentes_validos) & idx_no_continente) |> 
    select(-area_poly)
  
  n_ilhas_setores <- sum(df_setores$is_ilha)
  
  if (n_ilhas_setores == 0) {
    message("✅ Nenhum enclave administrativo detectado. Topologia limpa!")
  } else {
    
    # PASSO 4: REATRIBUIÇÃO POR COMPONENTE (ILHA INTEIRA DE UMA VEZ) ----
    ilhas_adm <- df_setores |> filter(is_ilha)
    blocos_principais <- df_setores |> filter(!is_ilha & idx_no_continente)
    
    componentes_ilha <- unique(ilhas_adm$id_componente)
    
    message(sprintf("4. Corrigindo %d setores agrupados em %d ilhas distintas...", n_ilhas_setores, length(componentes_ilha)))
    
    for (comp in componentes_ilha) {
      message('Corrigindo componente ', comp)
      tictoc::tic(msg = paste0('Correção concluída'))
      setores_ilha <- ilhas_adm[ilhas_adm$id_componente == comp, ]
      
      # 4.1 Funde a ilha num único polígono (Resolve o problema do setor envolto)
      geom_ilha_unida <- suppressWarnings(sf::st_union(sf::st_geometry(setores_ilha)))
      borda_ilha <- sf::st_boundary(geom_ilha_unida)
      # dist = 10 está em METROS (df_setores garantidamente em CRS projetado neste ponto)
      borda_ilha_buffer <- sf::st_buffer(borda_ilha, dist = 10)
      
      # 4.2 Encontra vizinhos externos da ilha consultando a lista global
      todos_vizinhos_idx <- unlist(lista_adjacencia_global[setores_ilha$idx_original])
      vizinhos_externos_idx <- setdiff(todos_vizinhos_idx, setores_ilha$idx_original)
      
      vizinhos_candidatos <- df_setores[vizinhos_externos_idx, ] |> 
        filter(!is_ilha) # Garante que vai se ligar a um continente, não a outra ilha órfã
      
      # 4.3 Se a ilha inteira não tocar em nenhum continente válido (ex: ilha dentro de ilha)
      if (nrow(vizinhos_candidatos) == 0) {
        idx_near <- sf::st_nearest_feature(geom_ilha_unida, sf::st_geometry(blocos_principais))
        atribuicao <- blocos_principais[[coluna_alvo]][idx_near]
      } else {
        
        # 4.4 Cruzamento do tubo da borda com os continentes vizinhos
        fronteiras <- suppressWarnings(sf::st_intersection(vizinhos_candidatos, borda_ilha_buffer))
        
        if (nrow(fronteiras) == 0) {
          idx_near <- sf::st_nearest_feature(geom_ilha_unida, sf::st_geometry(blocos_principais))
          atribuicao <- blocos_principais[[coluna_alvo]][idx_near]
        } else {
          pesos_fronteira <- as.numeric(sf::st_area(sf::st_geometry(fronteiras)))
          soma_fronteiras <- tapply(pesos_fronteira, fronteiras[[coluna_alvo]], sum)      
          atribuicao <- names(soma_fronteiras)[which.max(soma_fronteiras)]
        }
      }
      
      # 4.5 Aplica a nova UORG a TODOS os setores que formam esta ilha de uma só vez,
      # sincronizando nome_uorg/sigla com o rótulo canônico do cd_uorg vencedor
      linhas_comp <- df_setores$id_componente == comp
      rotulo <- lookup_labels[lookup_labels[[coluna_alvo]] == atribuicao, ]
      df_setores[[coluna_alvo]][linhas_comp] <- atribuicao
      if (nrow(rotulo) > 0) {
        df_setores$nome_uorg[linhas_comp] <- rotulo$nome_uorg[1]
        df_setores$sigla[linhas_comp] <- rotulo$sigla[1]
      }
      df_setores$criterio[linhas_comp] <- "Imputado (Correção Topológica)"
      tictoc::toc()
      gc()
    }
    
    # PASSO 5: RESGATE E ATUALIZAÇÃO DO OSRM ----
    message("5. Atualizando métricas OSRM das rotas corrigidas...")
    
    ilhas_atualizadas <- df_setores |> filter(is_ilha) |> st_drop_geometry()
    
    resultados_osrm <- arrow::open_dataset(arq_resultados_osrm) |> 
      filter(cd_setor %in% ilhas_atualizadas$cd_setor) |> 
      collect() |>
      distinct(cd_setor, cd_uorg, .keep_all = TRUE) |>
      inner_join(ilhas_atualizadas |> select(cd_setor, cd_uorg), by = c("cd_setor", "cd_uorg")) |>
      distinct(cd_setor, .keep_all = T) |>
      select(cd_setor, nome_uorg_imputado = nome_uorg, sigla_imputado = sigla, 
             duracao_imputado = duracao, distancia_imputado = distancia, 
             distancia_reta_imputado = distancia_reta, valor_criterio_imputado = valor_criterio, 
             ordem_imputado = ordem, perda_imputado = perda, rota_valida_imputado = rota_valida)
    
    # nome_uorg_imputado/sigla_imputado (acima) vêm do cache OSRM e podem faltar quando nunca
    # houve rota calculada do setor até o cd_uorg vencedor; nesse caso caem para o rótulo
    # canônico (lookup_labels) em vez de deixar nome_uorg/sigla como NA para o setor corrigido.
    df_setores <- df_setores |>
      left_join(resultados_osrm, by = "cd_setor") |>
      left_join(
        lookup_labels |> dplyr::rename(nome_uorg_fallback = nome_uorg, sigla_fallback = sigla),
        by = coluna_alvo
      ) |>
      mutate(
        duracao = ifelse(is_ilha, duracao_imputado, duracao),
        distancia = ifelse(is_ilha, distancia_imputado, distancia),
        distancia_reta = ifelse(is_ilha, distancia_reta_imputado, distancia_reta),
        valor_criterio = ifelse(is_ilha, valor_criterio_imputado, valor_criterio),
        ordem = ifelse(is_ilha, ordem_imputado, ordem),
        rota_valida = ifelse(is_ilha, rota_valida_imputado, rota_valida),
        nome_uorg = ifelse(is_ilha, dplyr::coalesce(nome_uorg_imputado, nome_uorg_fallback), nome_uorg),
        sigla = ifelse(is_ilha, dplyr::coalesce(sigla_imputado, sigla_fallback), sigla),
        perda = ifelse(is_ilha, perda_imputado, perda)
      ) |>
      select(-ends_with('_imputado'), -nome_uorg_fallback, -sigla_fallback)
  }
  
  # Limpeza Final das colunas de controle usadas no grafo e na iteração
  colunas_remover <- c("idx_original", "is_ilha", "id_componente")
  df_setores <- df_setores |> select(-any_of(colunas_remover))

  # Devolve o arquivo de saída no CRS original de entrada (o CRS projetado acima foi
  # usado apenas internamente para garantir buffers/áreas corretos em metros)
  if (!is.na(crs_original_setores) && sf::st_crs(df_setores) != crs_original_setores) {
    df_setores <- sf::st_transform(df_setores, crs_original_setores)
  }

  if (!dir.exists(dirname(caminho_saida))) dir.create(dirname(caminho_saida), recursive = TRUE)
  st_write(df_setores, caminho_saida, delete_dsn = TRUE, quiet = TRUE)

  tictoc::toc()
  return(caminho_saida)
}

#' @title Corrige enclaves administrativos a nível de município (V3)
#' @description Aplica a mesma lógica de detecção de enclaves de corrigindo_topologia(), mas usando
#' os municípios (não os setores censitários) como unidade do grafo de adjacência. Isso evita que a
#' correção topológica de setor (que não tem noção de município) devolva parte dos setores de um
#' município já imputado para uma CR vizinha, quebrando a exclusividade municipal decidida pela
#' Diretoria Colegiada em 28/07/2026 (ver _targets.R, bloco "VERSÃO 3"). Quando um município fica
#' geometricamente isolado do bloco principal de sua CR, ele é realocado inteiro (nunca
#' parcialmente) para a CR vizinha com maior fronteira compartilhada.
#' @param arq_entrada Caminho do gpkg de setores censitários já com a CR de município aplicada
#' (ex.: arquivo_gpkg_imputado_final_mun), com uma única CR por cd_mun.
#' @param sf_municipios Objeto sf com os polígonos municipais do IBGE (cd_mun, geometria).
#' @param sf_limite Objeto sf com o limite físico do país/UF, usado para não "corrigir" municípios
#' que são ilhas geográficas reais (não apenas administrativas).
#' @param coluna_alvo Nome da coluna com o código da CR. Padrão: "cd_uorg".
#' @return Caminho do gpkg de setores corrigido.
corrigir_enclaves_municipais <- function(arq_entrada, sf_municipios, sf_limite, coluna_alvo = "cd_uorg") {

  # arq_entrada = targets::tar_read(arquivo_gpkg_imputado_final_mun)
  # sf_municipios = targets::tar_read(arq_municipios)
  # sf_limite = targets::tar_read(arq_limite_fisico)

  library(sf)
  library(dplyr)
  library(igraph)

  tictoc::tic(msg = "Correção de Enclaves Municipais Completa")

  df_setores <- st_read(arq_entrada, quiet = TRUE)
  df_setores$cd_mun <- as.character(df_setores$cd_mun)
  crs_original <- st_crs(df_setores)

  # CR vencedora por município -- pré-condição: já deve ser única (gerar_df_imputacao_mun() +
  # imputar_atendimento() garantem isso antes desta função rodar).
  cd_uorg_por_mun <- df_setores |> st_drop_geometry() |> distinct(cd_mun, .data[[coluna_alvo]])
  stopifnot(!any(duplicated(cd_uorg_por_mun$cd_mun)))

  sf_mun_cr <- sf_municipios |>
    mutate(cd_mun = as.character(cd_mun)) |>
    inner_join(cd_uorg_por_mun, by = "cd_mun") |>
    st_make_valid()

  if (isTRUE(st_is_longlat(sf_mun_cr)) || is.na(st_crs(sf_mun_cr))) {
    sf_mun_cr <- st_transform(sf_mun_cr, CRS_ALBERS_BR)
  }

  # PASSO 1: IDENTIFICAR MUNICÍPIOS EM ILHAS FÍSICAS (NÃO SOFREM CORREÇÃO) ----
  sf_limite <- st_make_valid(sf_limite)
  if (st_crs(sf_mun_cr) != st_crs(sf_limite)) sf_limite <- st_transform(sf_limite, st_crs(sf_mun_cr))
  sf_limite_poly <- suppressWarnings(st_cast(sf_limite, "POLYGON"))
  sf_limite_poly$area_km2 <- as.numeric(st_area(sf_limite_poly))
  continente_fisico <- sf_limite_poly |> filter(area_km2 == max(area_km2))

  pontos_mun <- suppressWarnings(st_point_on_surface(st_geometry(sf_mun_cr)))
  idx_no_continente <- lengths(st_intersects(pontos_mun, continente_fisico)) > 0

  # PASSO 2: GRAFO DE ADJACÊNCIA ENTRE MUNICÍPIOS (não entre setores) ----
  adjacencia_mun <- st_touches(sf_mun_cr)
  g_global <- graph_from_adj_list(adjacencia_mun, mode = "all")
  arestas <- as_edgelist(g_global, names = FALSE)

  uorgs <- sf_mun_cr[[coluna_alvo]]
  idx_arestas_fronteira <- which(uorgs[arestas[, 1]] != uorgs[arestas[, 2]])
  g_adm <- delete_edges(g_global, idx_arestas_fronteira)

  # PASSO 3: BLOCOS PRINCIPAIS E ENCLAVES, POR CR ----
  sf_mun_cr$id_componente <- paste0(sf_mun_cr[[coluna_alvo]], "_", components(g_adm)$membership)
  sf_mun_cr$area_poly <- as.numeric(st_area(sf_mun_cr))

  resumo_comp <- sf_mun_cr |>
    st_drop_geometry() |>
    group_by(.data[[coluna_alvo]], id_componente) |>
    summarise(area_total = sum(area_poly), .groups = "drop") |>
    group_by(.data[[coluna_alvo]]) |>
    mutate(is_main = area_total == max(area_total)) |>
    ungroup()

  componentes_validos <- resumo_comp |> filter(is_main) |> pull(id_componente)
  sf_mun_cr <- sf_mun_cr |> mutate(is_ilha = (!id_componente %in% componentes_validos) & idx_no_continente)

  n_ilhas_mun <- sum(sf_mun_cr$is_ilha)

  if (!dir.exists(DIR_PROCESSADOS)) dir.create(DIR_PROCESSADOS, recursive = TRUE)
  caminho_saida <- file.path(DIR_PROCESSADOS, "setores_cr_suavizado_mun.gpkg")

  if (n_ilhas_mun == 0) {
    message("Nenhum enclave municipal detectado. Município preservado como unidade mínima.")
    st_write(df_setores, caminho_saida, delete_dsn = TRUE, quiet = TRUE)
    tictoc::toc()
    return(caminho_saida)
  }

  message(sprintf("Corrigindo %d município(s) em situação de enclave administrativo...", n_ilhas_mun))

  # PASSO 4: REATRIBUIÇÃO POR COMPONENTE (MUNICÍPIO INTEIRO DE UMA VEZ) ----
  ilhas_mun <- sf_mun_cr |> filter(is_ilha)
  blocos_principais <- sf_mun_cr |> filter(!is_ilha & idx_no_continente)
  componentes_ilha <- unique(ilhas_mun$id_componente)

  tb_correcoes <- vector("list", length(componentes_ilha))

  for (i in seq_along(componentes_ilha)) {
    comp <- componentes_ilha[i]
    setor_mun <- ilhas_mun |> filter(id_componente == comp)

    geom_unida <- suppressWarnings(st_union(st_geometry(setor_mun)))
    borda <- st_buffer(st_boundary(geom_unida), dist = 10)

    idx_vizinhos <- which(lengths(st_intersects(blocos_principais, borda)) > 0)
    vizinhos <- blocos_principais[idx_vizinhos, ]

    if (nrow(vizinhos) == 0) {
      idx_near <- st_nearest_feature(geom_unida, st_geometry(blocos_principais))
      atribuicao <- blocos_principais[[coluna_alvo]][idx_near]
    } else {
      fronteiras <- suppressWarnings(st_intersection(vizinhos, borda))
      if (nrow(fronteiras) == 0) {
        idx_near <- st_nearest_feature(geom_unida, st_geometry(blocos_principais))
        atribuicao <- blocos_principais[[coluna_alvo]][idx_near]
      } else {
        # fronteiras vem de st_intersection(vizinhos, borda) com borda = buffer poligonal (10m) do
        # contorno da ilha -- o resultado é sempre POLYGON (faixa fina ao longo da fronteira), nunca
        # LINESTRING. st_length() em geometria poligonal retorna 0 sempre (GEOS não calcula
        # perímetro), o que zerava soma_fronteiras para todo vizinho e fazia which.max() escolher o
        # "vencedor" pela ordem de tapply() (cd_uorg mais baixo), não pela maior fronteira
        # compartilhada como pretendido. st_area() dá o peso correto: a área da faixa é proporcional
        # ao comprimento da fronteira, já que a largura do buffer é fixa (mesma técnica usada em
        # corrigindo_topologia(), acima, para o equivalente a nível de setor).
        pesos_fronteira <- as.numeric(st_area(st_geometry(fronteiras)))
        soma_fronteiras <- tapply(pesos_fronteira, fronteiras[[coluna_alvo]], sum)
        atribuicao <- names(soma_fronteiras)[which.max(soma_fronteiras)]
      }
    }

    tb_correcoes[[i]] <- data.frame(cd_mun = setor_mun$cd_mun, cd_uorg_corrigido = atribuicao)
  }

  tb_correcoes <- bind_rows(tb_correcoes)
  message("Municípios corrigidos: ", paste(tb_correcoes$cd_mun, "->", tb_correcoes$cd_uorg_corrigido, collapse = "; "))

  # Rótulo canônico (nome_uorg/sigla) por código de CR, usando o mais frequente em df_setores.
  # Necessário porque a reatribuição abaixo só decide o cd_uorg vencedor (maior fronteira/vizinho
  # mais próximo) -- sem isso, o setor realocado ficaria com o cd_uorg da CR nova mas o
  # nome_uorg/sigla da CR antiga, produzindo linhas fantasma no relatório final (mesmo cd_uorg
  # aparecendo com dois nomes de CR diferentes).
  lookup_labels <- df_setores |>
    st_drop_geometry() |>
    dplyr::count(.data[[coluna_alvo]], nome_uorg, sigla, sort = TRUE) |>
    dplyr::distinct(.data[[coluna_alvo]], .keep_all = TRUE) |>
    dplyr::select(-n)

  tb_correcoes <- tb_correcoes |>
    dplyr::left_join(lookup_labels, by = stats::setNames(coluna_alvo, "cd_uorg_corrigido"))

  # Aplica a correção a TODOS os setores dos municípios afetados, de uma só vez (nunca parcial)
  idx <- match(df_setores$cd_mun, tb_correcoes$cd_mun)
  linhas_corrigir <- !is.na(idx)
  df_setores[[coluna_alvo]][linhas_corrigir] <- tb_correcoes$cd_uorg_corrigido[idx[linhas_corrigir]]
  df_setores$nome_uorg[linhas_corrigir] <- tb_correcoes$nome_uorg[idx[linhas_corrigir]]
  df_setores$sigla[linhas_corrigir] <- tb_correcoes$sigla[idx[linhas_corrigir]]
  df_setores$criterio[linhas_corrigir] <- "Imputado (Correção Topológica Municipal)"

  if (!is.na(crs_original) && st_crs(df_setores) != crs_original) {
    df_setores <- st_transform(df_setores, crs_original)
  }

  st_write(df_setores, caminho_saida, delete_dsn = TRUE, quiet = TRUE)
  tictoc::toc()

  return(caminho_saida)
}

#' @title Reforça a exclusividade municipal como rede de segurança final (V3)
#' @description Depois da correção topológica de enclaves (a nível de município e de setor),
#' verifica se ainda restam municípios com mais de uma CR (caso residual, ex.: município fisicamente
#' descontínuo em partes que não são vizinhas entre si) e, se houver, força todos os seus setores
#' para a CR com maior área somada naquele município. Não deve ter efeito na maioria das execuções --
#' existe como garantia estrutural, não como mecanismo primário de correção (ver
#' corrigir_enclaves_municipais()).
#' @param arq_entrada Caminho do gpkg de setores censitários pós correção topológica de setor.
#' @param coluna_alvo Nome da coluna com o código da CR. Padrão: "cd_uorg".
#' @return Caminho do gpkg de setores com exclusividade municipal garantida.
reforcar_exclusividade_municipal <- function(arq_entrada, coluna_alvo = "cd_uorg") {

  library(sf)
  library(dplyr)

  df_setores <- st_read(arq_entrada, quiet = TRUE)
  df_setores$cd_mun <- as.character(df_setores$cd_mun)

  municipios_violados <- df_setores |>
    st_drop_geometry() |>
    group_by(cd_mun) |>
    filter(n_distinct(.data[[coluna_alvo]]) > 1) |>
    ungroup()

  # Curto-circuito antes do group_by/summarise/max() abaixo: numa tibble vazia (caso normal,
  # já que corrigir_enclaves_municipais() garante exclusividade por construção), max(area_total)
  # dispararia um warning inofensivo mas ruidoso ("nenhum argumento não faltante para max").
  vencedor_por_mun <- if (nrow(municipios_violados) == 0) {
    municipios_violados |> distinct(cd_mun) |> mutate(cd_uorg_reforcado = character())
  } else {
    municipios_violados |>
      group_by(cd_mun, .data[[coluna_alvo]]) |>
      summarise(area_total = sum(area_km2, na.rm = TRUE), .groups = "drop_last") |>
      filter(area_total == max(area_total)) |>
      ungroup() |>
      distinct(cd_mun, .keep_all = TRUE) |>
      select(cd_mun, cd_uorg_reforcado = all_of(coluna_alvo))
  }

  if (nrow(vencedor_por_mun) == 0) {
    message("Nenhum município residual com mais de uma CR. Nada a reforçar.")
  } else {
    message(sprintf(
      "Reforçando exclusividade em %d município(s) residual(is): %s",
      nrow(vencedor_por_mun), paste(vencedor_por_mun$cd_mun, collapse = ", ")
    ))

    # Mesmo cuidado de corrigir_enclaves_municipais(): sincroniza nome_uorg/sigla com o cd_uorg
    # vencedor, usando o rótulo mais frequente por código em df_setores, para não deixar um cd_uorg
    # correto com o nome/sigla da CR antiga.
    lookup_labels <- df_setores |>
      st_drop_geometry() |>
      dplyr::count(.data[[coluna_alvo]], nome_uorg, sigla, sort = TRUE) |>
      dplyr::distinct(.data[[coluna_alvo]], .keep_all = TRUE) |>
      dplyr::select(-n)

    vencedor_por_mun <- vencedor_por_mun |>
      dplyr::left_join(lookup_labels, by = stats::setNames(coluna_alvo, "cd_uorg_reforcado"))

    idx <- match(df_setores$cd_mun, vencedor_por_mun$cd_mun)
    linhas_corrigir <- !is.na(idx)
    df_setores[[coluna_alvo]][linhas_corrigir] <- vencedor_por_mun$cd_uorg_reforcado[idx[linhas_corrigir]]
    df_setores$nome_uorg[linhas_corrigir] <- vencedor_por_mun$nome_uorg[idx[linhas_corrigir]]
    df_setores$sigla[linhas_corrigir] <- vencedor_por_mun$sigla[idx[linhas_corrigir]]
    df_setores$criterio[linhas_corrigir] <- "Imputado (Reforço de Exclusividade Municipal)"
  }

  if (!dir.exists(DIR_PROCESSADOS)) dir.create(DIR_PROCESSADOS, recursive = TRUE)
  caminho_saida <- file.path(DIR_PROCESSADOS, "setores_cr_suavizado_mun_final.gpkg")
  st_write(df_setores, caminho_saida, delete_dsn = TRUE, quiet = TRUE)

  return(caminho_saida)
}

#' @title Verifica exclusividade municipal (um único CR por município)
#' @description Versão executável do teste de QA que ficava comentado em imputar_atendimento()
#' (R/define_divisoes_cr.R) -- interrompe a execução com erro claro se algum município acabar
#' vinculado a mais de uma Coordenação Regional, em vez de deixar a inconsistência passar em
#' silêncio. Deve ser chamado como o último alvo da cadeia de correção da V3.
#' @param sf_dados Objeto sf ou caminho de gpkg com os setores censitários finais (colunas cd_mun e
#' coluna_alvo).
#' @param coluna_alvo Nome da coluna com o código da CR. Padrão: "cd_uorg".
#' @return TRUE (invisível) se a exclusividade for respeitada; interrompe com stop() caso contrário.
verificar_exclusividade_municipal <- function(sf_dados, coluna_alvo = "cd_uorg") {

  if (is.character(sf_dados)) sf_dados <- sf::st_read(sf_dados, quiet = TRUE)

  violacoes <- sf_dados |>
    sf::st_drop_geometry() |>
    dplyr::group_by(cd_mun) |>
    dplyr::summarise(n_cr = dplyr::n_distinct(.data[[coluna_alvo]]), .groups = "drop") |>
    dplyr::filter(n_cr > 1)

  if (nrow(violacoes) > 0) {
    stop(sprintf(
      "Exclusividade municipal violada em %d município(s): %s",
      nrow(violacoes), paste(violacoes$cd_mun, collapse = ", ")
    ))
  }

  message("Exclusividade municipal verificada: cada município possui exatamente uma CR.")
  invisible(TRUE)
}

#' @title Compara total_pi por CR: agregação por setor (antiga) vs. método híbrido SIDRA (novo)
#' @description Alvo de QA não-bloqueante (nunca interrompe o pipeline) para acompanhar a troca de
#' `total_pi` da agregação geométrica por setor censitário para a importação direta SIDRA (TI +
#' Município "fora de TI") em `calcular_populacao_indigena_cr_sidra()`. Duas fontes de diferença são
#' esperadas e não indicam erro: (1) o gap de TIs sem código oficial do IBGE (ver
#' `total_pi_ti_nao_oficial_estimada`, não somado ao novo total); (2) mudança metodológica real --
#' `malha_basica_bruta` sempre atribui uma TI inteira a uma única CR, enquanto o método por setor
#' podia fragmentar a população de uma TI entre CRs vizinhas caso seus setores tivessem sido
#' imputados a CRs diferentes.
#' @param arq_final_sc_mun Caminho do gpkg com `total_pi` calculado por setor (alvo
#'   `arquivo_final_sc_mun`, antes da troca).
#' @param arq_malha_basica_bruta,tb_pop_ti_sidra,tb_pop_mun_fora_ti_sidra,df_tis_ibge,
#'   df_sc_agregado,df_mapeamento_setores Mesmos parâmetros de
#'   `calcular_populacao_indigena_cr_sidra()`.
#' @param limiar_pct Diferença percentual acima da qual uma CR é sinalizada (padrão: 0.03 = 3%).
#' @return tibble com `cd_uorg`, `total_pi_setor_antigo`, `total_pi_sidra_novo`, `diferenca_abs`,
#'   `diferenca_pct`, `sinalizado`.
verificar_populacao_indigena_cr_sidra <- function(
    arq_final_sc_mun,
    arq_malha_basica_bruta,
    tb_pop_ti_sidra,
    tb_pop_mun_fora_ti_sidra,
    df_tis_ibge,
    df_sc_agregado,
    df_mapeamento_setores,
    limiar_pct = 0.03
) {

  total_pi_antigo <- sf::st_read(arq_final_sc_mun, quiet = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(cd_uorg = as.character(cd_uorg)) |>
    dplyr::select(cd_uorg, total_pi_setor_antigo = total_pi)

  total_pi_novo <- calcular_populacao_indigena_cr_sidra(
    arq_malha_basica_bruta = arq_malha_basica_bruta,
    tb_pop_ti_sidra = tb_pop_ti_sidra,
    tb_pop_mun_fora_ti_sidra = tb_pop_mun_fora_ti_sidra,
    df_tis_ibge = df_tis_ibge,
    df_sc_agregado = df_sc_agregado,
    df_mapeamento_setores = df_mapeamento_setores
  ) |>
    dplyr::select(cd_uorg, total_pi_sidra_novo = total_pi, total_pi_ti_nao_oficial_estimada)

  comparacao <- total_pi_antigo |>
    dplyr::full_join(total_pi_novo, by = "cd_uorg") |>
    dplyr::mutate(
      diferenca_abs = total_pi_sidra_novo - total_pi_setor_antigo,
      diferenca_pct = diferenca_abs / dplyr::if_else(total_pi_setor_antigo == 0, NA_real_, total_pi_setor_antigo),
      sinalizado = !is.na(diferenca_pct) & abs(diferenca_pct) > limiar_pct
    ) |>
    dplyr::arrange(dplyr::desc(abs(diferenca_pct)))

  n_sinalizados <- sum(comparacao$sinalizado, na.rm = TRUE)
  if (n_sinalizados > 0) {
    message(sprintf(
      "verificar_populacao_indigena_cr_sidra(): %d CR(s) com diferença acima de %.0f%% entre o total_pi antigo (setor) e o novo (SIDRA). Ver tar_read(verificacao_populacao_sidra).",
      n_sinalizados, limiar_pct * 100
    ))
  }

  comparacao
}


#' @title Calcula imputações multimodais (Rodoviário + Hidroviário) para a região Norte
#' @description Processa matrizes de distância massivas, define o modal ótimo 
#' e limpa enclaves topológicos com grafos avançados (borda unificada).
#' @param sf_setores Objeto sf com os setores censitários do Norte (coluna cd_setor).
#' @param sf_crs Objeto sf com as sedes das CRs (coluna cd_uorg).
#' @param sf_rodovias Objeto sf com a malha rodoviária.
#' @param sf_hidrovias Objeto sf com a malha hidroviária.
#' @param sf_portos Objeto sf com os pontos portuários.
#' @param vel_rod,vel_hid,vel_walk Velocidades (km/h) para cada modal.
#' @param max_walk_km Limite máximo de caminhada até o ponto de acesso.
#' @param max_ratio Fator de penalização máxima em relação à linha reta.
#' @return Lista contendo: 'imputacao_principal' (Data.frame limpo) e 'combinacoes' (Data.frame com todas as rotas).

#' @title Calcula st_distance(x, y, by_element = TRUE) em lotes
#' @description Uma chamada única de `sf::st_distance(..., by_element = TRUE)` com dezenas
#' de milhares de pares ponto-vs-linha provoca uma falha de segmentação no GEOS (confirmado
#' com GEOS 3.14.1 / sf 1.1.0 nesta base: quebra de forma reprodutível em torno de 6500-6550
#' pares em uma única chamada, mesmo com todas as geometrias individualmente válidas). Dividir
#' em lotes menores evita o problema sem alterar o resultado.
#' @param x,y Objetos sf de mesmo comprimento (pareados 1:1).
#' @param chunk_size Tamanho do lote (bem abaixo do limiar observado de ~6500).
st_distance_by_element_chunked <- function(x, y, chunk_size = 2000) {
  n <- nrow(x)
  idx_lotes <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  do.call(c, lapply(idx_lotes, function(idx) {
    sf::st_distance(x[idx, ], y[idx, ], by_element = TRUE)
  }))
}

calcular_imputacoes_norte <- function(
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
  
  #' =========================================================================
  # DEBUG START
  # sf_setores   <- targets::tar_read(sf_setores_filtrados) |> dplyr::filter(cd_uf %in% c(12, 13, 16, 21, 51, 15, 11, 14, 17)) |>
  #   dplyr::filter(cd_mun == 1304237)
  # sf_crs       <- targets::tar_read(sf_pontos_filtrados) #|> dplyr::filter(!(codigoUnidade %in% c(509690, 509751)))
  # sf_rodovias  <- targets::tar_read(mapa_vias) |> dplyr::filter(fclass %in% c('motorway', 'trunk', 'primary', 'secondary'))
  # # sf_hidrovias <- targets::tar_read(hidrovias_antaq) |> dplyr::filter(cla_icacao == "Navegável")
  # sf_hidrovias <- targets::tar_read(mapa_hidrovias) |> dplyr::filter(fclass %in% c('river'))
  # sf_portos    <- targets::tar_read(portos_antaq)
  #
  # vel_rod = 60.0
  # vel_hid = 20.0
  # vel_walk = 4.5
  # max_walk_km = 30.0
  # max_ratio = 3.0
  #' =========================================================================
  
  crs_projeto <- CRS_ALBERS_BR
  
  cli::cli_alert_info("Preparando redes para processamento multimodal...")
  
  # 1. Padronização e Extração de Geometrias
  sf_setores <- sf_setores |> sf::st_transform(crs_projeto)
  
  sf_setores_pt <- sf_setores |> 
    sf::st_centroid() |> 
    sf::st_zm(drop = TRUE)
  
  sf_sedes <- sf_crs |>
    sf::st_zm(drop = TRUE) |>
    sf::st_transform(crs_projeto) |>
    dplyr::arrange(codigoUnidade)
  
  # Materializa cada etapa em uma atribuição separada (em vez de uma única cadeia
  # de pipe encadeada a partir do argumento lazy da função) -- evitou de forma
  # reprodutível uma falha de segmentação nesta etapa nesta máquina.
  sf_rodovias_proj <- sf_rodovias |> sf::st_transform(crs_projeto)
  rede_rod <- sfnetworks::as_sfnetwork(sf_rodovias_proj, directed = FALSE)

  sf_hidrovias_proj <- sf_hidrovias |> sf::st_transform(crs_projeto)
  rede_hid <- sfnetworks::as_sfnetwork(sf_hidrovias_proj, directed = FALSE)

  portos <- sf_portos |> sf::st_transform(crs_projeto)
  
  #' ==============================================================================
  # 2. MATRIZES DE CUSTO
  #' ==============================================================================
  cli::cli_alert_info("Calculando matrizes de distância e snappping...")

  edges_rod <- sf::st_as_sf(rede_rod, "edges")

  dist_s_rod <- units::drop_units(st_distance_by_element_chunked(sf_setores_pt, edges_rod[sf::st_nearest_feature(sf_setores_pt, edges_rod),])) / 1000
  dist_cr_rod <- units::drop_units(sf::st_distance(sf_sedes, edges_rod[sf::st_nearest_feature(sf_sedes, edges_rod),], by_element = TRUE)) / 1000

  # Calculado uma única vez e reaproveitado abaixo (evita recalcular o mesmo st_nearest_feature)
  idx_setores_portos <- sf::st_nearest_feature(sf_setores_pt, portos)
  idx_sedes_portos <- sf::st_nearest_feature(sf_sedes, portos)

  dist_s_hid <- units::drop_units(st_distance_by_element_chunked(sf_setores_pt, portos[idx_setores_portos,])) / 1000
  dist_cr_hid <- units::drop_units(sf::st_distance(sf_sedes, portos[idx_sedes_portos,], by_element = TRUE)) / 1000

  mat_rod_km  <- units::drop_units(sfnetworks::st_network_cost(rede_rod, from = sf_setores_pt, to = sf_sedes)) / 1000
  mat_hid_km  <- units::drop_units(sfnetworks::st_network_cost(rede_hid, from = portos[idx_setores_portos,], to = portos[idx_sedes_portos,])) / 1000
  mat_reta_km <- units::drop_units(sf::st_distance(sf_setores_pt, sf_sedes)) / 1000
  
  #' ==============================================================================
  # 3. LÓGICA DE DECISÃO
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
  # NOVO BLOCO: GERAR TODAS AS COMBINAÇÕES (SETOR x UORG) E ORDENAR
  #' ==============================================================================
  cli::cli_alert_info("Gerando dataframe com todas as combinações e ranqueamento...")
  
  # Calcula o tempo mínimo entre os modais para CADA par
  T_MIN <- pmin(T_ROD, T_HID, T_WALK)
  
  # Identifica qual modal garantiu o tempo mínimo para a matriz de combinações
  MODAL_MAT <- matrix("Linha_Reta", nrow = nrow(T_MIN), ncol = ncol(T_MIN))
  MODAL_MAT[T_MIN == T_HID] <- "Hidrovia"
  MODAL_MAT[T_MIN == T_ROD] <- "Rodovia"
  
  # Associa as dimensões para não perder os IDs no derretimento (melt)
  dimnames(T_MIN) <- list(sf_setores_pt$cd_setor, sf_sedes$codigoUnidade)
  dimnames(MODAL_MAT) <- dimnames(T_MIN)
  
  # Transforma as matrizes em dataframes longos de forma otimizada com as.table
  df_long_tempo <- as.data.frame(as.table(T_MIN), stringsAsFactors = FALSE) |>
    dplyr::rename(cd_setor = Var1, cd_uorg = Var2, duracao_horas = Freq)
  
  df_long_modal <- as.data.frame(as.table(MODAL_MAT), stringsAsFactors = FALSE) |>
    dplyr::rename(cd_setor = Var1, cd_uorg = Var2, modal = Freq)
  
  # Junta dados, formata tipos, converte para minutos e cria a coluna 'ordem'
  df_combinacoes <- df_long_tempo |>
    dplyr::left_join(df_long_modal, by = c("cd_setor", "cd_uorg")) |>
    dplyr::mutate(
      cd_setor = as.numeric(cd_setor),
      cd_uorg = as.integer(cd_uorg),
      duracao = duracao_horas * 60 # Em minutos
    ) |>
    dplyr::select(-duracao_horas) |>
    dplyr::group_by(cd_setor) |>
    dplyr::arrange(duracao, .by_group = TRUE) |>
    dplyr::mutate(ordem = dplyr::row_number()) |>
    dplyr::ungroup()
  #' ==============================================================================
  
  # (Continuação da lógica original de alocação da primeira opção)
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
      duracao_calc = df_tempos[cbind(1:nrow(df_tempos), modal_venc_idx)] * 60, # Em minutos
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
  
  limpar_enclaves_funai <- function(df_setores, sf_sedes, col_cr = "cd_uorg") {
    tolerancia_metros = 200
    library(sf)
    library(dplyr)
    library(igraph)
    
    # PROCESSANDO EM WGS 84 PRA UTILIZAR MOTOR S2 -- confirmado empiricamente mais rápido
    # que GEOS planar para st_is_within_distance() em polígonos nesta escala (~17s para o
    # maior grupo em S2/4326 vs. vários minutos sem terminar em GEOS/Albers no mesmo grupo).
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
  
  setores_ama_limpos <- limpar_enclaves_funai(sf_alocados, sf_crs |> dplyr::rename(cd_uorg = codigoUnidade))
  
  #' ==============================================================================
  # 5. FORMATAÇÃO FINAL DO ARTEFATO
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
  
  # Retorna a lista contendo ambos os dataframes
  return(list(
    imputacao_principal = df_imputacao,
    combinacoes = df_combinacoes
  ))
}

#' @title Calcula a malha básica (jurisdição por Terra Indígena e por Município, sem ajustes manuais)
#' @description Reproduz as regras de negócio de determinação de jurisdição usadas no painel_funai
#' (`processa_malha()`, projeto `painel_funai/R/import_functions.R`), sem a etapa de ajustes manuais
#' (overrides), que continua residindo naquele projeto -- este alvo produz a base "pré-ajustes", que o
#' painel_funai passa a consumir como insumo em vez de recalcular a interseção espacial e o voto por
#' município ele mesmo.
#'
#' Para cada Terra Indígena, atribui a CR com maior percentual de sobreposição de área; em caso de
#' correspondência com a unidade gestora declarada no SII (`undadm_codigo`, traduzido para `cd_uorg`
#' via `cd_sii` -- ver `adicionar_cd_sii()`) ou de CR nova (sem correspondência prévia no SII), a
#' justificativa reflete isso em vez de "maior área de sobreposição". Para cada município, atribui a
#' CR com maior soma de área de setor censitário na malha de área de atuação (com desempate
#' determinístico por `cd_uorg`), com fallback manual para municípios instalados depois da malha de
#' área de atuação ter sido construída (sem setores censitários correspondentes).
#' @param arq_poligonos_cr Caminho do gpkg de polígonos finais por CR (alvo `arquivo_final_sc_mun`).
#' @param arq_area_atuacao Caminho do gpkg de setores censitários com `cd_mun`/`cd_uorg`/`area_km2`
#'   (alvo `arquivo_gpkg_suavizado_mun_final`).
#' @param arq_tis Objeto sf com as Terras Indígenas (alvo `arq_tis`).
#' @param arq_municipios Objeto sf com os polígonos municipais do IBGE (alvo `arq_municipios`).
#' @param arq_uorg_ug Data.frame de correspondência `cd_uorg`/`ug`/`ug_ds` (alvo `arq_uorg_ug`).
#' @param dicionario_cr Dicionário de CRs usado para derivar `cd_sii` (alvo `dicionario_cr`, ver
#'   `adicionar_cd_sii()` em `R/gera_graficos_relatorio.R`).
#' @param regras_imputacao Tabela de regras (alvo `regras_imputacao`); usa as regras da etapa
#'   "malha_basica" como ajustes manuais sobre a malha calculada.
#' @param dir_saida Diretório de saída (padrão: `DIR_PROCESSADOS`).
#' @param crs_area CRS métrico para cálculo de área (padrão: `CRS_ALBERS_BR`).
#' @return Caminho do gpkg gerado (`malha_basica_bruta.gpkg`).
calcular_malha_basica_bruta <- function(
    arq_poligonos_cr,
    arq_area_atuacao,
    arq_tis,
    arq_municipios,
    arq_uorg_ug,
    dicionario_cr,
    regras_imputacao,
    dir_saida = DIR_PROCESSADOS,
    crs_area = CRS_ALBERS_BR
) {

  # CRs criadas na reestruturação, sem correspondência prévia no SII (ver mesma constante em
  # gera_graficos_relatorio.R / painel_funai/R/import_functions.R).
  CRS_NOVAS <- c("509690", "509751", "509512")

  tb_area <- sf::st_read(arq_poligonos_cr, quiet = TRUE) |>
    sf::st_transform(crs_area) |>
    sf::st_make_valid() |>
    dplyr::mutate(cd_uorg = as.character(cd_uorg))

  lookup_cr <- tb_area |> sf::st_drop_geometry() |> dplyr::distinct(cd_uorg, nome_uorg, sigla)

  # --- Terras Indígenas ---------------------------------------------------

  arq_uorg_ug_sii <- adicionar_cd_sii(arq_uorg_ug, dicionario_cr)

  tis <- arq_tis |>
    sf::st_transform(crs_area) |>
    sf::st_make_valid()

  tis <- tis |>
    dplyr::mutate(
      undadm_codigo = as.character(undadm_codigo),
      area_total_ti_ha = as.numeric(sf::st_area(tis)) / 10000
    ) |>
    dplyr::left_join(
      arq_uorg_ug_sii |> dplyr::transmute(cd_uorg_sii = as.character(cd_uorg), cd_sii),
      by = dplyr::join_by(undadm_codigo == cd_sii)
    )

  ints_ti <- sf::st_intersection(
    tb_area |> dplyr::select(cd_uorg, nome_uorg, sigla),
    tis |> dplyr::select(terrai_codigo, terrai_nome, area_total_ti_ha, undadm_codigo, cd_uorg_sii)
  )

  ints_ti <- ints_ti |>
    dplyr::mutate(
      area_intersecao_ha = as.numeric(sf::st_area(ints_ti)) / 10000,
      pct_intersecao = area_intersecao_ha / area_total_ti_ha
    )

  # Vencedor por TI (maior área de sobreposição com a malha de roteamento): calculado sobre a
  # interseção (ints_ti), que dá o percentual de área correto para decidir quem fica com a TI --
  # mas a geometria usada em ti_jurisdicao, abaixo, é sempre o polígono ORIGINAL e completo da TI
  # (tabela `tis`), nunca o pedaço recortado pela interseção. Manter só o pedaço recortado (bug
  # corrigido nesta versão) deixava parte da TI sem cobertura na malha básica sempre que ela se
  # espalhava por mais de uma zona de roteamento da malha V2+MSOL -- a sobra reaparecia "fatiada"
  # pelos municípios que a TI deveria sobrepor por inteiro, já que a regra de negócio da DIRCOL é
  # que a TI (mais específica) prevalece inteira sobre o(s) município(s) que ela atravessa. Caso
  # real observado: TI Vale do Javari, que atravessa múltiplas zonas de roteamento.
  vencedor_ti <- ints_ti |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      justificativa = dplyr::case_when(
        cd_uorg %in% CRS_NOVAS ~ "Malha básica: CR nova, sem correspondência SII prévia",
        cd_uorg == cd_uorg_sii ~ "Malha básica: correspondência com unidade gestora do SII",
        TRUE ~ "Malha básica: maior área de sobreposição com a Terra Indígena"
      )
    ) |>
    dplyr::group_by(terrai_codigo) |>
    dplyr::slice_max(pct_intersecao, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(terrai_codigo, cd_uorg, nome_uorg, sigla, justificativa)

  ti_jurisdicao <- tis |>
    dplyr::select(terrai_codigo, terrai_nome) |>
    dplyr::inner_join(vencedor_ti, by = "terrai_codigo") |>
    dplyr::transmute(
      cd_territorio = as.character(terrai_codigo),
      nm_territorio = terrai_nome,
      tipo_territorio = "Terra Indígena",
      cd_uorg, nome_uorg, sigla, justificativa
    )

  # --- Municípios -----------------------------------------------------------

  tb_mun <- arq_municipios |>
    sf::st_transform(crs_area) |>
    sf::st_make_valid() |>
    dplyr::mutate(cd_mun = as.character(cd_mun))

  tb_setores <- sf::st_read(
    arq_area_atuacao,
    quiet = TRUE,
    query = paste0(
      "SELECT cd_mun, cd_uorg, area_km2 FROM \"", sf::st_layers(arq_area_atuacao)$name[1], "\""
    )
  ) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(cd_mun = as.character(cd_mun), cd_uorg = as.character(cd_uorg)) |>
    dplyr::filter(!is.na(cd_uorg), cd_uorg != "")

  # Vencedor por município: CR com maior soma de área de setor censitário (um município pode ter
  # setores em mais de uma CR na área de atuação). Desempate determinístico por cd_uorg quando há
  # empate exato de área.
  mun_voto <- tb_setores |>
    dplyr::group_by(cd_mun, cd_uorg) |>
    dplyr::summarise(area_total = sum(area_km2, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(cd_mun, dplyr::desc(area_total), cd_uorg) |>
    dplyr::distinct(cd_mun, .keep_all = TRUE)

  # Ajustes manuais da malha básica: etapa "malha_basica" de dados/imputacoes/regras_imputacao.csv.
  # Município sem setores censitários na malha de área de atuação (ex.: instalado depois dela, como
  # Boa Esperança do Norte/MT) não tem voto e só entra na malha por uma dessas regras; os demais são
  # reatribuídos por coalesce sobre o resultado consolidado (ver "Ajustes manuais pontuais" abaixo).
  overrides_manuais <- regras_imputacao |>
    dplyr::filter(etapa == "malha_basica") |>
    dplyr::transmute(
      cd_territorio = codigo,
      tipo_territorio = dplyr::if_else(tipo == "ti", "Terra Indígena", "Município"),
      cd_uorg_override = cd_uorg_destino,
      justificativa_override = justificativa
    )
  municipios_manuais <- overrides_manuais |>
    dplyr::filter(tipo_territorio == "Município") |>
    dplyr::transmute(cd_mun = cd_territorio, cd_uorg = cd_uorg_override, motivo = justificativa_override)

  mun_votados <- tb_mun |>
    dplyr::inner_join(mun_voto, by = "cd_mun") |>
    dplyr::left_join(lookup_cr, by = "cd_uorg") |>
    dplyr::mutate(justificativa = "Malha básica: jurisdição predominante por área entre os setores censitários do município")

  mun_sem_voto <- tb_mun |>
    dplyr::filter(!cd_mun %in% mun_voto$cd_mun) |>
    dplyr::inner_join(municipios_manuais, by = "cd_mun") |>
    dplyr::left_join(lookup_cr, by = "cd_uorg") |>
    dplyr::rename(justificativa = motivo)

  mun_jurisdicao <- dplyr::bind_rows(
    mun_votados |> dplyr::select(cd_mun, nm_mun, cd_uorg, nome_uorg, sigla, justificativa),
    mun_sem_voto |> dplyr::select(cd_mun, nm_mun, cd_uorg, nome_uorg, sigla, justificativa)
  ) |>
    dplyr::transmute(
      cd_territorio = cd_mun,
      nm_territorio = nm_mun,
      tipo_territorio = "Município",
      cd_uorg, nome_uorg, sigla, justificativa
    )

  # --- Consolidação -----------------------------------------------------------

  # tb_mun (arq_municipios) usa "geometry" como nome da coluna de geometria, enquanto tb_area/tis
  # (lidos via sf::st_read de gpkg deste pipeline) usam "geom" -- alinha antes do bind_rows() para
  # não acabar com duas colunas de geometria (uma delas toda NA) no resultado.
  sf_col_ti <- attr(ti_jurisdicao, "sf_column")
  sf_col_mun <- attr(mun_jurisdicao, "sf_column")
  if (!identical(sf_col_mun, sf_col_ti)) {
    names(mun_jurisdicao)[names(mun_jurisdicao) == sf_col_mun] <- sf_col_ti
    sf::st_geometry(mun_jurisdicao) <- sf_col_ti
  }

  # --- Máscara TI -> Município ---------------------------------------------------------------
  # Réplica, restrita aos dois níveis usados neste projeto (TI e Município), da cascata de
  # `ST_Difference` de `processar_dissolucao_malha_por_cr()` em painel_funai/R/procs.R: a camada
  # mais específica (TI) nunca é tocada; a menos específica (Município) tem subtraída a área já
  # coberta pela TI antes do dissolve final por CR (dissolver_malha_basica_por_cr()). Sem isso, um
  # município que contém uma TI atribuída a outra CR ficava com a área da TI contada duas vezes
  # (uma na linha da TI, outra dentro do polígono cheio do município) -- inflando area_ha/
  # total_area_km2 por CR e duplicando contagens de pontos (ex.: focos de queimada) em qualquer
  # st_join contra a malha dissolvida.
  epsilon_fechamento_m <- 90 # fecha frestas por desalinhamento de simplificação entre TI e município

  mascara_ti <- ti_jurisdicao |>
    sf::st_union() |>
    sf::st_make_valid() |>
    sf::st_buffer(epsilon_fechamento_m)

  toca_ti <- lengths(sf::st_intersects(mun_jurisdicao, mascara_ti)) > 0

  mun_com_ti <- mun_jurisdicao[toca_ti, ]
  mun_sem_ti <- mun_jurisdicao[!toca_ti, ]

  if (nrow(mun_com_ti) > 0) {
    mun_com_ti <- sf::st_difference(mun_com_ti, mascara_ti) |>
      sf::st_make_valid()
    mun_com_ti <- mun_com_ti[!sf::st_is_empty(mun_com_ti), ]
  }

  mun_jurisdicao <- dplyr::bind_rows(mun_sem_ti, mun_com_ti)

  malha_base <- dplyr::bind_rows(ti_jurisdicao, mun_jurisdicao)

  # --- Ajustes manuais pontuais ------------------------------------------------
  # Reatribuição de território já presente na malha básica, por coalesce sobre o resultado já
  # consolidado (TI + Município) -- regras em `overrides_manuais`, definidas acima.

  malha_base <- malha_base |>
    dplyr::mutate(cd_territorio = as.character(cd_territorio)) |>
    dplyr::left_join(overrides_manuais, by = c("cd_territorio", "tipo_territorio")) |>
    dplyr::left_join(
      lookup_cr |> dplyr::rename(cd_uorg_override = cd_uorg, nome_uorg_override = nome_uorg, sigla_override = sigla),
      by = "cd_uorg_override"
    ) |>
    dplyr::mutate(
      cd_uorg = dplyr::coalesce(cd_uorg_override, cd_uorg),
      nome_uorg = dplyr::coalesce(nome_uorg_override, nome_uorg),
      sigla = dplyr::coalesce(sigla_override, sigla),
      justificativa = dplyr::coalesce(justificativa_override, justificativa)
    ) |>
    dplyr::select(-cd_uorg_override, -nome_uorg_override, -sigla_override, -justificativa_override)

  if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)
  caminho_saida <- file.path(dir_saida, "malha_basica_bruta.gpkg")
  sf::st_write(malha_base, caminho_saida, delete_dsn = TRUE, quiet = TRUE)

  return(caminho_saida)
}

#' @title Calcula a jurisdição ATUAL (declarada/oficial) por TI e por Município
#' @description Réplica de `calcular_malha_basica_bruta()`, mas com a CR de cada território
#' vindo da vinculação já institucionalizada, não do algoritmo de roteamento: Terra Indígena
#' pela vinculação registrada no SII (`adicionar_cd_sii()`); Município pela CR que o declarou
#' atender no processo SEI n° 08620.010286/2021-89 e no site institucional da Funai, já
#' desempatado pelo maior número de documento SEI quando há mais de uma declaração para o mesmo
#' município (`obter_municipios_declarados_cr()`, alvo `municipios_declarados_cr`). Território
#' sem nenhuma vinculação (TI sem correspondência no SII, ou município sem nenhuma CR
#' declarante) entra com `cd_uorg` sentinela `"0"` ("Sem vinculação declarada") em vez de ficar
#' de fora -- ao contrário da proposta algorítmica, a jurisdição atual tem lacunas reais, e o
#' mapa deve mostrá-las, não escondê-las.
#' @details Mesma regra de sobreposição de `calcular_malha_basica_bruta()`: a camada mais
#' específica (TI, vinculada ou não) nunca é tocada; Município tem subtraída a área já coberta
#' por alguma TI, para não contar a mesma área duas vezes no dissolve por CR
#' (`dissolver_malha_basica_por_cr()`, reaproveitada sem alteração para este resultado).
#' @param arq_tis Objeto sf das Terras Indígenas.
#' @param arq_uorg_ug Data.frame de correspondência SII/UORG (alvo `arq_uorg_ug`).
#' @param dicionario_cr Dicionário de CRs, usado por `adicionar_cd_sii()` (alvo `dicionario_cr`).
#' @param arq_municipios Objeto sf dos municípios brasileiros (alvo `arq_municipios`).
#' @param df_declarados Município-CR declarados, já desempatado por maior SEI (alvo
#'   `municipios_declarados_cr`).
#' @param arq_siorg Data.frame do SIORG (alvo `arq_siorg`), usado para nome/sigla das CRs a
#'   partir do `cd_uorg` da vinculação SII (as TIs só trazem o código, não o nome/sigla da CR).
#' @param dir_saida Diretório de saída (padrão: `DIR_PROCESSADOS`).
#' @param crs_area CRS métrico para cálculo de área (padrão: `CRS_ALBERS_BR`).
#' @return Caminho do gpkg gerado (`jurisdicao_atual_bruta.gpkg`).
calcular_jurisdicao_atual_bruta <- function(
    arq_tis,
    arq_uorg_ug,
    dicionario_cr,
    arq_municipios,
    df_declarados,
    arq_siorg,
    dir_saida = DIR_PROCESSADOS,
    crs_area = CRS_ALBERS_BR
) {

  cd_uorg_sem_vinculacao <- "0"
  nome_sem_vinculacao <- "Sem vinculação declarada"

  arq_uorg_ug_sii <- adicionar_cd_sii(arq_uorg_ug, dicionario_cr)

  # CRs (não UTLs): mesmo filtro codigoUnidadePai == 173 já usado para sf_pontos_filtrados em
  # _targets.R. codigoUnidade 509592 é uma unidade duplicada/descontinuada no SIORG (mesmo
  # filtro de exclusão já aplicado ali).
  lookup_cr <- arq_siorg |>
    dplyr::filter(codigoUnidadePai == 173, codigoUnidade != 509592) |>
    dplyr::transmute(cd_uorg = as.character(codigoUnidade), nome_uorg = nome, sigla)

  # --- Terras Indígenas: vinculação atual no SII ---------------------------------------
  tis <- arq_tis |>
    sf::st_transform(crs_area) |>
    sf::st_make_valid() |>
    dplyr::mutate(undadm_codigo = as.character(undadm_codigo)) |>
    dplyr::left_join(
      arq_uorg_ug_sii |> dplyr::transmute(cd_uorg_sii = as.character(cd_uorg), cd_sii),
      by = dplyr::join_by(undadm_codigo == cd_sii)
    ) |>
    dplyr::left_join(lookup_cr, by = c("cd_uorg_sii" = "cd_uorg"))

  ti_jurisdicao <- tis |>
    dplyr::transmute(
      cd_territorio = as.character(terrai_codigo),
      nm_territorio = terrai_nome,
      tipo_territorio = "Terra Indígena",
      cd_uorg = dplyr::coalesce(cd_uorg_sii, cd_uorg_sem_vinculacao),
      nome_uorg = dplyr::coalesce(nome_uorg, nome_sem_vinculacao),
      sigla = dplyr::coalesce(sigla, nome_sem_vinculacao),
      justificativa = dplyr::if_else(
        is.na(cd_uorg_sii),
        "Jurisdição atual: TI sem vinculação registrada no SII",
        "Jurisdição atual: vinculação registrada no SII"
      )
    )

  # --- Municípios: declaração de atendimento (já desempatada por maior número SEI) -----
  tb_mun <- arq_municipios |>
    sf::st_transform(crs_area) |>
    sf::st_make_valid() |>
    dplyr::mutate(cd_mun = as.character(cd_mun))

  # cd_uorg vem de df_declarados (codigoUnidade), mas nome_uorg/sigla vêm sempre do lookup_cr
  # canônico (SIORG vigente) -- nunca do nome_cr/sigla já embutidos em df_declarados (derivados
  # do texto de cr_municipio.xlsx, que pode ficar desatualizado frente ao SIORG). Usar duas
  # fontes de nome para o mesmo cd_uorg é exatamente a armadilha de "cd_uorg reatribuído sem
  # sincronizar nome_uorg/sigla" documentada em CLAUDE.md -- gera linhas fantasma (o mesmo
  # cd_uorg aparecendo duas vezes no dissolve por CR, uma para cada nome).
  declarados <- df_declarados |>
    dplyr::transmute(cd_mun = as.character(cd_mun), cd_uorg = as.character(codigoUnidade))

  mun_jurisdicao <- tb_mun |>
    dplyr::left_join(declarados, by = "cd_mun") |>
    dplyr::mutate(cd_uorg = dplyr::coalesce(cd_uorg, cd_uorg_sem_vinculacao)) |>
    dplyr::left_join(lookup_cr, by = "cd_uorg") |>
    dplyr::mutate(
      nome_uorg = dplyr::coalesce(nome_uorg, nome_sem_vinculacao),
      sigla = dplyr::coalesce(sigla, nome_sem_vinculacao),
      justificativa = dplyr::if_else(
        cd_uorg == cd_uorg_sem_vinculacao,
        "Jurisdição atual: nenhuma CR declarou atender este município (processo SEI/site institucional)",
        "Jurisdição atual: município declarado por CR (processo SEI/site institucional)"
      )
    ) |>
    dplyr::transmute(
      cd_territorio = cd_mun, nm_territorio = nm_mun, tipo_territorio = "Município",
      cd_uorg, nome_uorg, sigla, justificativa
    )

  # --- Máscara TI -> Município (mesma técnica de calcular_malha_basica_bruta(), inclusive para
  # TI sem vinculação: a regra de negócio é TI sobrepõe Município independentemente de a TI ter
  # ou não CR declarada -- não é a definição de CR que dita a prioridade topológica). -----------
  sf_col_ti <- attr(ti_jurisdicao, "sf_column")
  sf_col_mun <- attr(mun_jurisdicao, "sf_column")
  if (!identical(sf_col_mun, sf_col_ti)) {
    names(mun_jurisdicao)[names(mun_jurisdicao) == sf_col_mun] <- sf_col_ti
    sf::st_geometry(mun_jurisdicao) <- sf_col_ti
  }

  epsilon_fechamento_m <- 90
  mascara_ti <- ti_jurisdicao |>
    sf::st_union() |>
    sf::st_make_valid() |>
    sf::st_buffer(epsilon_fechamento_m)

  toca_ti <- lengths(sf::st_intersects(mun_jurisdicao, mascara_ti)) > 0
  mun_com_ti <- mun_jurisdicao[toca_ti, ]
  mun_sem_ti <- mun_jurisdicao[!toca_ti, ]

  if (nrow(mun_com_ti) > 0) {
    mun_com_ti <- sf::st_difference(mun_com_ti, mascara_ti) |>
      sf::st_make_valid()
    mun_com_ti <- mun_com_ti[!sf::st_is_empty(mun_com_ti), ]
  }

  mun_jurisdicao <- dplyr::bind_rows(mun_sem_ti, mun_com_ti)

  jurisdicao_atual <- dplyr::bind_rows(ti_jurisdicao, mun_jurisdicao)

  if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)
  caminho_saida <- file.path(dir_saida, "jurisdicao_atual_bruta.gpkg")
  sf::st_write(jurisdicao_atual, caminho_saida, delete_dsn = TRUE, quiet = TRUE)

  return(caminho_saida)
}

#' @title Dissolve a malha básica por CR (um polígono por cd_uorg)
#' @description Camada de referência "situação atual" (pré-reestruturação), no mesmo formato das
#' camadas de proposta (`arquivo_final_sc`, `arquivo_final_sc_v2`, ...): uma única feição por CR,
#' unindo os territórios (municípios/TIs) que `calcular_malha_basica_bruta()` atribuiu a cada
#' `cd_uorg`. Corresponde à camada "Malha Básica" do projeto QGIS de Áreas de Atuação, antes mantida
#' como um shapefile estático fora deste pipeline.
#' @param arq_malha_basica_bruta Caminho do gpkg gerado por `calcular_malha_basica_bruta()` (ou
#'   qualquer gpkg no mesmo formato: colunas `cd_uorg`/`nome_uorg`/`sigla`, ex.:
#'   `calcular_jurisdicao_atual_bruta()`).
#' @param dir_saida Diretório de saída (padrão: `DIR_PROCESSADOS`).
#' @param crs_area CRS métrico para cálculo de área (padrão: `CRS_ALBERS_BR`).
#' @param nome_arquivo Nome do gpkg de saída (padrão: `"malha_basica_dissolvida.gpkg"`).
#' @return Caminho do gpkg gerado.
dissolver_malha_basica_por_cr <- function(arq_malha_basica_bruta, dir_saida = DIR_PROCESSADOS, crs_area = CRS_ALBERS_BR, nome_arquivo = "malha_basica_dissolvida.gpkg") {

  # Mesmo epsilon de calcular_malha_basica_bruta(); fecha frestas residuais entre territórios
  # vizinhos de uma mesma CR (bordas que não se alinham perfeitamente após o recorte por
  # st_difference() lá aplicado), sem alterar visivelmente o contorno externo -- mesma técnica de
  # painel_funai/R/procs.R (buffer positivo seguido do negativo, aplicado após o dissolve).
  epsilon_fechamento_m <- 90

  malha_bruta <- sf::st_read(arq_malha_basica_bruta, quiet = TRUE) |>
    sf::st_transform(crs_area) |>
    sf::st_make_valid()

  # --- Consolidação por hierarquia administrativa (Município -> UF) -----------------------------
  # Otimização de processamento, réplica -- na etapa em que este projeto de fato dissolve por CR --
  # da técnica de consolida_malha() + processar_dissolucao_malha_por_cr() em painel_funai/R/procs.R:
  # quando TODOS os municípios de uma UF já estão atribuídos à mesma CR (checagem feita sobre a
  # camada de município completa de malha_basica_bruta.gpkg, que já cobre a totalidade dos
  # municípios do país -- não é preciso recontar contra uma fonte "oficial" à parte, como painel_funai
  # precisa fazer para seu fluxo de imputação parcial ao vivo), une essas N geometrias antes do
  # GROUP BY final em vez de deixá-las como feições individuais -- união é associativa, então o
  # resultado final não muda, só fica mais barato: o GEOS une poucos polígonos grandes em vez de
  # milhares de municípios pequenos numa única operação. Não altera malha_basica_bruta.gpkg (essa
  # camada mantém uma linha por município, preservando os joins de atributo por cd_mun feitos em
  # outras funções, ex.: tabela_dados_malha_basica()) -- a consolidação é interna a esta função.
  municipios <- malha_bruta |>
    dplyr::filter(tipo_territorio == "Município") |>
    dplyr::mutate(cd_uf = substr(cd_territorio, 1, 2))
  demais <- malha_bruta |> dplyr::filter(tipo_territorio != "Município")

  ufs_com_cr_unica <- municipios |>
    sf::st_drop_geometry() |>
    dplyr::distinct(cd_uf, cd_uorg) |>
    dplyr::count(cd_uf, name = "n_crs_distintas") |>
    dplyr::filter(n_crs_distintas == 1) |>
    dplyr::pull(cd_uf)

  municipios_consolidados <- municipios |>
    dplyr::filter(cd_uf %in% ufs_com_cr_unica) |>
    dplyr::group_by(cd_uf, cd_uorg, nome_uorg, sigla) |>
    dplyr::summarise(.groups = "drop") |>
    dplyr::select(-cd_uf)

  municipios_restantes <- municipios |>
    dplyr::filter(!cd_uf %in% ufs_com_cr_unica) |>
    dplyr::select(-cd_uf)

  malha_para_dissolver <- dplyr::bind_rows(
    demais |> dplyr::select(cd_uorg, nome_uorg, sigla),
    municipios_restantes |> dplyr::select(cd_uorg, nome_uorg, sigla),
    municipios_consolidados
  )

  malha_dissolvida <- malha_para_dissolver |>
    dplyr::group_by(cd_uorg, nome_uorg, sigla) |>
    dplyr::summarise(.groups = "drop") |>
    sf::st_make_valid() |>
    sf::st_buffer(epsilon_fechamento_m) |>
    sf::st_buffer(-epsilon_fechamento_m) |>
    sf::st_make_valid()

  malha_dissolvida$area_ha <- as.numeric(sf::st_area(malha_dissolvida)) / 10000

  if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)
  caminho_saida <- file.path(dir_saida, nome_arquivo)
  sf::st_write(malha_dissolvida, caminho_saida, delete_dsn = TRUE, quiet = TRUE)

  return(caminho_saida)
}
