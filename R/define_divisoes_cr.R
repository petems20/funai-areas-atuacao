#' @title Prepara lotes de destinos para a API OSRM
#' @description Fraciona um conjunto de polígonos de destino em lotes menores para respeitar os limites 
#' de elementos (origens x destinos) por requisição da matriz de distâncias da API do servidor demo do OSRM.
#' Atualmente o servidor é limitado até 10000 elementos por requisição, então essa função calcula quantos destinos 
#' podem ser processados por vez
#' @param dados_origens Data.frame contendo as coordenadas das origens (CRs).
#' @param sf_poligonos Objeto sf contendo os polígonos de destino (ex: setores censitários).
#' @param limite_elementos_matriz Inteiro. Limite máximo de elementos por requisição na API.
#' @return Uma lista de data.frames, onde cada elemento é um lote de destinos.
prepara_lotes_osrm <- function(dados_origens, sf_poligonos, limite_elementos_matriz = 10000) {
  library(sf)
  library(dplyr)

  # Calcula destinos a partir do centróide do setor censitário
  suppressWarnings(sf_poligonos_cent <- st_centroid(sf_poligonos))
  coords_poligonos <- st_coordinates(sf_poligonos_cent)

  # Monta dataframe para processamento vetorizado da API
  df_destinos <- data.frame(
    id_destino = seq_len(nrow(sf_poligonos_cent)),
    lon = as.numeric(coords_poligonos[,1]),
    lat = as.numeric(coords_poligonos[,2])
  )

  # Calcula lotes por tamanho
  n_origens <- nrow(dados_origens)
  max_destinos <- floor(limite_elementos_matriz / n_origens)

  if (max_destinos < 1) stop(sprintf("Erro: O limite da matriz estourou (n_origens = %d).", n_origens))

  return(split(df_destinos, ceiling(seq_len(nrow(df_destinos)) / max_destinos)))
}

# requisitar_api_osrm() foi consolidada em R/geoprocessamento.R (era duplicada aqui e lá).
# geoprocessamento.R é sourced via tar_source() no mesmo ambiente, então a função
# permanece disponível normalmente para as demais funções deste arquivo (ex: os targets
# que chamam requisitar_api_osrm() diretamente em _targets.R).

#' @title Extrai as respostas cruas da API do OSRM
#' @description Converte o retorno do alvo `resultados_api_brutos` (uma lista por lote, com
#'   `lote_destinos` e a `tabela` devolvida por `osrm::osrmTable()`) numa tabela longa com uma linha
#'   por par setor x CR. Essa tabela é o insumo bruto do roteamento: é ela que fica congelada em
#'   `dados/brutos/` (ver `ARQ_RESPOSTAS_OSRM` e `OSRM_MODO` em `R/config.R`).
#' @param lista_bruta Valor do alvo `resultados_api_brutos`.
#' @param sf_pontos Pontos de origem (CRs), na mesma ordem usada nas requisições.
#' @param arquivo_saida Caminho do `.parquet` a gravar.
#' @return Caminho do `.parquet` gravado, com `id_destino`, `cd_uorg`, `duracao` e `distancia`.
extrair_respostas_osrm <- function(lista_bruta, sf_pontos, arquivo_saida) {
  cd_uorgs <- as.character(sf_pontos$codigoUnidade)
  n_origens <- length(cd_uorgs)

  # lista_bruta é a lista achatada e nomeada que o {targets} devolve para um alvo com
  # pattern=map() (um objeto por branch, cada um já uma lista com elementos
  # `lote_destinos`/`tabela`) -- separa em duas listas alinhadas pelo nome do elemento.
  lista_destinos <- lista_bruta[grep("_lote_destinos$", names(lista_bruta))]
  lista_tabelas  <- lista_bruta[grep("_tabela$", names(lista_bruta))]

  respostas <- purrr::map2(lista_destinos, lista_tabelas, function(lote_dest, tab) {
    if (is.null(tab) || is.null(tab$durations)) return(NULL)
    n_destinos <- ncol(tab$durations)
    data.table::data.table(
      id_destino = rep(lote_dest$id_destino, each = n_origens),
      cd_uorg = rep(cd_uorgs, times = n_destinos),
      duracao = as.vector(tab$durations),
      distancia = as.vector(tab$distances)
    )
  }) |>
    data.table::rbindlist()

  dir.create(dirname(arquivo_saida), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(respostas, sink = arquivo_saida)
  arquivo_saida
}

#' @title Consolida as respostas do OSRM
#' @description A partir das respostas cruas (`extrair_respostas_osrm()`), calcula a distância em
#'   linha reta de cada par, o critério de comparação (duração, distância ou linha reta), a ordem
#'   das CRs por setor e a validade da rota, e grava a tabela usada pelo restante do pipeline.
#' @param arq_respostas Caminho do `.parquet` de respostas cruas.
#' @param sf_pontos Objeto sf com os pontos de origem (CRs).
#' @param sf_setores Objeto sf dos setores censitários (destinos), na ordem de `id_destino`.
#' @param dir_output Nome do arquivo de saída (gravado em `DIR_CACHE_OSRM`).
#' @return Caminho do `.parquet` consolidado.
consolidar_resultados_osrm <- function(arq_respostas, sf_pontos, sf_setores, dir_output) {

  df_final <- data.table::as.data.table(arrow::read_parquet(arq_respostas))

  # Distância em linha reta entre cada origem e o centróide do setor de destino -- mesmos pontos
  # usados nas requisições (ver prepara_lotes_osrm()).
  coords_pontos <- sf::st_coordinates(sf_pontos)
  src_sf <- sf::st_as_sf(
    data.frame(lon = coords_pontos[, 1], lat = coords_pontos[, 2]),
    coords = c("lon", "lat"),
    crs = 4326
  )
  idx_origem <- match(df_final$cd_uorg, as.character(sf_pontos$codigoUnidade))
  suppressWarnings(centroides <- sf::st_centroid(sf::st_geometry(sf_setores)))
  coords_dest <- sf::st_coordinates(centroides)
  dst_sf <- sf::st_as_sf(
    data.frame(lon = coords_dest[, 1], lat = coords_dest[, 2]),
    coords = c("lon", "lat"),
    crs = 4326
  )
  # Calculado por destino (em blocos) para conter a memória: matriz origens x destinos do bloco
  destinos <- sort(unique(df_final$id_destino))
  blocos <- split(destinos, ceiling(seq_along(destinos) / 5000))
  dist_reta <- data.table::rbindlist(lapply(blocos, function(ids) {
    m <- sf::st_distance(src_sf, dst_sf[ids, ]) |> units::drop_units() |> as.matrix()
    data.table::data.table(
      id_destino = rep(ids, each = nrow(m)),
      idx_origem = rep(seq_len(nrow(m)), times = length(ids)),
      distancia_reta = as.vector(m)
    )
  }))
  df_final[, idx_origem := idx_origem]
  df_final[dist_reta, distancia_reta := i.distancia_reta, on = .(id_destino, idx_origem)]
  df_final[, idx_origem := NULL]
  rm(dist_reta); gc()
  
  # Resgate instantâneo do cd_setor em O(1)
  vetor_cd_setor <- as.character(sf_setores$cd_setor)
  vetor_cd_uf <- as.integer(sf_setores$cd_uf)
  vetor_nm_uf <- as.character(sf_setores$nm_uf)
  vetor_cd_regiao <- as.integer(sf_setores$cd_regiao)
  vetor_nm_regiao <- as.character(sf_setores$nm_regiao)
  
  
  df_final[
    , `:=`(
      cd_setor  =  vetor_cd_setor[id_destino],
      cd_uf     =  vetor_cd_uf[id_destino],
      nm_uf     =  vetor_nm_uf[id_destino],
      cd_regiao =  vetor_cd_regiao[id_destino],
      nm_regiao =  vetor_nm_regiao[id_destino]
    )
  ]
  
  
  # Pré-calcula flags por setor
  df_final[
    , `:=`(
      tem_duracao   = any(!is.na(duracao)),
      tem_distancia = any(!is.na(distancia))
    ),
    by = cd_setor
  ]
  
  # Aplica critérios de forma vetorizada
  df_final[
    , `:=`(
      criterio = fifelse(
        tem_duracao,
        "OSRM (Duração)",
        fifelse(
          tem_distancia,
          "OSRM (Distância)",
          "Linha Reta"
        )
      ),
      valor_criterio = fifelse(
        tem_duracao,
        duracao,
        fifelse(
          tem_distancia,
          distancia,
          distancia_reta
        )
      )
    )
  ]
  
  # ordena por valor_criterio e cria ordem
  df_final[
    order(cd_setor, valor_criterio),
    `:=`(
      ordem = seq_len(.N),
      valor_criterio_melhor = first(valor_criterio),
      menor_duracao = ifelse(tem_duracao, min(duracao, na.rm = TRUE), NA_real_),
      menor_distancia = ifelse(tem_distancia, min(distancia, na.rm = TRUE), NA_real_),
      menor_distancia_reta = min(distancia_reta, na.rm = TRUE)
    ),
    by = cd_setor
  ][
    , c("tem_duracao", "tem_distancia") := NULL
  ]
  
  # Cria coluna de perda
  df_final[, perda := valor_criterio - valor_criterio_melhor]
  
  df_final <- df_final[order(cd_setor, ordem), ]
  
  # Insere nome e sigla da CR
  setDT(sf_pontos)
  
  sf_pontos[, codigoUnidade := as.character(codigoUnidade)]
  
  df_final <- df_final[
    sf_pontos[
      , .(codigoUnidade, nome_uorg = nome, sigla)
    ],
    on = .(cd_uorg = codigoUnidade)
  ]
  
  df_final <- unique(df_final, by = c("cd_setor", "cd_uorg"))
  
  # Parâmetros de validade
  limite_minutos <- 24 * 60
  
  # Aplicação da regra de invalidação
  df_final[, rota_valida := !is.na(distancia) & (duracao <= limite_minutos)]
  
  
  diretorio_saida <- DIR_CACHE_OSRM
  if (!dir.exists(diretorio_saida)) dir.create(diretorio_saida, recursive = TRUE)

  caminho_arquivo <- file.path(diretorio_saida, dir_output)
  write_parquet(df_final, sink = caminho_arquivo)
  
  return(caminho_arquivo)
}


#' @title Processa Resultados do OSRM
#' @description Consome a tabela monolítica consolidada e elege a CR ideal para cada setor censitário.
#' @param dt_consolidado data.table gerado pelo target tabela_osrm_consolidada.
#' @return Um data.table indexado por cd_setor contendo a unidade organizacional ótima escolhida.
processar_calculos_osrm_v2 <- function(
    dt_consolidado,
    realocacao = FALSE, 
    sf_setores_atual = NULL,
    malha_setores = arq_setores_censitarios) {
  
  # dt_consolidado = tar_read(tabela_osrm_consolidada)
  # malha_setores = tar_read(arq_setores_censitarios)
  # realocacao = TRUE
  # sf_setores_atual = tar_read(sf_setores_realocados_v2)
  
  dt_consolidado <- arrow::read_parquet(dt_consolidado)
  
  data.table::setDT(dt_consolidado)
  
  # Regra de Realocação: Invalidar a CR Atual via Join
  if (realocacao) {
    if (is.null(sf_setores_atual)) {
      rlang::abort("Para realocar, forneça 'sf_setores_atual' contendo 'cd_setor' e 'cd_uorg'.")
    }
    
    # Prepara o dicionário de setores atuais (remove geometria se for sf)
    df_atual <- if (inherits(sf_setores_atual, "sf")) sf::st_drop_geometry(sf_setores_atual) else sf_setores_atual
    data.table::setDT(df_atual)
    
    # Garante que as chaves sejam do mesmo tipo para o join
    df_atual[, `:=`(cd_setor = as.character(cd_setor), cd_uorg = as.character(cd_uorg))]
    dt_consolidado[, `:=`(cd_setor = as.character(cd_setor), cd_uorg = as.character(cd_uorg))]
    
    # 2. UPDATE ON JOIN: Onde cd_setor e cd_uorg baterem, invalida a rota
    dt_consolidado[
      df_atual, 
      `:=`(
        rota_valida = FALSE,
        criterio = "Realocado" # Marcador interno da linha perdedora
      ), 
      on = .(cd_setor, cd_uorg)
    ]
  }
  
  # Parâmetros de validade
  # limite_minutos <- 24 * 60
  
  # Aplicação da regra de invalidação
  # dt_consolidado[, rota_valida := !is.na(distancia) & (duracao <= limite_minutos)]
  
  # Recalcula campos
  
  dt_consolidado[, `:=`(
    rotas_validas = sum(rota_valida),
    rotas_invalidadas = sum(!rota_valida & (!is.na(distancia) | !is.na(duracao))),
    rotas_total = sum(!is.na(distancia) | !is.na(duracao))
  ), by = cd_setor]
  
  # Ordenação: rotas válidas primeiro, seguidas por duração, distância e linha reta
  dt_ordenado <- dt_consolidado[order(
    -rota_valida, 
    data.table::fcoalesce(duracao, Inf), 
    data.table::fcoalesce(distancia, Inf), 
    distancia_reta
  )]
  
  # 2. IDENTIFICAÇÃO DE EMPATES
  # Identifica todos os candidatos que empatam exatamente com o primeiro da fila (melhor)
  # Utiliza-se round para evitar micro-diferenças de ponto flutuante no R
  dt_ordenado[, is_best := (
    rota_valida == rota_valida[1] &
      round(data.table::fcoalesce(duracao, Inf), 4) == round(data.table::fcoalesce(duracao[1], Inf), 4) &
      round(data.table::fcoalesce(distancia, Inf), 4) == round(data.table::fcoalesce(distancia[1], Inf), 4) &
      round(distancia_reta, 4) == round(distancia_reta[1], 4)
  ), by = cd_setor]
  
  
  dt_candidatos <- dt_ordenado[is_best == TRUE]
  dt_candidatos[, n_empates := .N, by = cd_setor]
  
  dt_incontestaveis <- dt_candidatos[n_empates == 1]
  dt_empatados      <- dt_candidatos[n_empates > 1]

  # Colunas originais de dt_consolidado (sem is_best/n_empates/area_jurisdicao, que são
  # auxiliares criadas apenas nas cópias intermediárias). Definida antes do if/else para que
  # dt_otimo tenha sempre o mesmo formato de colunas independentemente do caminho seguido.
  colunas_manter <- names(dt_consolidado)

  if (nrow(dt_empatados) == 0) {
    dt_otimo <- dt_incontestaveis[, ..colunas_manter]
  } else {
    
    # 3. EXTRAÇÃO DE ÁREAS DA MALHA --------------------------------------------
    sf_malha <- if (inherits(malha_setores, "sf")) sf::st_drop_geometry(malha_setores) else malha_setores
    
    # Se a área não estiver na tabela, calcula na hora em metros quadrados
    
    if ("area_km2" %in% names(sf_malha)) {
    dt_areas <- data.table::data.table(
      cd_setor = as.character(sf_malha$cd_setor),
      area_m2 = as.numeric(sf_malha$area_km2)
    )
    } else {
      rlang::abort("malha_setores precisa conter o campo de area_km2.")
    }
    
    # 4. RESOLUÇÃO DETERMINÍSTICA DO EMPATE
    # A. Calcula a "Área Base" de cada CR usando apenas os setores incontestáveis
    dt_incontestaveis_area <- merge(dt_incontestaveis, dt_areas, by = "cd_setor", all.x = TRUE)
    dt_cr_base_area <- dt_incontestaveis_area[, .(area_jurisdicao = sum(area_m2, na.rm = TRUE)), by = cd_uorg]
    
    # B. Acopla a Área Base aos candidatos que estão empatados
    dt_empatados <- merge(dt_empatados, dt_cr_base_area, by = "cd_uorg", all.x = TRUE)
    dt_empatados[is.na(area_jurisdicao), area_jurisdicao := 0] # CRs sem nenhum setor incontestável começam com 0
    
    # C. Desempate Absoluto:
    # 1º: Menor área de jurisdição
    # 2º: Ordem alfanumérica da CR (cd_uorg) -> Garante unicidade e consistência na reprodutibilidade
    dt_empatados_resolvidos <- dt_empatados[order(cd_setor, area_jurisdicao, cd_uorg)]
    dt_empatados_resolvidos <- dt_empatados_resolvidos[, .SD[1], by = cd_setor]
    
    # 5. CONSOLIDAÇÃO FINAL
    dt_otimo <- data.table::rbindlist(list(
      dt_incontestaveis[, ..colunas_manter],
      dt_empatados_resolvidos[, ..colunas_manter]
    ))
  }

  # Atualização da coluna de auditoria de critérios
  dt_otimo[, criterio_final := dplyr::case_when(
    rota_valida & !is.na(duracao) ~ "OSRM (Duração)",
    rota_valida & !is.na(distancia) ~ "OSRM (Distância)",
    TRUE ~ "Linha Reta (Fallback)"
  )]
  
  if (nrow(dt_empatados) > 0) {
    setores_desempatados <- unique(dt_empatados$cd_setor)
    dt_otimo[cd_setor %in% setores_desempatados, criterio_final := paste0(criterio_final, " + Desempate por Área")]
  }
  
  dt_otimo <- dt_otimo[order(cd_setor, ordem), ]

  return(dt_otimo)
}

#' @title Processa realocações territoriais com base nos resultados do OSRM
#'
#' @description Seleciona a unidade organizacional ótima para cada setor censitário a partir
#' dos resultados consolidados do OSRM, invalidando previamente as alocações
#' atuais e priorizando rotas válidas, menor duração, menor distância e menor
#' distância em linha reta.
#'
#' Os resultados são incorporados ao objeto espacial de polígonos e gravados
#' em arquivo.
#'
#' @param arq_resultados_osrm Caminho para o dataset consolidado de resultados
#'   do OSRM.
#' @param sf_poligonos Objeto \code{sf} contendo os polígonos a serem atualizados.
#' @param sf_setores_atual Objeto contendo a configuração territorial atual.
#'   Deve possuir as colunas \code{cd_setor} e \code{cd_uorg}.
#' @param nome_arquivo Nome do arquivo de saída.
#'
#' @return Caminho do arquivo espacial gerado contendo os polígonos atualizados.
#'
#' @export
processa_realocacoes <- function(arq_resultados_osrm, sf_poligonos, sf_setores_atual = NULL, nome_arquivo) {
  
  # arq_resultados_osrm = tar_read(tabela_osrm_consolidada)
  # sf_poligonos = st_read(tar_read(arquivo_gpkg_suavizado))
  # realocacao = TRUE
  # sf_setores_atual = tar_read(sf_setores_realocados_v2)
  
  if (is.null(sf_setores_atual) || !all(c('cd_setor', 'cd_uorg') %in% colnames(sf_setores_atual))) {
      rlang::abort("Para realocar, forneça 'sf_setores_atual' contendo 'cd_setor' e 'cd_uorg'.")
  }
  
  dt_consolidado <- open_dataset(arq_resultados_osrm) |> filter(cd_setor %in% sf_setores_atual$cd_setor) |> collect()
  
  data.table::setDT(dt_consolidado)
    
  # 1. Prepara o dicionário de setores atuais (remove geometria se for sf)
  df_atual <- if (inherits(sf_setores_atual, "sf")) sf::st_drop_geometry(sf_setores_atual) else sf_setores_atual
  data.table::setDT(df_atual)
    
  # Garante que as chaves sejam do mesmo tipo para o join (character é mais seguro)
  df_atual[, `:=`(cd_setor = as.character(cd_setor), cd_uorg = as.character(cd_uorg))]
  dt_consolidado[, `:=`(cd_setor = as.character(cd_setor), cd_uorg = as.character(cd_uorg))]
    
  # 2. UPDATE ON JOIN: Onde cd_setor e cd_uorg baterem, invalida a rota
    dt_consolidado[
      df_atual, 
      `:=`(
        rota_valida = FALSE,
        criterio = "Realocado" # Marcador interno da linha perdedora
      ), 
      on = .(cd_setor, cd_uorg)
    ]
  
  # recalcula contagens de rotas válidas, inválidas e total por setor
  dt_consolidado[, `:=`(
    rotas_validas = sum(rota_valida),
    rotas_invalidadas = sum(!rota_valida & (!is.na(distancia) | !is.na(duracao))),
    rotas_total = sum(!is.na(distancia) | !is.na(duracao))
  ), by = cd_setor]
  
  # Ordenação: rotas válidas primeiro, seguidas por duração, distância e linha reta
  dt_ordenado <- dt_consolidado[order(
    -rota_valida, 
    data.table::fcoalesce(duracao, Inf), 
    data.table::fcoalesce(distancia, Inf), 
    distancia_reta
  )]
  
  # Eleição do campeão: seleciona apenas a primeira linha de cada grupo de setor
  dt_otimo <- dt_ordenado[, .SD[1], by = cd_setor]
  
  # Atualização fina da coluna de auditoria de critérios
  dt_otimo[, criterio := dplyr::case_when(
    rota_valida & !is.na(duracao) ~ "Realocação OSRM (Duração)",
    rota_valida & !is.na(distancia) ~ "Realocação OSRM (Distância)",
    TRUE ~ "Realocação Linha Reta"
  )]
  
  # SELECIONA APENAS COLUNAS CD_SETOR E CD_UORG
  dt_otimo <- dt_otimo[, .(cd_setor, cd_uorg, duracao, distancia, distancia_reta, criterio,
                           valor_criterio, ordem, perda, nome_uorg, sigla, rota_valida, rotas_validas,
                           rotas_invalidadas, rotas_total)]
  
  
  # guarda geometria
  geom <- st_geometry(sf_poligonos)
  
  # remove geometria
  sf_attrs <- st_drop_geometry(sf_poligonos)
  
  # update
  sf_attrs <- rows_update(
    sf_attrs,
    dt_otimo,
    by = "cd_setor"
  )
  
  # recoloca geometria
  sf_poligonos <- st_as_sf(
    sf_attrs,
    geometry = geom
  )
  
  diretorio_saida <- DIR_PROCESSADOS
  if (!dir.exists(diretorio_saida)) dir.create(diretorio_saida, recursive = TRUE)
  
  caminho_arquivo <- file.path(diretorio_saida, nome_arquivo)
  sf::st_write(sf_poligonos, caminho_arquivo, delete_dsn = TRUE, quiet = TRUE)
  
  return(caminho_arquivo)
}

#' @title Processa realocações utilizando cache do OSRM
#' @description Otimiza a realocação de setores específicos consumindo matrizes OSRM já cacheadas da Versão 1, 
#' anulando (mascarando) a UORG original para forçar o algoritmo a encontrar a próxima melhor alternativa.
#' 
#' @param resultado_bruto Lista cacheada contendo o lote de destinos e a tabela OSRM bruta.
#' @param sf_pontos Objeto sf com os pontos de origem.
#' @param sf_realocacao Objeto sf filtrado contendo apenas os destinos que exigem realocação.
#' @return Um data.frame com os novos resultados de roteamento apenas para os alvos realocados.
processar_realocacao_com_cache <- function(resultado_bruto, sf_pontos, sf_realocacao) {
  library(sf)
  library(dplyr)
  library(units)
  
  lote_destinos <- resultado_bruto$lote_destinos
  tabela <- resultado_bruto$tabela
  
  # 1. Cruzamos os destinos deste lote com os que realmente precisam de realocação
  alvos_lote <- lote_destinos |>
    inner_join(
      sf_realocacao |> st_drop_geometry() |> select(id_destino, uorg_ignorar = cd_uorg), 
      by = "id_destino"
    )
  
  # Se nenhum setor deste lote precisa de realocação, devolvemos vazio e o R pula para o próximo!
  if (nrow(alvos_lote) == 0) {
    return(data.frame()) 
  }
  
  # 2. Resgatamos os índices das colunas exatas destes alvos nas matrizes do cache
  col_indices <- which(lote_destinos$id_destino %in% alvos_lote$id_destino)
  
  coords_pontos <- st_coordinates(sf_pontos)
  df_origens <- data.frame(
    id_origem = seq_len(nrow(sf_pontos)),
    cd_uorg = sf_pontos$codigoUnidade,
    lon = as.numeric(coords_pontos[,1]),
    lat = as.numeric(coords_pontos[,2])
  )
  
  src_sf <- st_as_sf(df_origens, coords = c("lon", "lat"), crs = 4326)
  dst_sf <- st_as_sf(alvos_lote, coords = c("lon", "lat"), crs = 4326)
  
  # 3. EXTRAÇÃO CIRÚRGICA DO CACHE (Pegamos só a fatia que importa)
  dur_matrix <- tabela$durations[, col_indices, drop = FALSE]
  dist_matrix <- tabela$distances[, col_indices, drop = FALSE]
  dist_reta_matrix <- st_distance(src_sf, dst_sf) |> drop_units() |> as.matrix()
  
  # 4. A MÁSCARA (Anulamos a CR antiga forçando NA para estes alvos específicos)
  for (j in seq_along(alvos_lote$id_destino)) {
    uorg_ignorar <- alvos_lote$uorg_ignorar[j]
    idx_ignorar <- which(df_origens$cd_uorg == uorg_ignorar)
    if (length(idx_ignorar) > 0) {
      dur_matrix[idx_ignorar, j] <- NA
      dist_matrix[idx_ignorar, j] <- NA
      dist_reta_matrix[idx_ignorar, j] <- Inf
    }
  }
  
  # ELIMINANDO ROTAS INVÁLIDAS
  # Parâmetros de negócio
  multiplicador <- 3           # Tolerância
  buffer_m <- 50000             # Franquia de 50km (em METROS) para desvios locais
  limite_minutos <- 24 * 60      # Teto máximo aceitável de viagem (24 horas em MINUTOS)

  # Calcula o limite aceitável de distância para cada par Origem-Destino
  limite_distancia <- (dist_reta_matrix * multiplicador) + buffer_m

  # Invalida se:
  # A) O desvio for maior que o limite calculado OR
  # B) A viagem demorar mais de 24 horas no OSRM
  rotas_invalidas <- !is.na(dist_matrix) & (
    (dist_matrix > limite_distancia) |
      (dur_matrix > limite_minutos)
  )
  
  dur_matrix[rotas_invalidas] <- NA
  dist_matrix[rotas_invalidas] <- NA
  # --- FIM DA REGRA ---
  
  num_destinos <- ncol(dur_matrix)
  menor_duracao <- numeric(num_destinos)
  menor_distancia <- numeric(num_destinos)
  menor_distancia_reta <- numeric(num_destinos)
  id_origem_escolhida <- integer(num_destinos)
  criterio_utilizado <- character(num_destinos)
  
  # 5. CÁLCULO DA MELHOR ROTA ALTERNATIVA
  for (col_idx in seq_len(num_destinos)) {
    coluna_dur <- dur_matrix[, col_idx]
    coluna_dist <- dist_matrix[, col_idx]
    coluna_reta <- dist_reta_matrix[, col_idx] 
    
    if (!all(is.na(coluna_dur))) {
      linha_idx <- which.min(coluna_dur)
      criterio <- "Realocação OSRM (Duração)"
      dur_final <- coluna_dur[linha_idx]
      dist_final <- coluna_dist[linha_idx]
      dist_final_reta <- coluna_reta[linha_idx]
    } 
    else if (!all(is.na(coluna_dist))) {
      linha_idx <- which.min(coluna_dist)
      criterio <- "Realocação OSRM (Distância)"
      dur_final <- coluna_dur[linha_idx]
      dist_final <- coluna_dist[linha_idx]
      dist_final_reta <- coluna_reta[linha_idx]
    } 
    else {
      linha_idx <- which.min(coluna_reta)
      criterio <- "Realocação Linha Reta (Fallback)"
      dur_final <- NA_real_               
      dist_final <- NA_real_              
      dist_final_reta <- coluna_reta[linha_idx] 
    }
    
    id_origem_escolhida[col_idx] <- linha_idx
    menor_duracao[col_idx] <- dur_final
    menor_distancia[col_idx] <- dist_final
    menor_distancia_reta[col_idx] <- dist_final_reta
    criterio_utilizado[col_idx] <- criterio
  }
  
  df_resultado <- data.frame(
    id_destino = alvos_lote$id_destino,
    menor_duracao = menor_duracao,
    menor_distancia = menor_distancia,
    distancia_reta = menor_distancia_reta,
    id_origem = id_origem_escolhida,
    criterio = criterio_utilizado
  )
  
  return(df_resultado)
}

#' @title Salvar Geopackage Final de Atendimento
#' @description Une os polígonos dos setores censitários com os resultados calculados de rotas OSRM.
#' @param sf_poligonos Objeto sf original com os polígonos dos setores censitários.
#' @param df_resultados_osrm Tabela vinda do target resultados_processados (formato data.table/data.frame).
#' @param sf_pontos Objeto sf com os pontos origens das CRs para resgate de metadados.
#' @param nome_arquivo Caractere indicando o nome do arquivo gpkg de saída.
#' @return Caminho do arquivo GeoPackage gerado.
salva_geopackage_final_osrm <- function(sf_poligonos, df_resultados_osrm, sf_pontos, nome_arquivo) {
  
  # sf_poligonos = tar_read(sf_setores_filtrados)
  # df_resultados_osrm = tar_read(resultados_processados)
  # sf_pontos = tar_read(sf_pontos_filtrados)
  # nome_arquivo = "setores_cr_todos.gpkg"
  
  # Preparação da tabela de rotas ótimas
  df_rotas <- as.data.frame(df_resultados_osrm) |> 
    dplyr::mutate(
      cd_setor = as.character(cd_setor),
      cd_uorg = as.character(cd_uorg)
    ) |>
    dplyr::select(-nm_regiao, -nm_uf, -cd_regiao, -cd_uf)
  
  # Consolidação geográfica final ----
  sf_final <- sf_poligonos |> 
    dplyr::mutate(cd_setor = as.character(cd_setor)) |> 
    dplyr::left_join(df_rotas, by = "cd_setor")
  
  diretorio_saida <- DIR_PROCESSADOS
  if (!dir.exists(diretorio_saida)) dir.create(diretorio_saida, recursive = TRUE)
  
  caminho_arquivo <- file.path(diretorio_saida, nome_arquivo)
  sf::st_write(sf_final, caminho_arquivo, delete_dsn = TRUE, quiet = TRUE)
  
  return(caminho_arquivo)
}


imputar_atendimento_declarado <- function(
      arq_gpkg_final, 
      arq_tabela_declarados,
      arq_siorg
      ) {
    library(sf)
    library(dplyr)
    library(readxl)
    
    crs_novas <- c(509512, 509751, 509690)
    
    # 1. LEITURA DOS DADOS
    sf_setores <- st_read(arq_gpkg_final, quiet = TRUE)
    
    siorg <- read.csv(arq_siorg)
    
    # Lê a tabela do Excel (pode exigir ajuste se a aba tiver nome específico)
    df_declarados <- read_excel(arq_tabela_declarados, sheet = 'UORG_MUN') 
    
    # 2. TRATAMENTO DA TABELA DE DECLARADOS (Desempate pelo SEI)
    df_declarados_unicos <- df_declarados |>
      # Garante que o SEI é numérico para podermos achar o valor máximo
      mutate(sei = as.numeric(sei)) |> 
      group_by(cd_mun) |>
      # Pega apenas 1 linha por município (a que tiver o maior SEI). 
      # with_ties = FALSE garante que se houver dois SEIs iguais, ele pega apenas a primeira ocorrencia
      slice_max(order_by = sei, n = 1, with_ties = FALSE) |> 
      ungroup() |>
      select(
        cd_mun = cd_mun,
        cd_uorg_declarada = codigoUnidade,
        nome_uorg_declarada = unidade
      ) |>
      # Força os tipos para não dar erro no join com o Shapefile
      mutate(
        cd_mun = as.character(cd_mun),
        cd_uorg_declarada = as.numeric(cd_uorg_declarada),
        nome_uorg_declarada = as.character(nome_uorg_declarada)
      ) |>
      distinct(cd_mun, cd_uorg_declarada, nome_uorg_declarada) |> left_join(
        siorg |> select(codigoUnidade, sigla_declarada = sigla) |> distinct(),
        by = join_by(cd_uorg_declarada == codigoUnidade)
      ) 
    
    # 3. JOIN E IMPUTAÇÃO NOS SETORES CENSITÁRIOS
    sf_setores_imputados <- sf_setores |>
      mutate(
        cd_mun = as.character(cd_mun),
        cd_uorg = as.numeric(cd_uorg)
      ) |>
      left_join(df_declarados_unicos, by = "cd_mun") |>
      mutate(
        # Cria uma "máscara" verdadeira apenas para as linhas que cumprem as 3 regras:
        # 1. O município está na planilha do SEI (!is.na)
        # 2. O OSRM falhou e usou linha reta
        # 3. A CR que estava alocada NÃO é uma das novas (usando ! e %in%)
        deve_imputar = !is.na(cd_uorg_declarada) & 
          criterio == 'Linha Reta (Fallback)' & 
          !(cd_uorg %in% crs_novas),
        
        # Aplica a máscara de forma limpa e direta sem repetir as regras
        criterio = ifelse(deve_imputar, "Imputado (Declarado SEI)", criterio),
        nome_uorg = ifelse(deve_imputar, nome_uorg_declarada, nome_uorg),
        cd_uorg = ifelse(deve_imputar, cd_uorg_declarada, cd_uorg),
        sigla = ifelse(deve_imputar, trimws(sigla_declarada), trimws(sigla)),
        
        # Atualiza a etapa para rastreabilidade
        etapa = 2 
      ) |>
      # Limpa as colunas auxiliares, incluindo a máscara que criamos
      select(-cd_uorg_declarada, -nome_uorg_declarada, -sigla_declarada,-deve_imputar)
    
    # 4. SALVAMENTO
    diretorio_saida <- DIR_PROCESSADOS
    if (!dir.exists(diretorio_saida)) dir.create(diretorio_saida, recursive = TRUE)
    
    caminho_arquivo <- file.path(diretorio_saida, 'setores_cr_imputados_tabela.gpkg')
    
    st_write(sf_setores_imputados, caminho_arquivo, delete_dsn = TRUE, quiet = TRUE)
    
    # Retorna o caminho para o targets
    return(caminho_arquivo)
  }

#' @title Aplica imputações de atendimento
#' @description Sobrescreve as atribuições algorítmicas do OSRM com base em regras manuais ou programáticas 
#' pré-definidas em uma tabela de imputação.
#' 
#' @param arq_gpkg Caminho do GeoPackage com as atribuições atuais.
#' @param df_imputacao Data.frame contendo o mapeamento `cd_setor` -> `codigoUnidadeImputado`.
#' @param sf_siorg Objeto sf com as unidades administrativas para resgate de nomes e siglas.
#' @param out_dir String. Nome do arquivo GeoPackage de saída.
#' @param n_crit String. Nome descritivo do critério que será registrado na coluna 'criterio'.
#' @return String com o caminho do arquivo GeoPackage imputado.
imputar_atendimento <- function(
    arq_gpkg, 
    df_imputacao,
    sf_siorg,
    out_dir,
    n_crit,
    n_etapa,
    arq_resultados_osrm
  ) {
  
  # arq_gpkg = tar_read(arquivo_gpkg_resultado)
  # df_imputacao = tar_read(df_imputacao)
  # sf_siorg = tar_read(sf_pontos_filtrados)
  # out_dir = 'setores_cr_imputados_final.gpkg'
  # n_crit = 'Duração (Não OSRM)'
  
  # arq_gpkg = tar_read(arquivo_gpkg_suavizado_v2)
  # df_imputacao = tar_read(df_imputacao_v3)
  # sf_siorg = tar_read(sf_pontos_filtrados)
  # out_dir = 'setores_cr_imputados_final_v3.gpkg'
  # n_crit = 'Imputação Manual (V3)'
  # resultados_osrm = open_dataset(tar_read(tabela_osrm_consolidada)) |> dplyr::filter(cd_setor %in% df_imputacao$cd_setor) |> collect()
  
  resultados_osrm <- read_parquet(arq_resultados_osrm)
  
    # 1. LEITURA DOS DADOS
  sf_setores <- sf::st_read(arq_gpkg, quiet = TRUE)
    
  # PUXA DADOS DO OSRM PARA AS IMPUTAÇÕES
  df_imputados <- df_imputacao |> 
      dplyr::mutate(
      codigoUnidadeImputado = as.character(codigoUnidadeImputado),
      cd_setor = as.character(cd_setor)
    ) |>
      dplyr::left_join(
        resultados_osrm |> 
          dplyr::select(cd_setor, cd_uorg, nome_uorg, sigla, duracao, distancia, distancia_reta, criterio, valor_criterio, ordem, perda_imputado = perda, rota_valida),
        by = dplyr::join_by(cd_setor, codigoUnidadeImputado == cd_uorg)
      )
    
    # REMOVENDO IMPUTAÇÕES DESNECESSÁRIAS
    df_imputados <- df_imputados |>
      dplyr::anti_join(
        sf_setores |> sf::st_drop_geometry() |> dplyr::distinct(cd_setor, cd_uorg) |> dplyr::mutate(cd_uorg = as.character(cd_uorg)),
        by = dplyr::join_by(cd_setor, codigoUnidadeImputado == cd_uorg)
      )
    
    # 3. JOIN E IMPUTAÇÃO NOS SETORES CENSITÁRIOS
    sf_setores_imputados <- sf_setores |>
      dplyr::mutate(
        cd_mun = as.character(cd_mun),
        cd_uorg = as.character(cd_uorg)
      ) |>
      # Join trazendo nome e sigla com sufixo para evitar conflitos com colunas existentes
      dplyr::left_join(df_imputados |> 
                         dplyr::distinct(cd_setor, .keep_all = TRUE),
                by = "cd_setor", 
                suffix = c("", "_imputado"))
    
    sf_setores_imputados <- sf_setores_imputados |>
      dplyr::mutate(
        deve_imputar = !is.na(codigoUnidadeImputado),
        
        duracao = ifelse(deve_imputar, duracao_imputado, duracao),
        distancia = ifelse(deve_imputar, distancia_imputado, distancia),
        distancia_reta = ifelse(deve_imputar, distancia_reta_imputado, distancia_reta),
        criterio = ifelse(deve_imputar, n_crit, criterio),
        valor_criterio = ifelse(deve_imputar, valor_criterio_imputado, valor_criterio),
        ordem = ifelse(deve_imputar, ordem_imputado, ordem),
        rota_valida = ifelse(deve_imputar, rota_valida_imputado, rota_valida),
        nome_uorg = ifelse(deve_imputar, nome_uorg_imputado, nome_uorg),
        sigla = ifelse(deve_imputar, sigla_imputado, sigla), # Atualização da sigla solicitada
        cd_uorg = ifelse(deve_imputar, codigoUnidadeImputado, cd_uorg),
        perda = ifelse(deve_imputar, perda_imputado, perda),
        
        
        etapa = n_etapa 
      )
    
    # Checagem de exclusividade municipal (N distinto de cd_uorg por cd_mun deve ser 1) agora é
    # feita de fato, como parte do pipeline V3, por verificar_exclusividade_municipal()
    # (R/geoprocessamento.R) -- ver alvo `verificacao_exclusividade_mun` em _targets.R. Essa etapa
    # (imputar_atendimento) roda antes da correção topológica, então não é o lugar certo para a
    # checagem final: a correção topológica de setor pode reintroduzir uma segunda CR num
    # município mesmo que aqui o resultado esteja correto (ver corrigir_enclaves_municipais()).

    cat("Número de setores imputados:", sum(sf_setores_imputados$deve_imputar, na.rm = TRUE), "\n")
      # Limpeza das colunas auxiliares
      sf_setores_imputados <- sf_setores_imputados |>
        dplyr::select(-ends_with('_imputado'), -deve_imputar, -codigoUnidadeImputado)
    
    # 4. SALVAMENTO
    diretorio_saida <- DIR_PROCESSADOS
    if (!dir.exists(diretorio_saida)) dir.create(diretorio_saida, recursive = TRUE)
    
    caminho_arquivo <- file.path(diretorio_saida, out_dir)
    
    st_write(sf_setores_imputados, caminho_arquivo, delete_dsn = TRUE, quiet = TRUE)
    
    return(caminho_arquivo)
}


#' @title Exporta dados de população indígena (normalizados) para um arquivo DuckDB
#' @description Isola o objeto pesado de população indígena (ex.: censo_agregado_sc_pi_dados,
#' ~1030 colunas x ~460 mil linhas, ~3,6GB em memória) num arquivo `.duckdb` (banco de dados
#' em disco), com as colunas "v0*" já normalizadas ("X" -- célula suprimida pelo sigilo do
#' IBGE -- convertido para 0, resto para numérico).
#' @details Alvo `format = "file"` isolado de propósito (ver `unir_poligonos_tabular_cr()`):
#' este é o maior objeto de todo o fluxo de `unir_poligonos_*()`. Grava em lotes de linhas
#' (`DBI::dbWriteTable()` no primeiro lote, `dbAppendTable()` nos seguintes) direto na tabela
#' DuckDB em disco -- ao contrário de um `arrow::write_parquet(arq_pi, ...)` de uma vez só
#' (~6GB de pico, medido, perto demais da RAM livre nesta máquina somada ao overhead de
#' pacotes do {targets}/crew) ou de `duckdb::duckdb_register()` (que NÃO é realmente "sem
#' cópia" para colunas de texto -- vetores character do R não são contíguos em memória,
#' então o registro precisa materializar um buffer Arrow contíguo à parte, travando o
#' processo). Gravar em lotes menores mantém o pico de conversão proporcional ao tamanho do
#' lote, não do total. Rodar esta conversão como um alvo `{targets}` próprio garante, além
#' disso, que a memória de `arq_pi` é devolvida ao sistema operacional (o worker do crew é
#' reciclado entre alvos, `memory = 'transient'` em `_targets.R`) antes de qualquer etapa
#' seguinte começar -- `rm()`+`gc()` sozinhos, dentro do mesmo processo, não bastavam.
#' @param arq_pi Objeto sf ou data.frame/data.table com os dados de população indígena
#' (censo_agregado_sc_pi ou censo_agregado_sc_pi_dados).
#' @param join_var String. Nome da coluna chave para o join (ex: 'cd_setor').
#' @param caminho_saida String. Caminho completo do arquivo .duckdb de saída.
#' @return String com o caminho do arquivo .duckdb gravado (tabela "indigenas").
#' @details Usada hoje só para `censo_agregado_sc_pi` (V3/município -- ~135 mil linhas x 8
#' colunas "v0*", pico medido ~3,4GB, dentro do orçamento de 6-7GB da máquina alvo). Para
#' `censo_agregado_sc_pi_dados` (V1/V2/V2_MSOL -- ~459 mil linhas x 1029 colunas "v0*"), o
#' loop de normalização coluna-a-coluna abaixo mede pico de ~9,94GB (ainda estoura o
#' orçamento) -- use `exportar_indigenas_duckdb_sql()` nesse caso.
exportar_indigenas_duckdb <- function(arq_pi, join_var, caminho_saida) {

  # arq_pi = tar_read(censo_agregado_sc_pi_dados)
  # join_var = 'cd_setor'
  # caminho_saida = file.path(DIR_PROCESSADOS, "censo_indigenas_normalizado.duckdb")

  arq_pi <- sf::st_drop_geometry(arq_pi)
  data.table::setDT(arq_pi)

  # join_var (cd_setor) pode chegar como integer64 (pacote bit64, usado por data.table
  # para inteiros de 15+ dígitos) -- precisa virar character explicitamente antes de
  # gravar, senão o join por texto downstream quebra silenciosamente (medido: as.character()
  # sem bit64 carregado também dá lixo, mas data.table já traz suporte a integer64 registrado).
  data.table::set(arq_pi, j = join_var, value = as.character(arq_pi[[join_var]]))

  # Normaliza as colunas "v0*" via limpar_sigilo_ibge() (R/gera_graficos_relatorio.R): "X"
  # (célula suprimida pelo sigilo do IBGE) -> 0, resto convertido para numérico -- coluna a
  # coluna via data.table::set() (modifica por referência, sem duplicar as ~1029 colunas de uma
  # vez como um dplyr::mutate(across(...)) faria).
  colunas_v0 <- grep("^v0", names(arq_pi), value = TRUE)
  for (col in colunas_v0) {
    data.table::set(arq_pi, j = col, value = limpar_sigilo_ibge(arq_pi[[col]]))
  }

  diretorio_saida <- dirname(caminho_saida)
  if (!dir.exists(diretorio_saida)) dir.create(diretorio_saida, recursive = TRUE)
  if (file.exists(caminho_saida)) unlink(caminho_saida)

  conn <- duckspatial::ddbs_create_conn(dbdir = caminho_saida, threads = 1, memory_limit_gb = 3)
  duckspatial::ddbs_load(conn)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  n <- nrow(arq_pi)
  n_lotes <- 8
  tam_lote <- ceiling(n / n_lotes)
  for (i in seq_len(n_lotes)) {
    idx_inicio <- (i - 1) * tam_lote + 1
    if (idx_inicio > n) break
    idx_fim <- min(i * tam_lote, n)
    lote <- as.data.frame(arq_pi[idx_inicio:idx_fim, ])
    if (i == 1) {
      DBI::dbWriteTable(conn, "indigenas", lote, overwrite = TRUE)
    } else {
      DBI::dbAppendTable(conn, "indigenas", lote)
    }
    rm(lote); gc()
  }
  rm(arq_pi); gc()

  caminho_saida
}

#' @title Exporta dados de população indígena (normalizados) para DuckDB -- via SQL nativo
#' @description Variante de `exportar_indigenas_duckdb()` para a tabela grande de
#' `censo_agregado_sc_pi_dados` (~459 mil linhas x 1029 colunas "v0*"): em vez de receber o
#' objeto já baixado/unido em R, baixa os 2 CSVs brutos do IBGE (pessoas + domicílios
#' indígenas por setor) e faz a união + normalização "X" -> 0 inteiramente em SQL/DuckDB
#' (`read_csv_auto()` + `CASE WHEN`), sem nunca montar a tabela larga como objeto R.
#' @details Medido nesta máquina: a versão em R (`exportar_indigenas_duckdb()`, loop de
#' `data.table::set()` coluna a coluna sobre as ~1029 colunas "v0*") tem pico de ~9,94GB e
#' leva ~116s -- ainda estoura o orçamento de 6-7GB da máquina alvo mesmo depois da divisão em
#' alvos `{targets}` separados. Essa variante SQL mede ~3,07GB de pico e ~55s (~69% menos RAM,
#' ~52% mais rápida), resultado idêntico (mesmo `total_pi`/`total_area_km2` agregados
#' downstream). A causa provável do pico alto na versão R não é nenhum limite do DuckDB em si
#' (o comportamento do R no Windows de não devolver memória ao SO após `gc()` é o suspeito
#' mais plausível para um loop de 1029 iterações com muitas alocações pequenas), então a
#' correção é não materializar a tabela larga em R, não ajustar parâmetros do DuckDB.
#' Consequência aceita: os CSVs descompactados ficam temporariamente em disco (~1,9GB, contra
#' os ~38MB do `.qs2` da versão em R) -- troca RAM por disco, que é a troca certa numa máquina
#' com RAM escassa. Os CSVs são baixados de novo aqui (não reaproveita o cache do alvo
#' `censo_agregado_sc_pi_dados`, que ainda existe para os outros consumidores da tabela larga
#' em R -- ver `gera_graficos_relatorio.R`); o custo extra é só o download (~35s).
#' @param url_pessoas URL do zip de pessoas indígenas por setor (mesmo default do alvo
#' `censo_agregado_sc_pi_dados` em `_targets.R`).
#' @param url_domicilios URL do zip de domicílios indígenas por setor (idem).
#' @param join_var String. Nome da coluna chave para o join (ex: 'cd_setor').
#' @param caminho_saida String. Caminho completo do arquivo .duckdb de saída.
#' @return String com o caminho do arquivo .duckdb gravado (tabela "indigenas").
exportar_indigenas_duckdb_sql <- function(url_pessoas, url_domicilios, join_var, caminho_saida) {

  # url_pessoas = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/Agregados_por_setores_pessoas_indigenas_BR.zip"
  # url_domicilios = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/Agregados_por_setores_domicilios_indigenas_BR.zip"
  # join_var = 'cd_setor'
  # caminho_saida = file.path(DIR_PROCESSADOS, "censo_indigenas_normalizado.duckdb")

  dir_csv <- tempfile("censo_csv_")
  dir.create(dir_csv, recursive = TRUE)
  on.exit(unlink(dir_csv, recursive = TRUE), add = TRUE)

  baixar_unzip_csv <- function(url) {
    antes <- list.files(dir_csv, pattern = "\\.csv$", full.names = TRUE)
    zip_path <- file.path(dir_csv, basename(url))
    download.file(url, zip_path, mode = "wb")
    unzip(zip_path, exdir = dir_csv)
    unlink(zip_path)
    depois <- list.files(dir_csv, pattern = "\\.csv$", full.names = TRUE)
    novo <- setdiff(depois, antes)
    if (length(novo) != 1) stop("Esperava exatamente 1 novo CSV no zip: ", url)
    novo
  }

  csv_pessoas <- baixar_unzip_csv(url_pessoas)
  csv_domicilios <- baixar_unzip_csv(url_domicilios)

  diretorio_saida <- dirname(caminho_saida)
  if (!dir.exists(diretorio_saida)) dir.create(diretorio_saida, recursive = TRUE)
  if (file.exists(caminho_saida)) unlink(caminho_saida)

  conn <- duckspatial::ddbs_create_conn(dbdir = caminho_saida, threads = 1, memory_limit_gb = 3)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(conn, sprintf(
    "CREATE TABLE pessoas_raw AS SELECT * FROM read_csv_auto('%s', delim=';', quote='\"', header=true, all_varchar=true)",
    csv_pessoas
  ))
  DBI::dbExecute(conn, sprintf(
    "CREATE TABLE domicilios_raw AS SELECT * FROM read_csv_auto('%s', delim=';', quote='\"', header=true, all_varchar=true)",
    csv_domicilios
  ))

  # join_var chega em maiúsculas nos CSVs do IBGE (ex.: "CD_SETOR") -- as colunas "v0*"
  # também, por isso ficam em minúsculas explicitamente no SELECT abaixo (mesma convenção de
  # nome de coluna de `baixar_e_unir_agregados_censo()`/`download_agregados_censo()`).
  jv_upper <- toupper(join_var)
  cols_pessoas <- setdiff(DBI::dbListFields(conn, "pessoas_raw"), jv_upper)
  cols_domicilios <- setdiff(DBI::dbListFields(conn, "domicilios_raw"), jv_upper)

  norm_expr <- function(tbl_alias, orig_col) {
    sprintf('CASE WHEN %s."%s" = \'X\' THEN 0 ELSE TRY_CAST(%s."%s" AS DOUBLE) END AS %s',
            tbl_alias, orig_col, tbl_alias, orig_col, tolower(orig_col))
  }
  sel_pessoas <- paste(vapply(cols_pessoas, function(c) norm_expr("p", c), character(1)), collapse = ", ")
  sel_domicilios <- paste(vapply(cols_domicilios, function(c) norm_expr("d", c), character(1)), collapse = ", ")

  DBI::dbExecute(conn, sprintf('
    CREATE TABLE indigenas AS
    SELECT p."%s" AS %s, %s, %s
    FROM pessoas_raw p
    LEFT JOIN domicilios_raw d ON p."%s" = d."%s"
  ', jv_upper, join_var, sel_pessoas, sel_domicilios, jv_upper, jv_upper))

  DBI::dbExecute(conn, "DROP TABLE pessoas_raw")
  DBI::dbExecute(conn, "DROP TABLE domicilios_raw")

  caminho_saida
}

#' @title Calcula a agregação tabular por CR (dados populacionais, sem geometria)
#' @description Faz o join entre a malha de setores censitários e os dados de população
#' indígena (já normalizados por `exportar_indigenas_duckdb()`), agregando por CR: área
#' total, população indígena total/urbana/rural, e a soma de todas as colunas "v0*" (tanto
#' as nativas da malha de setores quanto as do censo indígena).
#' @details Roda em SQL/DuckDB: a malha de setores (~468 mil feições, só as colunas
#' necessárias) é lida direto do `.gpkg` via `ST_Read()` -- validado isoladamente nesta
#' máquina (rodando como alvo `{targets}` próprio, processo/worker recém-iniciado): sem
#' conversão prévia para outro formato, evitando complexidade a mais (ver histórico de
#' depuração de `arquivo_final_sc`) -- o problema de memória original era o acúmulo de
#' várias etapas pesadas no mesmo processo R, resolvido pela divisão em alvos `{targets}`
#' separados (cada um com seu próprio worker, `memory = 'transient'`), não pelo formato do
#' arquivo de leitura. Os dados indígenas ficam num arquivo `.duckdb` (`ATTACH`ado aqui como
#' catálogo somente leitura); o resultado desta função (uma linha por CR) é pequeno e
#' seguro de manter em memória R.
#' @param arq_gpkg Caminho do GeoPackage com os setores já atribuídos e corrigidos.
#' @param arq_pi_duckdb Caminho do arquivo .duckdb gerado por `exportar_indigenas_duckdb()`.
#' @param join_var String. Nome da coluna chave para o join (ex: 'cd_setor').
#' @return Data.frame com a agregação tabular por CR (cd_uorg, nome_uorg, sigla,
#' total_pi/total_pi_urbana/total_pi_rural/total_area_km2 e todas as colunas "v0*").
unir_poligonos_tabular_cr <- function(arq_gpkg, arq_pi_duckdb, join_var) {

  # arq_gpkg = tar_read(arquivo_gpkg_suavizado_v2)
  # arq_pi_duckdb = tar_read(arq_pi_duckdb_v2)
  # join_var = 'cd_setor'

  nome_layer <- sf::st_layers(arq_gpkg)$name[1]
  arq_gpkg_abs <- normalizePath(arq_gpkg, winslash = "/", mustWork = TRUE)
  arq_pi_duckdb_abs <- normalizePath(arq_pi_duckdb, winslash = "/", mustWork = TRUE)

  conn <- duckspatial::ddbs_create_conn(dbdir = "memory", threads = 1, memory_limit_gb = 3)
  duckspatial::ddbs_load(conn)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(conn, sprintf("ATTACH '%s' AS pi_db (READ_ONLY)", arq_pi_duckdb_abs))

  # A malha nacional suavizada tem ~468 mil feições e ~57 colunas (ver "Coluna de
  # geometria..." em CLAUDE.md), mas a agregação abaixo só usa join_var + cd_uorg/
  # nome_uorg/sigla/cd_sit/area_km2 (+ colunas "v0*" nativas, ex.: v0001..v0007 --
  # distintas das colunas "v0*" de pi_db.indigenas, como v01690, detectadas
  # dinamicamente para não hardcodear a quantidade/nomes exatos).
  colunas_disponiveis <- names(st_read(arq_gpkg, query = sprintf('SELECT * FROM "%s" LIMIT 0', nome_layer), quiet = TRUE))
  colunas_v0_origem <- grep("^v0", colunas_disponiveis, value = TRUE)
  colunas_setores <- unique(c(join_var, "cd_uorg", "nome_uorg", "sigla", "cd_sit", "area_km2", colunas_v0_origem))
  DBI::dbExecute(conn, sprintf(
    "CREATE TABLE setores AS SELECT %s FROM ST_Read('%s')",
    paste(colunas_setores, collapse = ", "), arq_gpkg_abs
  ))

  colunas_pi <- names(DBI::dbGetQuery(conn, "SELECT * FROM pi_db.indigenas LIMIT 0"))
  colunas_v0_pi <- grep("^v0", colunas_pi, value = TRUE)

  # DISTINCT ON (join_var) reproduz o distinct(cd_setor, .keep_all=TRUE) do código
  # original (lá aplicado só depois do join -- equivalente a aplicar antes, já que
  # pi_db.indigenas tem join_var único, garantido por exportar_indigenas_duckdb()).
  # cd_sit IN (1,2,3) decide os buckets total_pi_urbana/total_pi_rural.
  expr_sum_v0_setores <- sprintf("SUM(s.%s) AS %s", colunas_v0_origem, colunas_v0_origem)
  expr_sum_v0_pi <- sprintf("SUM(i.%s) AS %s", colunas_v0_pi, colunas_v0_pi)
  query_tabular <- sprintf('
    WITH setores_dedup AS (
      SELECT DISTINCT ON (%s) * FROM setores
    )
    SELECT
      s.cd_uorg, s.nome_uorg, s.sigla,
      SUM(i.v01690) AS total_pi,
      SUM(CASE WHEN s.cd_sit IN (1,2,3) THEN i.v01690 ELSE 0 END) AS total_pi_urbana,
      SUM(CASE WHEN s.cd_sit NOT IN (1,2,3) OR s.cd_sit IS NULL THEN i.v01690 ELSE 0 END) AS total_pi_rural,
      SUM(s.area_km2) AS total_area_km2,
      %s, %s
    FROM setores_dedup s
    LEFT JOIN pi_db.indigenas i ON CAST(s.%s AS VARCHAR) = i.%s
    GROUP BY s.cd_uorg, s.nome_uorg, s.sigla
  ', join_var, paste(expr_sum_v0_setores, collapse = ", "), paste(expr_sum_v0_pi, collapse = ", "), join_var, join_var)

  DBI::dbGetQuery(conn, query_tabular)
}

#' @title Realiza o dissolve espacial dos setores censitários por CR
#' @description Agrupa a malha de setores censitários por UORG (CR) e une as geometrias
#' (dissolve), retornando um polígono por CR.
#' @details O dissolve das ~468 mil geometrias de setor em ~40 polígonos de CR é a etapa
#' mais cara de todo o fluxo `unir_poligonos_*()`. `ST_Read()` (GDAL/OGR) direto no `.gpkg`,
#' combinado com `GROUP BY` + `ST_Union_Agg()`, chegou a dar um pico repentino de memória e
#' travar o processo -- mas isso aconteceu sempre dentro da função monolítica original
#' `unir_poligonos()`, com vários outros objetos grandes (a tabela de população indígena,
#' a agregação tabular) já residentes na mesma sessão R. Rodando como alvo `{targets}`
#' isolado (processo/worker recém-iniciado, sem nada mais residente), `ST_Read()` direto no
#' gpkg é seguro e tem tempo/memória equivalentes a uma conversão prévia para Parquet
#' (validado nesta máquina: ~238s, pico ~3,3GB em ambos os casos) -- a causa raiz do
#' travamento era o acúmulo de memória entre etapas dentro do mesmo processo, resolvida
#' pela divisão em alvos separados (cada um com seu próprio worker, `memory = 'transient'`
#' em `_targets.R`), não o formato do arquivo de leitura. `ST_Union_Agg()` valida idêntico
#' (diferença de área ~0, ruído de ponto flutuante) ao `st_union()` do sf/GEOS usado antes
#' -- não reprojeta para CRS_ALBERS_BR (EPSG:10857) aqui: a extensão spatial do DuckDB não
#' suporta esse datum, então a união roda no CRS original do gpkg (WGS84/EPSG:4326).
#' @param arq_gpkg Caminho do GeoPackage com os setores já atribuídos e corrigidos.
#' @return Objeto sf com um polígono por CR (colunas cd_uorg, nome_uorg, geometria).
unir_poligonos_geometria <- function(arq_gpkg) {

  # arq_gpkg = tar_read(arquivo_gpkg_suavizado_v2)

  nome_layer <- sf::st_layers(arq_gpkg)$name[1]
  arq_gpkg_abs <- normalizePath(arq_gpkg, winslash = "/", mustWork = TRUE)

  geom_meta <- st_read(arq_gpkg, query = sprintf('SELECT geom FROM "%s" LIMIT 0', nome_layer), quiet = TRUE)
  geom_col <- attr(geom_meta, "sf_column")
  crs_origem <- sf::st_crs(geom_meta)

  conn <- duckspatial::ddbs_create_conn(dbdir = "memory", threads = 1, memory_limit_gb = 3)
  duckspatial::ddbs_load(conn)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  duckspatial::ddbs_options(mode = "sf")

  DBI::dbExecute(conn, sprintf(
    "CREATE TABLE sf_dissolve AS SELECT cd_uorg, nome_uorg, ST_Union_Agg(%s) AS geom FROM ST_Read('%s') GROUP BY cd_uorg, nome_uorg",
    geom_col, arq_gpkg_abs
  ))
  sf_geometrias <- duckspatial::ddbs_read_table(conn, "sf_dissolve")
  sf::st_set_crs(sf_geometrias, crs_origem)
}

#' @title Junta a agregação tabular e a geometria por CR, salva o GeoPackage final
#' @description Etapa final de `unir_poligonos_*()`: junta o resultado de
#' `unir_poligonos_geometria()` (geometria por CR) com o de `unir_poligonos_tabular_cr()`
#' (agregação tabular por CR), reprojeta para `CRS_EXTRACAO` e grava o GeoPackage
#' consolidado. Ambos os insumos já são pequenos (~40 linhas cada), então esta etapa é
#' leve e não precisa de nenhum cuidado especial de memória.
#' @param sf_geometria Objeto sf retornado por `unir_poligonos_geometria()`.
#' @param df_tabular Data.frame retornado por `unir_poligonos_tabular_cr()`.
#' @param dir_output String. Nome do arquivo GeoPackage de saída consolidado.
#' @return String com o caminho do arquivo GeoPackage resultante.
unir_poligonos_final <- function(sf_geometria, df_tabular, dir_output, CRS_EXTRACAO = CRS_EXTRACAO) {

  sf_uorgs <- sf_geometria |>
    left_join(df_tabular, by = c("cd_uorg", "nome_uorg"))

  sf_uorgs <- sf::st_transform(sf_uorgs, CRS_EXTRACAO)

  diretorio_saida <- DIR_PROCESSADOS
  if (!dir.exists(diretorio_saida)) dir.create(diretorio_saida, recursive = TRUE)

  caminho_arquivo <- file.path(diretorio_saida, dir_output)

  st_write(sf_uorgs, caminho_arquivo, delete_dsn = TRUE, quiet = TRUE)

  caminho_arquivo
}


  #' @title Mapeia setores censitários que intersectam Terras Indígenas (TIs)
  #' @description Identifica quais setores censitários estão dentro ou tocam os limites de terras indígenas.
  #' @param arq_setores sf object. Objeto SF com a malha de setores censitários.
  #' @param arq_tis sf object. Objeto SF com os polígonos das Terras Indígenas.
  #' @param col_id_ti integer or vector. Nome da coluna que identifica a TI de forma única (padrão: "terrai_cod").
  #' @param threshold integer. Limiar de recorte para o mapeamento do setor censitário em termos percentuais e em relação a área do setor censitário.
  #' @return Um data.frame contendo a relação entre `cd_setor` e o id da Terra Indígena.
  mapear_setores_ti <- function(sf_setores, sf_tis, cd_tis = NULL, col_id_ti = "terrai_codigo", threshold = 4) {
    
    # sf_setores = targets::tar_read(arq_setores_censitarios)
    # sf_tis = targets::tar_read(arq_tis)
    
    # Leitura com correção topológica imediata para evitar geometrias corrompidas
    sf_setores <- sf_setores |> sf::st_make_valid()
    sf_tis <- sf_tis |> sf::st_make_valid()

    # Filtrando para as TIS eventualmente selecionadas
    if(!is.null(cd_tis)){
      sf_tis <- sf_tis |> filter(terrai_codigo %in% cd_tis)
    }
    
    # Matendo apenas a geometria e os identificadores cruciais
    sf_setores <- sf_setores |> dplyr::select(cd_setor, area_km2)
    sf_tis     <- sf_tis |> dplyr::select(id_ti = dplyr::all_of(col_id_ti), terrai_nome, modalidade_ti, fase_ti)

    # Garante mesmo sistema de coordenadas antes do cruzamento espacial (st_join), no mesmo
    # padrão adotado em corrigindo_topologia() (R/geoprocessamento.R)
    if (sf::st_crs(sf_setores) != sf::st_crs(sf_tis)) {
      sf_tis <- sf::st_transform(sf_tis, sf::st_crs(sf_setores))
    }

    # Encontra TODOS os setores (os que estão dentro + os que apenas tocam)
    setores_intersect <- sf::st_join(sf_setores, sf_tis, join = sf::st_intersects, left = FALSE)

    # Reprojetando para Albers e recalculando a área total do setor na mesma etapa/CRS usada
    # para a área de sobreposição (interseccao, abaixo). O antigo `area_km2` era herdado de uma
    # etapa anterior (upstream) com CRS/método de cálculo desconhecido, tornando pct_sc_contido
    # uma razão entre áreas não comparáveis (numerador e denominador em bases diferentes).
    sf_setores_proj <- sf::st_transform(setores_intersect |> dplyr::select(cd_setor), CRS_ALBERS_BR)
    sf_setores_proj$area_sc <- as.numeric(sf::st_area(sf_setores_proj)) / 1e6
    sf_tis_proj     <- sf::st_transform(sf_tis, CRS_ALBERS_BR) |> dplyr::select(id_ti, terrai_nome, modalidade_ti, fase_ti)

    # sf_setores_proj e sf_tis_proj já estão em CRS_ALBERS_BR (linhas acima) -- st_intersection()
    # preserva o CRS de entrada, então nenhuma reprojeção é necessária aqui.
    interseccao <- sf::st_intersection(sf_setores_proj, sf_tis_proj) |>
      dplyr::mutate(
        area_km2 = as.numeric(sf::st_area(geom)/1e6),
        pct_sc_contido = round(area_km2*100 / area_sc, 2)
        )
    
    
    # Filtrando treshold e preparando output.
    df_setores_ti <- interseccao |> 
      sf::st_drop_geometry() |> 
      dplyr::group_by(cd_setor) |>
      # Mantém a TI que pega a maior porção do setor (em caso de sobreposição com múltiplas TIs)
      dplyr::filter(area_km2 == max(area_km2)) |> 
      dplyr::ungroup() |> 
      dplyr::distinct() |> 
      # Define o threshold de ruído
      dplyr::filter(pct_sc_contido >= threshold) |> 
      dplyr::mutate(
        # Classifica a relação baseada em tolerância de borda
        tipo_relacao = case_when(
          pct_sc_contido >= (100-threshold) ~ 1, # 1 = Contido na TI
          TRUE                   ~ 2  # 2 = Parcial/Borda
        )
      ) |> 
      dplyr::select(-area_km2)
    
    return(df_setores_ti)
  }
  
  #' @title Atribui Coordenação Regional aos setores com base na área majoritária da TI
  #' @description Calcula a interseção geométrica entre TIs e CRs. Se uma TI está dividida, 
  #' a CR que detém a maior área do polígono da TI "herda" todos os setores censitários vinculados a ela.
  #' @param df_setores_ti Data.frame gerado pela função `mapear_setores_ti()`.
  #' @param arq_tis String. Caminho do arquivo com os polígonos das Terras Indígenas.
  #' @param arq_poligonos_cr String. Caminho do arquivo com os polígonos definidos das CRs.
  #' @param col_id_ti String. Nome da coluna que identifica a TI (padrão: "terrai_cod").
  #' @param n_crit String. Nome da regra para trilha de auditoria.
  #' @return Um data.frame de imputação pronto para ser consumido pela função `imputar_atendimento()`.
  atribuir_cr_por_area_ti <- function(
    df_setores_ti, 
    arq_tis, 
    arq_poligonos_cr, 
    col_id_ti = "terrai_codigo",
    n_crit = "Majoritário (Área TI)"
  ) {
    
    # 1. LEITURA E SELEÇÃO DE COLUNAS
    sf_tis <- sf::st_read(arq_tis, quiet = TRUE) |> 
      # sf::st_make_valid() |> 
      dplyr::select(id_ti = dplyr::all_of(col_id_ti))
    
    sf_crs <- sf::st_read(arq_poligonos_cr, quiet = TRUE) |> 
      # sf::st_make_valid() |> 
      dplyr::select(cd_uorg)
    
    # 2. PROJEÇÃO MÉTRICA PARA CÁLCULO DE ÁREA EXATO
    # Utilizando o EPSG: 10857 (SIRGAS 2000 ALBERS), padrão para cálculos de área em escala nacional
    crs_plano <- CRS_ALBERS_BR
    sf_tis_plano <- sf::st_transform(sf_tis, crs_plano)
    sf_crs_plano <- sf::st_transform(sf_crs, crs_plano)
    
    # 3. INTERSEÇÃO E CÁLCULO DE ÁREA VETORIZADO
    # Gera os "pedacinhos" de TIs cortados pelas fronteiras das CRs
    interseccao_ti_cr <- sf::st_intersection(sf_tis_plano, sf_crs_plano)
    
    # Calcula a área em metros quadrados de cada pedaço
    interseccao_ti_cr$area_pedaco <- sf::st_area(interseccao_ti_cr) |> 
      units::drop_units() |> 
      as.numeric()
    
    # 4. DEFINIÇÃO DA CR MAJORITÁRIA POR TI
    atribuicao_ti <- interseccao_ti_cr |>
      sf::st_drop_geometry() |>
      dplyr::group_by(id_ti) |>
      # Extrai estritamente a linha (CR) que contém a maior fatia de área da TI
      dplyr::slice_max(order_by = area_pedaco, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(id_ti, codigoUnidadeImputado = cd_uorg)
    
    # 5. ATRIBUIÇÃO AOS SETORES CENSITÁRIOS E AUDITORIA
    df_imputacao <- df_setores_ti |>
      dplyr::inner_join(atribuicao_ti, by = "id_ti") |>
      dplyr::select(cd_setor, codigoUnidadeImputado) |>
      # Garantia contra duplicações caso um setor intercepte mais de uma TI.
      # Pode-se sofisticar o desempate aqui se necessário.
      dplyr::distinct(cd_setor, .keep_all = TRUE) |>
      dplyr::mutate(criterio_auditoria = n_crit)
    
    return(df_imputacao)
  }
  
  #' @title Atribui Coordenação Regional com Desempate por Tipo de Relação
  #' @description Filtra os setores vinculados a TIs e prioriza registros 100% contidos.
  #' @param df_setores_ti Data.frame gerado pela função `mapear_setores_ti()`.
  #' @param codigos_ti Vetor com os IDs das Terras Indígenas desejadas.
  #' @param codigo_cr Código da Unidade Organizacional (CR) a ser atribuída.
  #' @return Data.frame com `cd_setor`, `codigoUnidadeImputado` e `tipo_relacao`.
  atribuir_cr_usando_mapa_ti <- function(df_setores_ti, codigos_ti, codigo_cr) {
    
    df_imputacao <- df_setores_ti |>
      dplyr::filter(id_ti %in% codigos_ti) |>
      dplyr::mutate(codigoUnidadeImputado = as.character(codigo_cr)) |>
      dplyr::arrange(tipo_relacao) # |>
      # Resolve duplicidade interna primeiro
      # dplyr::distinct(cd_setor, .keep_all = TRUE) |>
      # CORREÇÃO: Mantém a flag tipo_relacao para o desempate global do imap_dfr
      # dplyr::select(id_ti, cd_setor, codigoUnidadeImputado, tipo_relacao)
    
    return(df_imputacao)
  }

  
  #' @title Mapear cruzamento de setores censitários com infraestruturas lineares
  #' @description Identifica a intersecção topológica entre setores censitários (polígonos) 
  #' e feições lineares (rodovias, hidrovias, ferrovias), calculando a extensão exata (em km)
  #' da linha que passa por dentro de cada setor.
  #' 
  #' @param sf_setores Objeto sf contendo os polígonos dos setores censitários.
  #' @param sf_linhas Objeto sf contendo as geometrias de linha (ex: arquivos_hidroviario).
  #' @param id_setor String. Nome da coluna identificadora do setor censitário.
  #' @param id_linha String. Nome da coluna identificadora da infraestrutura (ex: "nome_rio", "br").
  #' @return Um data.frame relacionando o setor censitário, a linha e o comprimento da intersecção em km.
  mapear_setores_linhas <- function(sf_setores, sf_linhas, id_setor = "cd_setor", id_linha = "identificador") {
    
    # sf_setores = tar_read(arq_setores_censitarios)
    # sf_linhas = sf::st_read("dados/brutos/arquivos_hidroviario.gpkg", quiet = TRUE)
    # id_setor = "cd_setor"
    # id_linha = "id"
    
    # sf_linhas <- sf::st_make_valid(sf_linhas)
    
    # 2. Projeção para sistema métrico (CRS_ALBERS_BR, ver R/config.R)
    # Mandatório para que st_length() retorne valores em metros precisos, em vez de graus.
    
    
    sf_setores_proj <- sf::st_transform(
      sf_setores,
      CRS_ALBERS_BR
    )

    sf_linhas_proj <- sf::st_transform(
      sf_linhas,
      CRS_ALBERS_BR
    )
    
    sf_setores_proj <- sf::st_make_valid(sf_setores_proj)
    sf_linhas_proj <- sf::st_make_valid(sf_linhas_proj)
    
    # 3. Filtragem Espacial Prévia (Otimização de Performance)
    # Remove as linhas que passam longe dos setores antes de rodar o corte pesado
    sf_linhas_filtradas <- sf::st_filter(sf_linhas_proj, sf_setores_proj)
    
    # 4. Intersecção Topológica (Corta a linha exatamente nas bordas do setor)
    suppressWarnings({
      interseccao <- sf::st_intersection(sf_setores_proj, sf_linhas_filtradas)
    })
    
    # Fail-safe: Se a intersecção for vazia, retorna data.frame com a estrutura correta
    if (nrow(interseccao) == 0) {
      df_vazio <- data.frame(setor = character(), linha = character(), extensao_km = numeric())
      names(df_vazio) <- c(id_setor, id_linha, "extensao_km")
      return(df_vazio)
    }
    
    # 5. Cálculo do Comprimento e Consolidação
    resultado <- interseccao |>
      dplyr::mutate(
        extensao_m = as.numeric(sf::st_length(geom)),
        extensao_km = extensao_m / 1000
      ) |>
      sf::st_drop_geometry() |>
      # Agrupa pois uma mesma rodovia pode ser "fatiada" em pedaços múltiplos dentro do mesmo setor
      dplyr::group_by(.data[[id_setor]], .data[[id_linha]]) |>
      dplyr::summarise(
        extensao_km = sum(extensao_km, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(extensao_km > 0)
    
    return(resultado)
  }
  
