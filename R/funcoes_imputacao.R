# ==============================================================================
# Regras de imputação (atribuição manual de território a uma CR)
# ==============================================================================
# Todas as decisões do tipo "este território fica com aquela CR" ficam em
# dados/imputacoes/regras_imputacao.csv (ver README da pasta), uma linha por regra.
# As funções abaixo leem essa tabela e a aplicam sobre os setores censitários:
#
#   ler_regras_imputacao()      lê e valida a tabela
#   aplicar_regras_imputacao()  aplica as regras de uma etapa (v2, msol), respeitando a
#                               precedência dada pelas colunas prioridade e ordem
#   imputar_por_territorio()    regras dos tipos uf, municipio e setor
#   imputar_por_ti()            regras do tipo ti
#   imputar_por_hidrovia()      regras do tipo hidrovia
#
# As regras do tipo "calculado" não têm a atribuição na tabela: são calculadas no
# código (melhor rota, realocação) e entram na consolidação na prioridade indicada.

TIPOS_REGRA_IMPUTACAO <- c(
  uf = "territorio", municipio = "territorio", setor = "territorio",
  ti = "ti", hidrovia = "hidrovia",
  arquivo_setores = "arquivo_setores", calculado = "calculado"
)

#' @title Lê e valida a tabela de regras de imputação
#' @param arquivo Caminho de `regras_imputacao.csv`.
#' @param dicionario_cr Dicionário de CRs (alvo `dicionario_cr`), usado para conferir se
#'   `sigla_destino` corresponde a `cd_uorg_destino`.
#' @return tibble com todas as colunas como character, exceto `prioridade` e `ordem` (integer).
ler_regras_imputacao <- function(arquivo, dicionario_cr) {
  regras <- data.table::fread(arquivo, sep = ";", colClasses = "character", encoding = "UTF-8") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ dplyr::coalesce(trimws(.x), "")),
      prioridade = as.integer(prioridade),
      ordem = as.integer(ordem)
    )

  tipos_invalidos <- setdiff(regras$tipo, names(TIPOS_REGRA_IMPUTACAO))
  if (length(tipos_invalidos) > 0) {
    stop("Tipos de regra desconhecidos em ", arquivo, ": ", paste(tipos_invalidos, collapse = ", "))
  }
  if (anyNA(regras$prioridade) || anyNA(regras$ordem)) {
    stop("As colunas prioridade e ordem de ", arquivo, " devem ser números inteiros em todas as linhas")
  }
  sem_destino <- regras$cd_uorg_destino == "" & !regras$tipo %in% c("arquivo_setores", "calculado")
  if (any(sem_destino)) {
    stop("Regras sem cd_uorg_destino em ", arquivo, " (linhas ", paste(which(sem_destino) + 1, collapse = ", "), ")")
  }

  # sigla_destino é só para leitura humana: avisa (sem interromper) se divergir do dicionário
  siglas <- dicionario_cr |>
    dplyr::transmute(cd_uorg_destino = as.character(codigo_siorg), sigla_dicionario = trimws(sigla))
  divergentes <- regras |>
    dplyr::filter(cd_uorg_destino != "") |>
    dplyr::left_join(siglas, by = "cd_uorg_destino") |>
    dplyr::filter(is.na(sigla_dicionario) | sigla_dicionario != sigla_destino) |>
    dplyr::distinct(cd_uorg_destino, sigla_destino, sigla_dicionario)
  if (nrow(divergentes) > 0) {
    warning(
      "sigla_destino diverge do dicionário de CRs em ", arquivo, ":\n",
      paste(utils::capture.output(print(divergentes)), collapse = "\n")
    )
  }

  regras
}

#' @title Lê as imputações manuais por setor censitário
#' @param arquivo Caminho de `imputacoes_setor.csv` (colunas cd_setor, cd_uorg_destino, lote).
#' @return tibble com `cd_setor` e `codigoUnidadeImputado` (character), um registro por setor.
ler_imputacoes_setor <- function(arquivo) {
  data.table::fread(arquivo, sep = ";", colClasses = "character", encoding = "UTF-8") |>
    tibble::as_tibble() |>
    dplyr::transmute(cd_setor, codigoUnidadeImputado = cd_uorg_destino) |>
    dplyr::distinct(cd_setor, .keep_all = TRUE)
}

#' @title Indica se a CR atual de cada linha atende às condições da regra
#' @description Vetorizado sobre as três entradas. `somente` e `exceto` são listas de
#'   códigos de CR separadas por vírgula ("" = sem condição). Setor sem CR atual (NA) só
#'   atende a regras sem condição de CR.
atende_condicao_cr <- function(cd_uorg, somente, exceto) {
  na_lista <- function(lista) {
    stringr::str_detect(paste0(",", lista, ","), stringr::fixed(paste0(",", cd_uorg, ",")))
  }
  (somente == "" | (!is.na(cd_uorg) & na_lista(somente))) &
    (exceto == "" | (!is.na(cd_uorg) & !na_lista(exceto)))
}

#' @title Aplica regras dos tipos uf, municipio e setor
#' @description Para cada setor, vale a primeira regra (menor `ordem`) cujo território e
#'   condição de CR atual casam com ele. A saída segue a ordem dos setores em `setores`.
#' @param regras Regras de uma mesma prioridade.
#' @param setores data.frame com `cd_setor`, `cd_uorg` e as colunas usadas pelas regras
#'   (`cd_uf` para uf, `cd_mun` para municipio).
imputar_por_territorio <- function(regras, setores) {
  coluna_por_tipo <- c(uf = "cd_uf", municipio = "cd_mun", setor = "cd_setor")

  setores <- setores |>
    dplyr::select(dplyr::any_of(c("cd_setor", "cd_uf", "cd_mun", "cd_uorg"))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character), .linha = dplyr::row_number())

  casamentos <- lapply(split(regras, regras$tipo), function(regras_tipo) {
    coluna <- coluna_por_tipo[[regras_tipo$tipo[1]]]
    setores |>
      dplyr::mutate(.codigo = .data[[coluna]]) |>
      dplyr::inner_join(regras_tipo, by = c(".codigo" = "codigo"), relationship = "many-to-many")
  })

  dplyr::bind_rows(casamentos) |>
    dplyr::filter(atende_condicao_cr(cd_uorg, somente_se_cr_atual, exceto_se_cr_atual)) |>
    dplyr::arrange(.linha, ordem) |>
    dplyr::distinct(cd_setor, .keep_all = TRUE) |>
    dplyr::transmute(cd_setor, codigoUnidadeImputado = cd_uorg_destino)
}

#' @title Aplica regras do tipo ti
#' @description Atribui à CR de destino os setores associados a cada TI em `df_setores_em_ti`.
#'   Quando um setor toca mais de uma TI, prevalece a relação mais forte (`tipo_relacao`) e,
#'   em empate, a regra de menor `ordem`. Regras com a mesma `ordem` formam um grupo.
#' @return tibble com as colunas de `df_setores_em_ti` mais `codigoUnidadeImputado` (as
#'   variáveis da TI são usadas no anexo da nota técnica, ver alvo `imputs_ti_v2`).
imputar_por_ti <- function(regras, df_setores_em_ti) {
  regras_ti <- regras |>
    dplyr::transmute(id_ti = as.integer(codigo), .ordem = ordem, codigoUnidadeImputado = cd_uorg_destino)

  df_setores_em_ti |>
    dplyr::inner_join(regras_ti, by = "id_ti", relationship = "many-to-many") |>
    dplyr::arrange(tipo_relacao, .ordem) |>
    dplyr::distinct(cd_setor, .keep_all = TRUE) |>
    dplyr::select(-.ordem) |>
    tibble::as_tibble()
}

#' @title Aplica regras do tipo hidrovia
#' @description Atribui à CR de destino os setores cortados pela hidrovia `codigo` (coluna
#'   `id` de `mapeamento_hidrovia`) que atendem à condição de CR atual da regra.
imputar_por_hidrovia <- function(regras, mapeamento_hidrovia, setores) {
  setores <- setores |>
    dplyr::transmute(cd_setor = as.character(cd_setor), cd_uorg = as.character(cd_uorg))
  mapeamento_hidrovia <- mapeamento_hidrovia |>
    dplyr::mutate(cd_setor = as.character(cd_setor))

  casamentos <- lapply(seq_len(nrow(regras)), function(i) {
    regra <- regras[i, ]
    alvo <- setores$cd_setor[atende_condicao_cr(setores$cd_uorg, regra$somente_se_cr_atual, regra$exceto_se_cr_atual)]
    mapeamento_hidrovia |>
      dplyr::filter(id == regra$codigo, cd_setor %in% alvo) |>
      dplyr::transmute(cd_setor, codigoUnidadeImputado = regra$cd_uorg_destino, .ordem = regra$ordem)
  })

  dplyr::bind_rows(casamentos) |>
    dplyr::arrange(.ordem) |>
    dplyr::distinct(cd_setor, .keep_all = TRUE) |>
    dplyr::select(-.ordem)
}

#' @title Aplica as regras de imputação de uma etapa
#' @description Calcula a imputação de cada prioridade da tabela e consolida: quando um setor
#'   aparece em mais de uma prioridade, vale a de menor número. Todas as regras de uma mesma
#'   prioridade devem ser da mesma família (territorial, ti, hidrovia, arquivo_setores ou
#'   calculado).
#' @param regras Regras de uma etapa (ex.: `regras_imputacao |> dplyr::filter(etapa == "v2")`).
#' @param setores data.frame de setores com a CR atual (necessário para regras territoriais e
#'   de hidrovia).
#' @param df_setores_em_ti Associação setor -> TI (alvo `df_setores_em_ti`), para regras ti.
#' @param mapeamento_hidrovia Associação setor -> hidrovia (alvo `mapeamento_setores_hidrovia`).
#' @param imputacoes_setor Saída de `ler_imputacoes_setor()`, para a regra arquivo_setores.
#' @param calculados Lista nomeada de data.frames (`cd_setor`, `codigoUnidadeImputado`), um para
#'   cada regra do tipo calculado, com o nome igual à coluna `codigo` da regra.
#' @return tibble com `cd_setor` e `codigoUnidadeImputado` (character), um registro por setor.
aplicar_regras_imputacao <- function(
    regras,
    setores = NULL,
    df_setores_em_ti = NULL,
    mapeamento_hidrovia = NULL,
    imputacoes_setor = NULL,
    calculados = list()
) {
  nomes_calculados <- regras$codigo[regras$tipo == "calculado"]
  if (!setequal(nomes_calculados, names(calculados))) {
    stop(
      "As regras do tipo calculado (", paste(nomes_calculados, collapse = ", "),
      ") não correspondem aos resultados fornecidos (", paste(names(calculados), collapse = ", "), ")"
    )
  }

  camadas <- lapply(split(regras, regras$prioridade), function(regras_prioridade) {
    familia <- unique(TIPOS_REGRA_IMPUTACAO[regras_prioridade$tipo])
    if (length(familia) != 1) {
      stop("A prioridade ", regras_prioridade$prioridade[1], " mistura famílias de regra: ", paste(familia, collapse = ", "))
    }

    imputacao <- switch(
      familia,
      territorio = imputar_por_territorio(regras_prioridade, setores),
      ti = imputar_por_ti(regras_prioridade, df_setores_em_ti),
      hidrovia = imputar_por_hidrovia(regras_prioridade, mapeamento_hidrovia, setores),
      arquivo_setores = imputacoes_setor,
      calculado = dplyr::bind_rows(calculados[regras_prioridade$codigo])
    )

    imputacao |>
      dplyr::transmute(
        cd_setor = as.character(cd_setor),
        codigoUnidadeImputado = as.character(codigoUnidadeImputado),
        origem = paste0(regras_prioridade$prioridade[1], ". ", paste(unique(regras_prioridade$tipo), collapse = "/"))
      )
  })

  consolidado <- dplyr::bind_rows(camadas) |>
    dplyr::distinct(cd_setor, .keep_all = TRUE)

  contagem_origem <- consolidado |> dplyr::count(origem, name = "n_setores")
  message("Imputações consolidadas por prioridade de origem:")
  message(paste(utils::capture.output(print(contagem_origem)), collapse = "\n"))

  consolidado |> dplyr::select(cd_setor, codigoUnidadeImputado)
}

#' @title Divide os setores de uma UF entre CRs candidatas pela melhor rota
#' @description Usada pela regra calculada `piaui_melhor_rota`: para cada setor do Piauí, a
#'   CR candidata com melhor posição (`ordem`) na tabela consolidada do OSRM.
gerar_imputs_melhor_rota_uf <- function(tabela_osrm, uf, crs_candidatas) {
  arrow::open_dataset(tabela_osrm) |>
    dplyr::filter(nm_uf == !!uf & cd_uorg %in% !!crs_candidatas) |>
    dplyr::collect() |>
    dplyr::arrange(ordem) |>
    dplyr::transmute(cd_setor = as.character(cd_setor), codigoUnidadeImputado = as.character(cd_uorg)) |>
    dplyr::distinct(cd_setor, .keep_all = TRUE)
}

#' @title Realoca os setores de uma CR para a melhor rota entre as demais CRs
#' @description Usada pela regra calculada `realocacao_msol`: os setores hoje atribuídos a
#'   `cd_uorg_origem` vão para a CR de melhor posição (`ordem`) entre as outras, segundo as
#'   combinações do roteamento multimodal do Norte (`df_imputacao_norte$combinacoes`).
gerar_imputs_realocacao_cr <- function(setores, combinacoes, cd_uorg_origem) {
  setores_origem <- setores |>
    dplyr::filter(cd_uorg == cd_uorg_origem) |>
    dplyr::pull(cd_setor) |>
    as.character()

  combinacoes |>
    dplyr::mutate(cd_setor = as.character(cd_setor)) |>
    dplyr::filter(cd_setor %in% setores_origem) |>
    dplyr::collect() |>
    dplyr::filter(cd_uorg != cd_uorg_origem) |>
    dplyr::arrange(ordem) |>
    dplyr::distinct(cd_setor, .keep_all = TRUE) |>
    dplyr::transmute(cd_setor, codigoUnidadeImputado = as.character(cd_uorg))
}

# Extraído do corpo do target `sf_setores_realocados_v2` em _targets.R (2.14).
# Identifica os setores censitários que precisam ser reprocessados/realocados em
# decorrência dos ajustes manuais aplicados na V2 (ver comentários AJUSTE 1-11 abaixo).
identificar_setores_realocados_v2 <- function(arquivo_gpkg_imputado_final) {

  cd_uf_pernambuco <- 26
  cd_unidade_pernambuco <- 509512

  cd_uf_sc <- 42
  cd_unidade_ints <- 87791

  cd_uf_pr <- 41
  cd_unidade_gpv <- 230279

  cd_unidade_xin <- 87956

  dados_imputados <- sf::st_read(arquivo_gpkg_imputado_final, quiet = TRUE)
  dados_imputados$id_destino <- seq_len(nrow(dados_imputados))

  # OBTENDO SETORES QUE IRÃO PASSAR POR REALOCAÇÃO EM DECORRÊNCIA DAS IMPUTAÇÕES REALIZADAS PARA A V2

  dados_imputados |>
    sf::st_transform(4326) |>
    dplyr::filter(
      #' AJUSTE 1: CR de Recife será responsável por todo o estado de Pernambuco e nada além de Pernambuco.
      #' Consequência: setores atribuídos a recife fora de Pernambuco deverão ser reprocessados.

      (cd_uorg == cd_unidade_pernambuco & cd_uf != cd_uf_pernambuco) |

      #' AJUSTE 2: As Terras Indígenas de Trombetas e Mapuera serão atribuídas à CR de Manaus
      #' Consequência: Não é necessário a realocação de nenhum setor a partir do critério, uma vez
      #' que será utilizada imputação manual dos setores para tanto.

      #' AJUSTE 3: Todas os os setores censitários atribuídos a CR Interior Sul e que estão no RS serão
      #' atribuídas a CR de Passo Fundo.

      #' AJUSTE 4: A CR do Interior Sul deverá englobar todo o estado de Santa Catarina, exceto a parte
      #' atribuída a CR Litoral Sul
      #' Consequência: Não é necessário fazer nada, exceto se indicado que a CR do Interior Sul deverá
      #' atender APENAS ao estado de Santa Catarina, ocasião em que o código abaixo poderá ser utilizado:

      # (cd_uorg == cd_unidade_ints & cd_uf != cd_uf_sc) |

      #' AJUSTE 5: Atribuir toda faixa do litoral sul onde estão as TI's da etnia Guarani
      #' a CR Litoral Sul.
      #' Consequência: A princípio não será necessário nenhuma realocação de setores.

      #' AJUSTE 6: Paraíba pode ser dividida entre múltiplas CR's:
      #' Consequência: Existe uma parte da Paraíba que corresponde a área de atuação da CR
      #' do Baixo São Francisco que provavelmente deverá ser realocada em decorrência da imputação

      #' AJUSTE 7: A CR de Guarapuava atenderá apenas o estado do Paraná.
      #' Consequência: Reatribuição dos setores censitários atribuídos a CR Guarapuava
      #' que não estão no estado do Paraná

      (cd_uorg == cd_unidade_gpv & cd_uf != cd_uf_pr) |

      #' AJUSTE 8: Minas Gerais inteira irá ficar com a CR de MG/ES
      #' Consequência: Setores censitarios de MG-ES fora de MG ou ES - realocados
      # do estado de Minas
      (cd_uorg == 87621 & !(cd_uf %in% c(31,32))) |

      #' AJUSTE 9: Xingu irá ficar somente com os setores censitários correspondetes às terras indígenas
      #' do Parque do Xingu e Jarina, e a parte cortada do parque do Xingu irá será realocado.
      (cd_uorg == cd_unidade_xin) |

      #' AJUSTE 11: Ilha do Bananal inteira será atribuída à CR de Araguaia Tocantis.

      # AJUSTE PÓS REUNIÃO DO DIA 29/06/2026 - REALOCAÇÃO DOS SETORES DA CR-SBA NO PIAUI
      (cd_uorg == 88104 & cd_uf == 22)
    )
}

# Extraído do corpo do target `df_imputacao_mun` em _targets.R (2.23).
# Atribui cada município (cd_mun) à CR (cd_uorg) responsável pela maior parte do
# critério (população indígena, quando existente no município; área, caso contrário),
# gerando a imputação a nível de setor censitário para os municípios compartilhados
# entre múltiplas CRs.
gerar_df_imputacao_mun <- function(arquivo_gpkg_suavizado_v2_msol, censo_agregado_sc_pi) {

  df <- sf::st_read(
    arquivo_gpkg_suavizado_v2_msol,
    query = glue::glue("SELECT
                   cd_setor,
                   situacao,
                   area_km2,
                   nm_regiao,
                   cd_mun,
                   nm_mun,
                   menor_duracao,
                   menor_distancia,
                   distancia_reta,
                   criterio,
                   cd_uorg,
                   nome_uorg,
                   sigla,
                   etapa
                   FROM setores_cr_suavizado_v2_msol"),
    geometry_column = NULL
    )

  suppressWarnings(
    df_pi <- censo_agregado_sc_pi |>
      sf::st_drop_geometry() |>
      dplyr::mutate(total_pi = as.integer(gsub('X', '1', v01690)), setor_indigena = TRUE) |>
      dplyr::select(cd_setor, total_pi, setor_indigena)
  )

  df <- df |> dplyr::left_join(df_pi, by = dplyr::join_by(cd_setor))

  # TABELA DE MUNICIPIOS E NUMERO DE MUNICÍPIOS COMPARTILHADOS
  tb <- df |> dplyr::group_by(cd_mun) |>
    dplyr::mutate(
      n_uorgs = dplyr::n_distinct(cd_uorg)
    ) |> dplyr::ungroup() |>
    dplyr::group_by(cd_uorg, nome_uorg, sigla) |>
    dplyr::summarise(
      n_mun_n_compartilhados = sum(n_uorgs == 1),
      n_mun_compartilhados = max(n_uorgs) - 1,
      n_mun = dplyr::n_distinct(cd_mun),

    )
  # PREPARA IMPUTAÇÃO DOS COMPARTILHADOS ----
  compartilhados <- df |> dplyr::group_by(cd_mun) |>
    dplyr::mutate(
      n_uorgs = dplyr::n_distinct(cd_uorg),
      area_mun = sum(area_km2, na.rm = T),
      total_pi_mun = sum(total_pi, na.rm = T)
    ) |> dplyr::ungroup() |>
    # filter(n_uorgs > 1) |>
    dplyr::group_by(cd_uorg, nome_uorg, sigla, cd_mun, nm_mun) |>
    dplyr::summarise(
      n_uorgs = max(n_uorgs),
      area_n_compartilhada = sum(area_km2, na.rm = T),
      area_setores_indigenas = sum(area_km2*setor_indigena, na.rm = T),
      area_total_mun = max(area_mun),
      total_pi_mun = max(total_pi_mun),
      total_pi = sum(total_pi, na.rm = T),
      pct_area_compartilhada = area_n_compartilhada*100 / area_total_mun,
      pct_pi = total_pi*100 / total_pi_mun,
      pi_por_area = total_pi*area_n_compartilhada,
      .groups = 'drop'
    ) |>
    dplyr::group_by(cd_mun) |>
    dplyr::mutate(
      criterio = dplyr::if_else(total_pi_mun > 0, pi_por_area, area_n_compartilhada)
    ) |> dplyr::ungroup()


  to_imput <- compartilhados |> dplyr::group_by(cd_mun) |>
    dplyr::filter(
      criterio == max(criterio)
    ) |> dplyr::select(cd_mun_imput = cd_mun, codigoUnidadeImputado = cd_uorg) |> dplyr::distinct()

  imput_municipio <- df |> dplyr::left_join(
    to_imput,
    by = dplyr::join_by(cd_mun == cd_mun_imput)
  ) |>
    dplyr::filter(!is.na(codigoUnidadeImputado)) |>
    dplyr::select(cd_setor, codigoUnidadeImputado)

  imput_municipio
}

