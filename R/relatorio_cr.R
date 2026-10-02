# ==============================================================================
# Relatório PDF padronizado por Coordenação Regional (CR)
# ==============================================================================
# Monta, para cada CR da malha básica, um conjunto padronizado de indicadores
# (Potencialidades, população/pirâmide etária SIASI, número de aldeias por sistema,
# localidades indígenas, etnias) e os rankeia entre as CRs, para alimentar o template
# `relatorio_cr.qmd` (um PDF por CR).

#' @title Normaliza texto para comparação de nomes entre bases (maiúsculas, sem acento)
#' @description Usado para casar nomes de Terra Indígena entre fontes que não compartilham um
#' código comum (ex.: `Código TERRA INDÍGENA` do SIASI e `cd_ti` da camada IBGE de Localidades
#' Indígenas usam espaços de código próprios, diferentes do `terrai_codigo` da Funai -- conferido
#' diretamente nos dados: SIASI bate por código em 0 dos casos testados, por nome normalizado em
#' ~94%; Localidades Indígenas bate por código em ~1%, por nome normalizado em ~99%).
#' @param x Vetor de texto.
#' @return Vetor de texto em maiúsculas, sem acento, sem espaços nas pontas.
normalizar_nome_ti <- function(x) {
  trimws(stringi::stri_trans_general(toupper(x), "Latin-ASCII"))
}

#' @title Atribui a CR (cd_uorg) responsável a cada linha de um data.frame, via malha básica
#' @description Réplica, para fontes de dados externas à malha básica (SIASI, aldeias, localidades
#' indígenas), da mesma regra de precedência já usada em `calcular_malha_basica_bruta()` e
#' `tabela_dados_malha_basica()`: a Terra Indígena tem precedência sobre o Município -- só recorre
#' ao município quando a linha não é atribuível a nenhuma TI. Tenta, nesta ordem: (1) código de TI
#' (`coluna_ti_codigo`), quando fornecida e o código bate com `cd_territorio` da malha básica; (2)
#' nome de TI normalizado (`coluna_ti_nome`), quando o código não bateu ou não foi fornecido; (3)
#' código de município (`coluna_municipio`), como fallback final. Faz mais sentido usar nome do que
#' código para TI sempre que a fonte não é a própria Funai (ver `normalizar_nome_ti()`).
#' @param df Data.frame de origem (SIASI, aldeias, localidades indígenas, ...).
#' @param malha_basica Objeto sf/data.frame no formato de `malha_basica_bruta` (colunas
#'   `cd_territorio`, `tipo_territorio`, `nm_territorio`, `cd_uorg`, `nome_uorg`, `sigla`).
#' @param coluna_ti_codigo Nome da coluna em `df` com o código de TI (ou `NULL` para não tentar).
#' @param coluna_ti_nome Nome da coluna em `df` com o nome de TI (ou `NULL` para não tentar).
#' @param coluna_municipio Nome da coluna em `df` com o código de município IBGE (7 dígitos, ou
#'   `NULL` para não tentar).
#' @return `df` com as colunas adicionais `cd_uorg`, `nome_uorg`, `sigla`, `metodo_atribuicao_cr`
#'   (`"ti_codigo"`, `"ti_nome"`, `"municipio"` ou `NA` quando nenhuma fonte resolveu).
atribuir_cr_via_malha_basica <- function(
    df,
    malha_basica,
    coluna_ti_codigo = NULL,
    coluna_ti_nome = NULL,
    coluna_municipio = NULL
) {
  if (inherits(malha_basica, "sf")) malha_basica <- sf::st_drop_geometry(malha_basica)

  tis_mb <- malha_basica |>
    dplyr::filter(tipo_territorio == "Terra Indígena") |>
    dplyr::transmute(
      cd_territorio = as.character(cd_territorio),
      nome_norm = normalizar_nome_ti(nm_territorio),
      cd_uorg, nome_uorg, sigla
    )

  mun_mb <- malha_basica |>
    dplyr::filter(tipo_territorio == "Município") |>
    dplyr::transmute(cd_territorio = as.character(cd_territorio), cd_uorg, nome_uorg, sigla)

  df <- df |> dplyr::mutate(.linha_id = dplyr::row_number())
  resolvidas <- vector("list", 0)
  pendentes <- df

  if (!is.null(coluna_ti_codigo) && nrow(pendentes) > 0) {
    m <- pendentes |>
      dplyr::mutate(cd_territorio = as.character(.data[[coluna_ti_codigo]])) |>
      dplyr::inner_join(tis_mb |> dplyr::distinct(cd_territorio, .keep_all = TRUE), by = "cd_territorio") |>
      dplyr::mutate(metodo_atribuicao_cr = "ti_codigo") |>
      dplyr::select(-cd_territorio, -nome_norm)
    resolvidas[[length(resolvidas) + 1]] <- m
    pendentes <- pendentes |> dplyr::filter(!.linha_id %in% m$.linha_id)
  }

  if (!is.null(coluna_ti_nome) && nrow(pendentes) > 0) {
    m <- pendentes |>
      dplyr::mutate(nome_norm = normalizar_nome_ti(.data[[coluna_ti_nome]])) |>
      dplyr::inner_join(tis_mb |> dplyr::distinct(nome_norm, .keep_all = TRUE), by = "nome_norm") |>
      dplyr::mutate(metodo_atribuicao_cr = "ti_nome") |>
      dplyr::select(-cd_territorio, -nome_norm)
    resolvidas[[length(resolvidas) + 1]] <- m
    pendentes <- pendentes |> dplyr::filter(!.linha_id %in% m$.linha_id)
  }

  if (!is.null(coluna_municipio) && nrow(pendentes) > 0) {
    m <- pendentes |>
      dplyr::mutate(cd_territorio = as.character(.data[[coluna_municipio]])) |>
      dplyr::inner_join(mun_mb, by = "cd_territorio") |>
      dplyr::mutate(metodo_atribuicao_cr = "municipio") |>
      dplyr::select(-cd_territorio)
    resolvidas[[length(resolvidas) + 1]] <- m
    pendentes <- pendentes |> dplyr::filter(!.linha_id %in% m$.linha_id)
  }

  pendentes <- pendentes |>
    dplyr::mutate(cd_uorg = NA_character_, nome_uorg = NA_character_, sigla = NA_character_, metodo_atribuicao_cr = NA_character_)

  dplyr::bind_rows(resolvidas, list(pendentes)) |>
    dplyr::arrange(.linha_id) |>
    dplyr::select(-.linha_id)
}

#' @title Importa os extratos brutos do SIASI preservando TI, aldeia, sexo e faixa etária
#' @description Substitui, para fins do relatório por CR, a agregação nacional já feita por
#' `importar_siasi_serie_historica()` (que descarta TI/aldeia/sexo/faixa etária, mantendo só
#' `ano`/`populacao_indigena` nacional) -- aqui preservamos as colunas necessárias para atribuir
#' população por CR e montar a pirâmide etária. Não substitui nem altera
#' `importar_siasi_serie_historica()`, que continua alimentando a série histórica nacional já
#' usada na nota técnica.
#' @param dir_siasi Diretório com os extratos anuais do SIASI (ver `DIR_SIASI` em `R/config.R`).
#' @return Tibble com `ti_nome`, `cd_municipio_ibge` (6 dígitos, sem DV, convenção do SIASI),
#'   `cd_aldeia_siasi`, `aldeia_nome`, `sexo`, `faixa_etaria`, `ano`, `populacao`.
importar_siasi_detalhado <- function(dir_siasi = DIR_SIASI) {

  arquivos <- list.files(dir_siasi, pattern = "\\.xlsx$", full.names = TRUE)

  if (length(arquivos) == 0) {
    stop("Nenhum extrato do SIASI encontrado em ", dir_siasi)
  }

  purrr::map_dfr(arquivos, function(arquivo) {
    readxl::read_excel(arquivo, col_types = "text") |>
      dplyr::transmute(
        ti_nome = `TERRA INDÍGENA`,
        cd_municipio_ibge = `Código MUNÍCIPIO IBGE`,
        cd_aldeia_siasi = `Código ALDEIA`,
        aldeia_nome = ALDEIA,
        sexo = SEXO,
        faixa_etaria = `FAIXA ETÁRIA`,
        ano = as.integer(ANO),
        populacao = as.numeric(`POPULAÇÃO INDÍGENA`)
      )
  })
}

#' @title Monta a tabela de dados do relatório por CR (uma linha por `cd_uorg`, com colunas-lista)
#' @description Ponto de entrada único do relatório por CR: recebe a malha básica (ou qualquer
#' arquivo no mesmo formato -- `cd_territorio`, `tipo_territorio`, `nm_territorio`, `cd_uorg`,
#' `nome_uorg`, `sigla`) e devolve uma linha por CR combinando os indicadores de Potencialidades já
#' calculados (`tabela_potencialidades`) com população/pirâmide etária do SIASI, número de aldeias
#' (Sisaldeias-Funai e SIASI), localidades indígenas (IBGE) e a lista de etnias/línguas -- e a
#' posição (ranking) de cada CR entre as demais nos indicadores-chave.
#' @param malha_basica Objeto sf ou data.frame no formato de `malha_basica_bruta` -- é este
#'   parâmetro que torna o relatório reproduzível a partir de qualquer malha no mesmo formato.
#' @param tabela_potencialidades Data.frame de `tabela_dados_final_malha_basica` (uma linha por
#'   `cd_uorg`, indicadores já calculados).
#' @param df_siasi_detalhado Resultado de `importar_siasi_detalhado()`.
#' @param arq_aldeias Objeto sf do alvo `arq_aldeias` (Sisaldeias-Funai, WFS "aldeias_pontos").
#' @param arq_localidades_indigenas Objeto sf do alvo `arq_localidades_indigenas` (IBGE 2022).
#' @param arq_tis Objeto sf com as TIs (alvo `arq_tis`), usado só para o mapa de destaque.
#' @param malha_basica_dissolvida Objeto sf de `malha_basica_dissolvida` (um polígono por CR),
#'   usado para o mapa de destaque no relatório.
#' @param arq_etnias,arq_linguas Data.frames dos alvos homônimos (Censo 2022, por município).
#' @return Tibble com uma linha por `cd_uorg` e colunas-lista `piramide_etaria`, `etnias`,
#'   `linguas`, além dos indicadores escalares e das colunas `*_rank`/`*_pct` de comparação.
montar_dados_relatorio_cr <- function(
    malha_basica,
    tabela_potencialidades,
    df_siasi_detalhado,
    arq_aldeias,
    arq_localidades_indigenas,
    arq_etnias,
    arq_linguas
) {

  if (is.character(malha_basica)) malha_basica <- sf::st_read(malha_basica, quiet = TRUE)
  malha_basica_attr <- if (inherits(malha_basica, "sf")) sf::st_drop_geometry(malha_basica) else malha_basica

  base_cr <- tabela_potencialidades |>
    sf::st_drop_geometry() |>
    dplyr::mutate(cd_uorg = as.character(cd_uorg)) |>
    dplyr::select(cd_uorg, nome_uorg, sigla, total_pi, total_pi_urbana, total_pi_rural,
                  total_area_km2, n_focos, n_focos_em_ti,
                  dplyr::starts_with("autorizado_por_pi_"))

  # --- População e pirâmide etária (SIASI), atribuídas por nome de TI ---------------------
  siasi_cr <- atribuir_cr_via_malha_basica(
    df_siasi_detalhado, malha_basica_attr, coluna_ti_nome = "ti_nome"
  ) |>
    dplyr::filter(!is.na(cd_uorg))

  # `siasi_cr` acumula um extrato por ano (histórico); somar população sem filtrar por ano
  # infla o total ao contar a mesma pessoa em vários anos -- usar só o extrato mais recente,
  # o mesmo corte já aplicado à pirâmide etária.
  ano_mais_recente <- suppressWarnings(max(siasi_cr$ano, na.rm = TRUE))
  siasi_cr_ano_atual <- siasi_cr |> dplyr::filter(ano == ano_mais_recente)

  pop_siasi_cr <- siasi_cr_ano_atual |>
    dplyr::group_by(cd_uorg) |>
    dplyr::summarise(total_pi_siasi = sum(populacao, na.rm = TRUE), .groups = "drop")

  piramide_cr <- siasi_cr_ano_atual |>
    dplyr::group_by(cd_uorg, sexo, faixa_etaria) |>
    dplyr::summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(cd_uorg) |>
    tidyr::nest(piramide_etaria = c(sexo, faixa_etaria, populacao)) |>
    dplyr::ungroup()

  n_aldeias_siasi_cr <- siasi_cr |>
    dplyr::filter(!is.na(cd_aldeia_siasi)) |>
    dplyr::group_by(cd_uorg) |>
    dplyr::summarise(n_aldeias_siasi = dplyr::n_distinct(cd_aldeia_siasi), .groups = "drop")

  # --- Aldeias (Sisaldeias-Funai): geoserver Funai "aldeias_pontos", código de TI já bate com
  # terrai_codigo diretamente (~94% dos casos testados), com fallback por município -----------
  if (inherits(arq_aldeias, "sf")) arq_aldeias <- sf::st_drop_geometry(arq_aldeias)
  aldeias_cr <- atribuir_cr_via_malha_basica(
    arq_aldeias, malha_basica_attr,
    coluna_ti_codigo = "cod_ti", coluna_municipio = "cod_municipio"
  ) |>
    dplyr::filter(!is.na(cd_uorg), flag_ativo == "A")

  n_aldeias_funai_cr <- aldeias_cr |>
    dplyr::group_by(cd_uorg) |>
    dplyr::summarise(n_aldeias_sisaldeias = dplyr::n_distinct(cod_aldeia), .groups = "drop")

  # --- Localidades indígenas (IBGE 2022): cd_ti não bate com terrai_codigo (~1% dos casos) --
  # atribuição por nome de TI normalizado, com fallback por município ------------------------
  if (inherits(arq_localidades_indigenas, "sf")) arq_localidades_indigenas <- sf::st_drop_geometry(arq_localidades_indigenas)
  localidades_cr <- atribuir_cr_via_malha_basica(
    arq_localidades_indigenas, malha_basica_attr,
    coluna_ti_nome = "nm_ti", coluna_municipio = "cd_munic"
  ) |>
    dplyr::filter(!is.na(cd_uorg))

  n_localidades_cr <- localidades_cr |>
    dplyr::group_by(cd_uorg) |>
    dplyr::summarise(n_localidades_indigenas = dplyr::n(), .groups = "drop")

  # --- Etnias e línguas (Censo 2022, por município -> CR via malha básica) ------------------
  mun_mb <- malha_basica_attr |>
    dplyr::filter(tipo_territorio == "Município") |>
    dplyr::transmute(cd_mun = as.character(cd_territorio), cd_uorg, nome_uorg, sigla)

  etnias_cr <- arq_etnias |>
    dplyr::mutate(cd_mun = as.character(cd_mun)) |>
    dplyr::inner_join(mun_mb |> dplyr::select(cd_mun, cd_uorg), by = "cd_mun") |>
    dplyr::group_by(cd_uorg, nm_etnia) |>
    dplyr::summarise(qt_pessoas = sum(qtd, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(qt_pessoas > 0) |>
    dplyr::arrange(cd_uorg, dplyr::desc(qt_pessoas)) |>
    dplyr::group_by(cd_uorg) |>
    tidyr::nest(etnias = c(nm_etnia, qt_pessoas)) |>
    dplyr::ungroup()

  linguas_cr <- arq_linguas |>
    dplyr::mutate(cd_mun = as.character(cd_mun)) |>
    dplyr::inner_join(mun_mb |> dplyr::select(cd_mun, cd_uorg), by = "cd_mun") |>
    dplyr::group_by(cd_uorg, nm_lingua) |>
    dplyr::summarise(qt_pessoas = sum(qtd, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(qt_pessoas > 0) |>
    dplyr::arrange(cd_uorg, dplyr::desc(qt_pessoas)) |>
    dplyr::group_by(cd_uorg) |>
    tidyr::nest(linguas = c(nm_lingua, qt_pessoas)) |>
    dplyr::ungroup()

  # --- Consolidação ---------------------------------------------------------------------
  dados <- base_cr |>
    dplyr::left_join(pop_siasi_cr, by = "cd_uorg") |>
    dplyr::left_join(piramide_cr, by = "cd_uorg") |>
    dplyr::left_join(n_aldeias_siasi_cr, by = "cd_uorg") |>
    dplyr::left_join(n_aldeias_funai_cr, by = "cd_uorg") |>
    dplyr::left_join(n_localidades_cr, by = "cd_uorg") |>
    dplyr::left_join(etnias_cr, by = "cd_uorg") |>
    dplyr::left_join(linguas_cr, by = "cd_uorg") |>
    dplyr::mutate(
      dplyr::across(
        c(total_pi_siasi, n_aldeias_siasi, n_aldeias_sisaldeias, n_localidades_indigenas),
        ~ dplyr::coalesce(.x, 0)
      ),
      n_etnias = purrr::map_int(etnias, ~ if (is.null(.x)) 0L else nrow(.x)),
      n_linguas = purrr::map_int(linguas, ~ if (is.null(.x)) 0L else nrow(.x))
    )

  calcular_ranking_cr(
    dados,
    colunas = c("total_pi", "total_area_km2", "n_focos",
                "total_pi_siasi", "n_aldeias_sisaldeias", "n_localidades_indigenas", "n_etnias")
  )
}

#' @title Adiciona colunas de ranking (posição entre as CRs) para as colunas indicadas
#' @description Para cada coluna em `colunas`, adiciona `<coluna>_rank` (posição, 1 = maior
#' valor) e `<coluna>_n` (total de CRs consideradas, para exibir "Xª de N"). Usa
#' `dplyr::min_rank()` sobre valores decrescentes -- empates recebem a mesma posição.
#' @param df Tibble com uma linha por CR.
#' @param colunas Vetor com os nomes das colunas numéricas a rankear.
#' @return `df` com as colunas `<coluna>_rank`/`<coluna>_n` adicionadas para cada item de `colunas`.
calcular_ranking_cr <- function(df, colunas) {
  n_total <- nrow(df)
  for (col in colunas) {
    df[[paste0(col, "_rank")]] <- dplyr::min_rank(dplyr::desc(df[[col]]))
    df[[paste0(col, "_n")]] <- n_total
  }
  df
}

#' @title Mapa de destaque de uma CR sobre o restante do território nacional
#' @description Mesmo padrão visual já usado em `criar_mapa_pi_carencia_atendimento()`/mapas de
#' divergência: fundo cinza-claro com todas as CRs, contorno reforçado e preenchimento na cor
#' institucional para a CR em destaque.
#' @param malha_basica_dissolvida Objeto sf (ou caminho de gpkg) com um polígono por `cd_uorg`
#'   (alvo homônimo, `format = "file"`).
#' @param cd_uorg_destaque Código da CR a destacar.
#' @param cor Cor de preenchimento da CR destacada.
#' @return Objeto ggplot.
criar_mapa_destaque_cr <- function(malha_basica_dissolvida, cd_uorg_destaque, cor = CORES_FUNAI[["principal"]]) {

  if (is.character(malha_basica_dissolvida)) malha_basica_dissolvida <- sf::st_read(malha_basica_dissolvida, quiet = TRUE)

  malha <- malha_basica_dissolvida |>
    dplyr::mutate(cd_uorg = as.character(cd_uorg)) |>
    sf::st_transform(4326)

  destaque <- malha |> dplyr::filter(cd_uorg == as.character(cd_uorg_destaque))

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = malha, fill = "grey92", color = "white", linewidth = 0.15) +
    ggplot2::geom_sf(data = destaque, fill = cor, color = "grey20", linewidth = 0.4) +
    ggplot2::theme_minimal(base_size = 11) +
    obter_tema(tipo = "mapa", cor = cor)
}

#' @title Ordena os níveis de faixa etária do SIASI (texto, espaçamento inconsistente)
#' @description As faixas do SIASI vêm como texto livre (`"0 - 4"`, `"15 -19"`, `"75 +"`, ...),
#' sem padronização de espaço em torno do hífen -- ordenar como texto (`sort()`) intercalaria
#' "10 - 14" antes de "5 - 9". Extrai o número inicial de cada faixa para ordenar corretamente.
#' @param faixas Vetor de texto com as faixas etárias.
#' @return Vetor de níveis (`factor` levels), na ordem correta, do mais jovem ao mais velho.
ordenar_faixa_etaria <- function(faixas) {
  faixas_unicas <- unique(faixas)
  idade_inicial <- as.integer(sub("^(\\d+).*$", "\\1", faixas_unicas))
  faixas_unicas[order(idade_inicial)]
}

#' @title Pirâmide etária (SIASI) para uma CR
#' @description Barras horizontais espelhadas (homens à esquerda, mulheres à direita), mesmo
#' princípio visual do gráfico de pirâmide já usado no projeto irmão `painel_funai`
#' (`mod_visualizacao_proposta.R`), reimplementado aqui em `ggplot2` para reaproveitar o tema e a
#' paleta institucional já usados no restante deste pipeline.
#' @param piramide_df Data.frame com `sexo` (`"M"`/`"F"`), `faixa_etaria`, `populacao` -- uma CR
#'   já filtrada (ex. `dados_relatorio_cr$piramide_etaria[[i]]`).
#' @return Objeto ggplot.
grafico_piramide_etaria <- function(piramide_df) {

  niveis <- ordenar_faixa_etaria(piramide_df$faixa_etaria)

  df_plot <- piramide_df |>
    dplyr::mutate(
      faixa_etaria = factor(faixa_etaria, levels = niveis),
      populacao_plot = ifelse(sexo == "M", -populacao, populacao),
      sexo_label = ifelse(sexo == "M", "Homens", "Mulheres")
    )

  limite <- max(abs(df_plot$populacao_plot), na.rm = TRUE)

  ggplot2::ggplot(df_plot, ggplot2::aes(x = faixa_etaria, y = populacao_plot, fill = sexo_label)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      limits = c(-limite, limite) * 1.05,
      labels = ~ scales::label_number(big.mark = ".", decimal.mark = ",")(abs(.x))
    ) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      c(CORES_FUNAI[["principal"]], CORES_FUNAI[["estimativa_sc"]]), c("Homens", "Mulheres")
    )) +
    ggplot2::labs(x = NULL, y = "População indígena (SIASI)", fill = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    obter_tema(tipo = "barra", cor = CORES_FUNAI[["principal"]]) +
    ggplot2::theme(legend.position = "top")
}
