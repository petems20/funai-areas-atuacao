# Constantes padrão de exportação dos gráficos (usadas por exporta_grafico())
LARGURA_PADRAO_CM <- 32
ALTURA_PADRAO_CM <- 18.9
DPI_PADRAO <- 300

# Paleta de cores institucionais reutilizada pelos gráficos/tabelas do relatório
CORES_FUNAI <- c(
  "principal" = "#005a5a",
  "verde_escuro" = "#2d5a27",
  "critico" = "#a50f15",
  "estimativa_sc" = "#c47f2d"
)

#' @title Formata "MM/AAAA" (referência do VisData/CadÚnico) para "mês-AAAA" em português
#' @description Converte a coluna `referencia` de `arq_cadu` (ex.: "08/2026") no mesmo formato
#' abreviado já usado nos textos do relatório (ex.: "ago-2026"). Usada para manter o rótulo do
#' gráfico de CadÚnico sincronizado com o período mais recente disponível na extração, em vez de
#' um literal fixo que fica desatualizado a cada nova extração do VisData.
#' @param referencia String no formato "MM/AAAA".
#' @return String no formato "mmm-AAAA" (ex.: "ago-2026").
formatar_referencia_cadunico <- function(referencia) {
  meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
  partes <- strsplit(referencia, "/")[[1]]
  mes <- as.integer(partes[1])
  ano <- partes[2]
  paste0(meses_pt[mes], "-", ano)
}

#' @title Extremo claro de uma escala sequencial de matiz único, em espaço HCL
#' @description Calcula o extremo "baixo" (claro) de uma escala de cor contínua para mapas
#' coropléticos, preservando o matiz (hue) da cor institucional de entrada e controlando
#' croma/luminância em espaço HCL -- em vez de aproximar o "claro" por uma versão transparente da
#' cor sobre fundo branco (`scales::alpha(cor, 0.1)`), técnica antes usada em
#' `criar_mapa_coropletico_cr()`, `criar_mapa_coropletico_evolucao_cr()` e
#' `criar_mapa_pi_carencia_atendimento()`. Transparência tem duas desvantagens: o resultado
#' percebido depende do fundo por trás do polígono (só é branco por coincidência, já que o `theme`
#' de mapa usa fundo transparente) e comprime demais o contraste entre valores baixos e ausentes,
#' que ficam quase idênticos. Uma cor opaca clara controlada em HCL preserva o matiz institucional
#' de forma consistente e legível em toda a escala. `ggplot2::scale_fill_gradient()` já interpola
#' entre os dois extremos em espaço Lab por padrão, então basta trocar os extremos.
#' @param cor Cor institucional (hex) usada como extremo escuro/alto da escala.
#' @param luminancia_clara Luminância (0-100) do extremo claro. Padrão: 96 (quase branco).
#' @param fator_croma Fração do croma original da cor mantida no extremo claro (mais baixo =
#'   mais próximo de cinza). Padrão: 0.18.
#' @param croma_maximo Teto de croma do extremo claro, para cores institucionais muito saturadas.
#' @return String hex com a cor do extremo claro da escala.
extremo_claro_hcl <- function(cor, luminancia_clara = 96, fator_croma = 0.18, croma_maximo = 8) {
  hcl <- farver::decode_colour(cor, to = "hcl")
  farver::encode_colour(
    matrix(c(hcl[1, "h"], min(hcl[1, "c"] * fator_croma, croma_maximo), luminancia_clara), nrow = 1),
    from = "hcl"
  )
}

obter_tema <- function(
    tipo,
    cor = CORES_FUNAI[["principal"]]
    ){
  
  if(tipo == 'mapa'){
    return(
      ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "top",
          axis.title = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(face = "bold", color = cor),
          plot.subtitle = ggplot2::element_text(color = "grey40"),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(
            fill = "transparent",
            colour = NA
          ),
          plot.background = ggplot2::element_rect(
            fill = "transparent",
            colour = NA
          ),
          plot.margin = ggplot2::margin(1, 1, 1, 1)
        )
    )
  } else if(tipo == 'barra'){
    return(
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold", color = cor),
        plot.subtitle = ggplot2::element_text(color = "grey40"),
        axis.text.y = ggplot2::element_text(size = 9, color = "black"),
        axis.text.x = ggplot2::element_text(size = 10, color = "black"),
        legend.position = "top", 
        panel.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(1, 1, 1, 1),
      )
    )
  } else if(tipo == 'heatmap'){
    return(
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold", color = cor),
        plot.subtitle = ggplot2::element_text(color = "grey40"),
        axis.text.y = ggplot2::element_text(size = 9, color = "black"),
        axis.text.x = ggplot2::element_text(size = 10, color = "black"),
        legend.position = "top", 
        legend.key.width = ggplot2::unit(1.5, "cm"),
        panel.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(0, 0, 0, 0),
      )
    )
  }
  
  
}

#' @title Calcula pontos-âncora para rótulos em malha de setores censitários
#' @description Ponto interno (`sf::st_point_on_surface()`, sempre dentro do polígono, ao
#' contrário do centróide geométrico) usado como âncora pelo `ggrepel::geom_label_repel()` em
#' `criar_mapa_coropletico_cr()`/`criar_mapa_coropletico_evolucao_cr()`. Não aplica nenhum
#' deslocamento manual por sigla: o `ggrepel` já resolve sobreposição de rótulos sozinho,
#' desenhando uma linha-guia até este ponto quando precisa afastar o rótulo -- deslocar o
#' próprio ponto-âncora (como uma versão anterior desta função fazia, com offsets fixos por
#' sigla calibrados à mão) faz o `ggrepel` desenhar a linha-guia a partir de um ponto fantasma
#' longe do polígono real, e não corrige mais nada (o `ggrepel` já reposiciona o rótulo em
#' cima do ponto certo) -- só descola rótulo e polígono. Foi essa combinação que deixava as
#' siglas CR-PP, CR-ALSE e CR-LIS malposicionadas.
#' @param df_plot Objeto sf (EPSG:4326 ou outro) contendo os polígonos das CRs e a coluna sigla.
#' @return Objeto sf (POINT) em SIRGAS 2000 / Albers (EPSG:5880).
centroides_rotulos_sc <- function(df_plot){
  centroides <- df_plot |>
    dplyr::mutate(sigla = trimws(sigla)) |>
    sf::st_transform(5880) |>
    sf::st_point_on_surface() |>
    dplyr::mutate(sigla = gsub('CR-', '', trimws(sigla)))

  # Renomeia a coluna de geometria para "geometry" -- os consumidores (criar_mapa_coropletico_cr(),
  # criar_mapa_coropletico_evolucao_cr()) mapeiam aes(geometry = geometry, ...) no ggrepel::
  # geom_label_repel(); df_plot pode chegar com a coluna de geometria nomeada "geom" (gpkgs deste
  # pipeline, ver CLAUDE.md), então não dá para simplesmente propagar o nome original. Mesmo
  # padrão de renomeação de calcular_malha_basica_bruta() em R/geoprocessamento.R.
  sf_col <- attr(centroides, "sf_column")
  names(centroides)[names(centroides) == sf_col] <- "geometry"
  sf::st_geometry(centroides) <- "geometry"
  centroides
}

#' @title Calcula pontos-âncora para rótulos em malha municipal
#' @description Alias de `centroides_rotulos_sc()` -- os dois tipos de malha (setor censitário e
#' município) usam exatamente o mesmo cálculo de âncora; o nome próprio existe só para deixar
#' explícito, no call site de `criar_mapa_coropletico_cr(tipo_malha = ...)`, qual malha está sendo
#' usada.
#' @param df_plot Objeto sf (EPSG:4326 ou outro) contendo os polígonos das CRs e a coluna sigla.
#' @return Objeto sf (POINT) em SIRGAS 2000 / Albers (EPSG:5880).
centroides_rotulos_mun <- centroides_rotulos_sc

#' @title Simplifica geometria para exibição em mapa nacional, preservando topologia
#' @description Reduz drasticamente o número de vértices de polígonos antes de plotar em
#' ggplot2 -- só para exibição, nunca para as funções de geoprocessamento que precisam da
#' geometria original (área, junções espaciais, `sf::st_touches()` para adjacência). Um `ggplot`
#' guarda o data.frame inteiro de cada camada (geometria incluída); polígonos dissolvidos por CR
#' ou a malha municipal completa, sem simplificação, chegam a dezenas de milhões de vértices e
#' produzem objetos de centenas de MB a poucos GB -- imperceptível num mapa de extensão nacional,
#' mas caro para serializar/armazenar (`format = "qs"` no store do `{targets}`).
#' Duas etapas: (1) `sf::st_simplify()` (GEOS, rápido) reduz a massa de vértices num CRS métrico;
#' (2) `rmapshaper::ms_simplify(keep_shapes = TRUE)` faz o polimento final preservando topologia
#' (bordas compartilhadas entre polígonos vizinhos, evitando frestas/sobreposições que
#' `st_simplify()` sozinho introduziria por processar cada polígono de forma independente). A
#' etapa 1 é necessária porque o mapshaper (via V8, não Node) estoura o heap de memória do V8 se
#' receber a malha inteira sem redução prévia.
#' @param sf_obj Objeto sf com a geometria a simplificar.
#' @param tolerancia_m Tolerância (metros) do `st_simplify()` inicial. Padrão: 200m --
#'   imperceptível em mapas de extensão nacional.
#' @param keep Fração de vértices a manter na etapa final do `rmapshaper` (0-1). Padrão: 0.05.
#' @param crs_calc CRS métrico para a simplificação (padrão: `CRS_ALBERS_BR`).
#' @return Objeto sf simplificado, no mesmo CRS de entrada.
simplificar_para_mapa <- function(sf_obj, tolerancia_m = 200, keep = 0.05, crs_calc = CRS_ALBERS_BR) {
  crs_original <- sf::st_crs(sf_obj)
  sf_obj |>
    sf::st_transform(crs_calc) |>
    sf::st_make_valid() |>
    sf::st_simplify(dTolerance = tolerancia_m, preserveTopology = TRUE) |>
    rmapshaper::ms_simplify(keep = keep, keep_shapes = TRUE) |>
    sf::st_transform(crs_original)
}

exporta_grafico <- function(
    grafico,
    nome_output,
    largura = LARGURA_PADRAO_CM,
    altura = ALTURA_PADRAO_CM,
    dpi = DPI_PADRAO
) {

  # cria a pasta figuras caso não exista
  if (!dir.exists(DIR_FIGURAS)) {
    dir.create(DIR_FIGURAS, recursive = TRUE)
  }

  # caminhos dos arquivos
  arquivo_png <- file.path(DIR_FIGURAS, paste0(nome_output, ".png"))
  arquivo_svg <- file.path(DIR_FIGURAS, paste0(nome_output, ".svg"))

  # tamanhos
  unidade = 'cm'

  # exporta PNG
  ggplot2::ggsave(
    filename = arquivo_png,
    plot = grafico,
    width = largura,
    height = altura,
    units = unidade,
    dpi = dpi,
    bg = "transparent",
    type = "cairo"
  )

  # exporta SVG
  ggplot2::ggsave(
    filename = arquivo_svg,
    plot = grafico,
    bg = 'transparent',
    width = largura,
    height = altura,
    units = unidade
  )

  invisible(list(
    png = normalizePath(arquivo_png),
    svg = normalizePath(arquivo_svg)
  ))
}


obter_pop_indigena_regiao <- function(
    url,
    arquivo_saida = file.path(DIR_TABELAS, "populacao_indigena_grande_regiao.xlsx")
) {
  
  tabela <- jsonlite::fromJSON(url) |>
    dplyr::slice(-1) |>
    dplyr::transmute(
      regiao = D1N,
      localizacao = D5N,
      populacao = as.numeric(V)
    ) |>
    tidyr::pivot_wider(
      names_from = localizacao,
      values_from = populacao
    ) |>
    dplyr::mutate(
      `Percentual em TI` = round(
        100 * `Em terras indígenas` / Total,
        1
      )
    ) |>
    dplyr::arrange(
      factor(
        regiao,
        levels = c(
          "Norte",
          "Nordeste",
          "Sudeste",
          "Sul",
          "Centro-Oeste"
        )
      )
    ) |>
    dplyr::rename(
      `Região` = regiao
    )
  
  
  # linha de total (Brasil)
  
  total <- tabela |>
    dplyr::summarise(
      `Região` = "Brasil",
      Total = sum(Total, na.rm = TRUE),
      `Em terras indígenas` = sum(`Em terras indígenas`, na.rm = TRUE),
      `Fora de terras indígenas` = sum(`Fora de terras indígenas`, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      `Percentual em TI` = round(
        100 * `Em terras indígenas` / Total,
        1
      )
    )
  
  tabela <- dplyr::bind_rows(tabela, total)
  
  
  # salvar
  
  if (!dir.exists(DIR_TABELAS)) {
    dir.create(DIR_TABELAS, recursive = TRUE)
  }
  
  openxlsx::write.xlsx(
    tabela,
    arquivo_saida,
    overwrite = TRUE
  )
  
  tabela
}

obter_pop_indigena_regiao_situacao <- function(
    url = "https://apisidra.ibge.gov.br/values/t/9970/n2/all/v/allxp/p/all/c287/100362/c2661/12870/c1/all",
    arquivo_saida = file.path(DIR_TABELAS, "populacao_indigena_situacao_domicilio_grande_regiao.xlsx")
) {
  
  # Carregar dados
  
  tabela <- jsonlite::fromJSON(url) |>
    dplyr::slice(-1) |>
    dplyr::transmute(
      regiao = D1N,
      situacao = D6N,
      populacao = as.numeric(V)
    ) |>
    tidyr::pivot_wider(
      names_from = situacao,
      values_from = populacao
    ) |>
    dplyr::rename(Região = regiao)
  
  
  # percentuais (base limpa)
  
  cols_situacao <- setdiff(names(tabela), c("Região", "Total"))
  
  for (col in cols_situacao) {
    tabela[[paste0("Perc_", col)]] <-
      round(100 * tabela[[col]] / tabela$Total, 1)
  }
  
  
  # ordenação regional
  
  ordem_regioes <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
  
  tabela <- tabela |>
    dplyr::arrange(factor(Região, levels = ordem_regioes))
  
  
  # função de formatação (Urbana/Rural)
  
  formatar_valor <- function(valor, perc) {
    paste0(
      format(valor, big.mark = ".", decimal.mark = ","),
      " (", format(perc, nsmall = 1, decimal.mark = ",", trim = TRUE), "%)"
    )
  }
  
  
  # aplica formatação apenas para apresentação
  
  if (all(c("Urbana", "Rural") %in% names(tabela))) {
    
    tabela <- tabela |>
      dplyr::mutate(
        Urbana = formatar_valor(Urbana, Perc_Urbana),
        Rural  = formatar_valor(Rural,  Perc_Rural)
      )
  }
  
  
  # linha Brasil (correta e única)
  
  total <- jsonlite::fromJSON(url) |>
    dplyr::slice(-1) |>
    dplyr::transmute(
      regiao = D1N,
      situacao = D6N,
      populacao = as.numeric(V)
    ) |>
    tidyr::pivot_wider(
      names_from = situacao,
      values_from = populacao
    ) |>
    dplyr::summarise(
      Região = "Brasil",
      dplyr::across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # percentuais no total
  for (col in cols_situacao) {
    total[[paste0("Perc_", col)]] <-
      round(100 * total[[col]] / total$Total, 1)
  }
  
  # formatação total (Urbana/Rural)
  if (all(c("Urbana", "Rural") %in% names(total))) {
    
    total <- total |>
      dplyr::mutate(
        Urbana = formatar_valor(Urbana, Perc_Urbana),
        Rural  = formatar_valor(Rural,  Perc_Rural)
      )
  }
  
  
  # unir tabela final
  
  tabela <- dplyr::bind_rows(tabela, total)
  
  
  # remover colunas auxiliares de percentual

  tabela <- tabela |>
    dplyr::select(-dplyr::starts_with("Perc_"))

  # salvar

  if (!dir.exists(DIR_TABELAS)) {
    dir.create(DIR_TABELAS, recursive = TRUE)
  }
  
  openxlsx::write.xlsx(
    tabela,
    arquivo_saida,
    overwrite = TRUE
  )
  
  tabela
}

# obter_pop_indigena_faixa_idh_mun <- function(
#   url_populacao = 'https://apisidra.ibge.gov.br/values/t/9718/n1/all/n6/all/v/93,350/p/all/c1714/60024/c2661/12870',
#   url_idhm = 'https://apisidra.ibge.gov.br/values/t/9718/n1/all/n6/all/v/93,350/p/all/c1714/60024/c2661/12870',
#   arquivo_saida = 
# ){
#   tabela <- jsonlite::fromJSON(url) |>
#     dplyr::slice(-1)
# }



#' @title Gera tabela consolidada dos dados brutos
#'
#' @param tabela_osrm_consolidada 
#'
#' @returns tabela_resultado
#' @export
#'
#' @examples
tabela_resultado_bruto <- function(arq_resultado_bruto){
  
  tabela <- arrow::open_dataset(arq_resultado_bruto)
  
  tabela_resumo <- tabela |>
    dplyr::distinct(cd_setor, .keep_all = T) |>
    dplyr::group_by(Critério = criterio) |>
    dplyr::summarise(
      Total = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      `%` = round(Total * 100 / sum(Total), 2)
    ) |>
    dplyr::arrange(
      dplyr::desc(Total)
    )
  
  linha_total <- tabela_resumo |>
    dplyr::summarise(
      `Critério` = "Total",
      Total = sum(Total),
      `%` = sum(`%`)
    )
  
  tabela_resumo <- dplyr::bind_rows(
    tabela_resumo,
    linha_total
  )
  
  return(tabela_resumo)
}

#' @title Gera anexos das imputações
#'
#' @param df_imputacoes
#' @param sf_pontos_filtrados
#' @param arq_final
#'
#' @returns Lista com os anexos de UF, Município, Distrito e Setor
#' @export
#'
#' @examples
gera_anexos_imputacoes <- function(
    df_imputacoes,
    imputs_ti,
    sf_pontos_filtrados,
    arq_final
){
  
  # df_imputacoes <- targets::tar_read(df_imputacao_v2)
  # 
  # imputs_ti <- targets::tar_read(df_imputacao_v2)
  # 
  # arq_final <-
  #   sf::st_read(
  #     targets::tar_read(arquivo_gpkg_suavizado),
  #     quiet = TRUE
  #   )
  # 
  # sf_pontos_filtrados <-
  #   targets::tar_read(sf_pontos_filtrados)
  
  ## Colunas que aparecerão em cada anexo
  
  campos_uorg <- c(
    "codigoUnidadeImputado",
    "sigla",
    "nome"
  )
  
  campos_uf <- c(
    "cd_uf",
    "nm_uf"
  )
  
  campos_mun <- c(
    "cd_uf",
    "nm_uf",
    "cd_mun",
    "nm_mun"
  )
  
  campos_dist <- c(
    "cd_uf",
    "nm_uf",
    "cd_mun",
    "nm_mun",
    "cd_dist",
    "nm_dist"
  )
  
  campos_setor <- c(
    "cd_uf",
    "nm_uf",
    "cd_mun",
    "nm_mun",
    "cd_dist",
    "nm_dist",
    "cd_setor"
  )
  

  ## Base das imputações

  
  df_imputados <-
    df_imputacoes |>
    
    dplyr::anti_join(
      arq_final |>
        sf::st_drop_geometry() |>
        dplyr::distinct(cd_setor, cd_uorg) |>
        dplyr::mutate(cd_uorg = as.character(cd_uorg)),
      by = dplyr::join_by(
        cd_setor,
        codigoUnidadeImputado == cd_uorg
      )
    ) |>
    
    dplyr::left_join(
      sf_pontos_filtrados |>
        sf::st_drop_geometry() |>
        dplyr::select(codigoUnidade, nome, sigla) |>
        dplyr::mutate(codigoUnidade = as.character(codigoUnidade)),
      by = dplyr::join_by(
        codigoUnidadeImputado == codigoUnidade
      )
    ) |>
    
    dplyr::left_join(
      arq_final |>
        sf::st_drop_geometry() |>
        dplyr::distinct(
          cd_setor,
          cd_uorg,
          cd_uf,
          nm_uf,
          cd_mun,
          nm_mun,
          cd_dist,
          nm_dist
        ) |>
        dplyr::mutate(cd_uorg = as.character(cd_uorg)),
      by = dplyr::join_by(cd_setor)
    ) |>
    
    dplyr::group_by(cd_uf) |>
    dplyr::mutate(
      unico_uf = dplyr::n_distinct(codigoUnidadeImputado) == 1
    ) |>
    
    dplyr::group_by(cd_mun) |>
    dplyr::mutate(
      unico_mun = dplyr::n_distinct(codigoUnidadeImputado) == 1
    ) |>
    
    dplyr::group_by(cd_dist) |>
    dplyr::mutate(
      unico_dist = dplyr::n_distinct(codigoUnidadeImputado) == 1
    ) |>
    
    dplyr::ungroup()
  

  ## Vetores de seleção

  
  cols_uf <- c(
    setNames(campos_uorg,
             c("cd_uorg","sigla_cr","nome_uorg")),
    campos_uf
  )
  
  cols_mun <- c(
    setNames(campos_uorg,
             c("cd_uorg","sigla_cr","nome_uorg")),
    campos_mun
  )
  
  cols_dist <- c(
    setNames(campos_uorg,
             c("cd_uorg","sigla_cr","nome_uorg")),
    campos_dist
  )
  
  cols_setor <- c(
    setNames(campos_uorg,
             c("cd_uorg","sigla_cr","nome_uorg")),
    campos_setor
  )
  

  ## Anexo UF

  
  ufs_imputados <-
    df_imputados |>
    dplyr::filter(unico_uf) |>
    dplyr::select(dplyr::all_of(cols_uf)) |>
    dplyr::distinct() |>
    dplyr::arrange(cd_uf)
  

  ## Anexo Município

  
  muns_imputados <-
    df_imputados |>
    dplyr::filter(unico_mun & !unico_uf) |>
    dplyr::select(dplyr::all_of(cols_mun)) |>
    dplyr::distinct() |>
    dplyr::arrange(cd_uf, cd_mun)
  

  ## Anexo Distrito

  
  dists_imputados <-
    df_imputados |>
    dplyr::filter(unico_dist & !unico_mun & !unico_uf) |>
    dplyr::select(dplyr::all_of(cols_dist)) |>
    dplyr::distinct() |>
    dplyr::arrange(cd_uf, cd_mun, cd_dist)

  
  ## Anexo TIs
  tis_imputadas <- df_imputados |>
    dplyr::filter(!unico_dist & !unico_mun & !unico_uf) |>
    dplyr::inner_join(imputs_ti,
              by = dplyr::join_by(cd_setor, codigoUnidadeImputado)) |> 
    dplyr::group_by(codigoUnidadeImputado, nome, id_ti, terrai_nome, modalidade_ti, fase_ti) |>
    dplyr::summarise(
      n_setores = dplyr::n_distinct(cd_setor),
      .groups = "drop"
    )

  ## Anexo Setor
  setores_imputados <-
    df_imputados |>
    dplyr::filter(!unico_dist & !unico_mun & !unico_uf) |>
    dplyr::anti_join(imputs_ti,
               by = dplyr::join_by(cd_setor, codigoUnidadeImputado)) |>
    dplyr::select(dplyr::all_of(cols_setor)) |>
    dplyr::distinct() |>
    dplyr::arrange(cd_uf, cd_mun, cd_dist, cd_setor)
  
  return(
    list(
      uf = ufs_imputados,
      municipio = muns_imputados,
      distrito = dists_imputados,
      tis = tis_imputadas,
      setor = setores_imputados
    )
  )
}

#' @title Gera tabela consolidada por critério e UF
#'
#' @param tabela_osrm_consolidada 
#'
#' @returns tabela_resultado
#' @export
#'
#' @examples
tabela_resumo_criterio <- function(arq_resultado_bruto){
  
  tabela <- arrow::open_dataset(arq_resultado_bruto)
  
  # Base de setores únicos
  setores <- tabela |>
    dplyr::filter(ordem == 1) |>
    dplyr::distinct(cd_setor, .keep_all = TRUE)
  
  # Total de setores por UF
  total_setores <- setores |>
    dplyr::group_by(UF = nm_uf) |>
    dplyr::summarise(
      Total_Setores = dplyr::n_distinct(cd_setor),
      .groups = "drop"
    ) |>
    dplyr::collect()
  
  # Quantidade de setores por critério
  tabela_resumo <- setores |>
    dplyr::group_by(
      UF = nm_uf,
      Critério = criterio
    ) |>
    dplyr::summarise(
      Total = dplyr::n_distinct(cd_setor),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::left_join(
      total_setores,
      by = "UF"
    ) |>
    dplyr::mutate(
      Percentual = round(
        100 * Total / Total_Setores,
        2
      )
    )
  
  # Formato para relatório
  tabela_final <- tabela_resumo |>
    tidyr::pivot_wider(
      names_from = Critério,
      values_from = c(Total, Percentual),
      values_fill = 0
    ) |>
    arrange(desc(`Percentual_Linha Reta`))
  
  # Linha total
  linha_total <- tabela_resumo |>
    dplyr::group_by(Critério) |>
    dplyr::summarise(
      Total = sum(Total),
      .groups = "drop"
    )
  
  total_geral <- sum(total_setores$Total_Setores)
  
  linha_total <- linha_total |>
    dplyr::mutate(
      Percentual = round(
        100 * Total / total_geral,
        2
      )
    ) |>
    tidyr::pivot_wider(
      names_from = Critério,
      values_from = c(Total, Percentual)
    ) |>
    dplyr::mutate(
      UF = "Total",
      Total_Setores = total_geral,
      .before = 1
    )
  
  tabela_final <- dplyr::bind_rows(
    tabela_final,
    linha_total
  )
  
  tabela_final
}

# Não usada por nenhum alvo do pipeline (substituída por tabela_dados_malha_basica()) -- mantida
# aqui, fora de _targets.R, para facilitar eventual reinserção futura (ver mesmo raciocínio em
# criar_grafico_orcamento_heatmap() e criar_mapa_perda_logistica(), abaixo/mais adiante no arquivo).
tabela_dados_sc <- function(
    arq_final = arquivo_final_sc_v2,
    arq_final_sc = arquivo_gpkg_suavizado_v2,
    agg_sc = censo_agregado_sc_pi,
    arq_siorg = arq_siorg,
    arq_siorg_estrutura = arq_siorg_estrutura,
    arq_siop = arq_siop,
    arq_cargos = arq_cargos,
    arq_ug = arq_uorg_ug,
    arq_bdqueimadas = arq_bdqueimadas,
    arq_tis = arq_tis
) {

  # arq_final = tar_read(arquivo_final_sc_v2_msol)
  # arq_final_sc = tar_read(arquivo_gpkg_suavizado_v2_msol)
  # agg_sc = tar_read(censo_agregado_sc_pi)
  # arq_siorg = tar_read(arq_siorg)
  # arq_siorg_estrutura = tar_read(arq_siorg_estrutura)
  # arq_siop = tar_read(arq_siop)
  # arq_cargos = tar_read(arq_cargos)
  # arq_ug = tar_read(arq_uorg_ug)
  # arq_bdqueimadas <- tar_read(arq_bdqueimadas)
  # arq_tis <- tar_read(arq_tis)

  # Leitura da base espacial final
  arq_final <- sf::st_read(arq_final, quiet = TRUE) |> mutate(sigla = trimws(sigla))
  arq_bdqueimadas <- sf::st_transform(arq_bdqueimadas, sf::st_crs(arq_final))
  arq_tis <- sf::st_transform(arq_tis, sf::st_crs(arq_final))

  # Consolidação dos dados orçamentários
  arq_siop <- arq_siop |>
    dplyr::filter(ano %in% c(2023, 2024, 2025, 2026))

  dados_orcamento <- arq_siop |>
    dplyr::group_by(ano, ug_resp) |>
    dplyr::summarise(
      autorizado = sum(autorizado, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = ano,
      values_from = autorizado,
      names_prefix = "autorizado_"
    ) |>
    dplyr::left_join(
      arq_ug |>
        dplyr::mutate(
          ug = as.character(ug),
          cd_uorg = as.character(cd_uorg)
        ),
      by = dplyr::join_by(ug_resp == ug)
    )

  # Identificação das coordenações regionais
  vetor_crs <- arq_siorg |>
    dplyr::filter(grepl("^CR-", trimws(sigla))) |>
    dplyr::pull(codigoUnidade)

  cr_unidade <- arq_siorg |>
    dplyr::filter(
      codigoUnidade %in% vetor_crs |
        codigoUnidadePai %in% vetor_crs
    ) |>
    dplyr::mutate(
      coord_regional = dplyr::if_else(
        codigoUnidadePai == 173,
        codigoUnidade,
        codigoUnidadePai
      )
    ) |>
    dplyr::select(
      coord_regional,
      codigoUnidade
    ) |>
    dplyr::left_join(
      arq_siorg |>
        dplyr::select(
          codigoUnidade,
          nome_cr = nome,
          sigla_cr = sigla
        ) |>
        dplyr::distinct(),
      by = dplyr::join_by(
        coord_regional == codigoUnidade
      )
    )

  # Cálculo dos pontos de CCE por CR
  vetor_unidades <- arq_siorg |>
    dplyr::filter(
      codigoUnidade %in% vetor_crs |
        codigoUnidadePai %in% vetor_crs
    ) |>
    dplyr::pull(codigoUnidade)

  unidades_cr <- arq_siorg_estrutura |>
    dplyr::filter(
      `Código Unidade` %in% vetor_unidades
    ) |>
    dplyr::mutate(
      cargo = paste(
        `Tipo do Cargo`,
        Categoria,
        sprintf("%02d", Nível)
      )
    ) |>
    dplyr::left_join(
      arq_cargos,
      by = dplyr::join_by(cargo)
    ) |>
    dplyr::left_join(
      cr_unidade |>
        dplyr::mutate(
          codigoUnidade = as.integer(codigoUnidade)
        ) |>
        dplyr::distinct(),
      by = dplyr::join_by(
        `Código Unidade` == codigoUnidade
      )
    )

  tabela_dados <- unidades_cr |>
    dplyr::group_by(
      coord_regional,
      nome_cr,
      sigla_cr
    ) |>
    dplyr::summarise(
      total_pts_cce = sum(
        cce_unitario * Quantidade,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(coord_regional = as.character(coord_regional)) |>
    dplyr::left_join(
      dados_orcamento |>
        dplyr::select(-ug_ds),
      by = dplyr::join_by(
        coord_regional == cd_uorg
      )
    )

  # Montagem da tabela principal
  tabela_dados_final <- arq_final |>
    dplyr::left_join(
      tabela_dados |> select(-nome_cr, sigla_cr),
      by = dplyr::join_by(
        cd_uorg == coord_regional
      )
    )

  # Contagem de focos de fogo por CR
  focos_fogo_cr <- sf::st_join(
    arq_bdqueimadas,
    arq_final,
    join = sf::st_within,
    left = FALSE
  ) |>
    sf::st_drop_geometry() |>
    dplyr::count(
      cd_uorg,
      name = "n_focos"
    )

  # Contagem de focos de fogo em terras indígenas
  focos_fogo_ti <- sf::st_join(
    arq_bdqueimadas,
    arq_tis |>
      dplyr::select(
        terrai_nome,
        terrai_codigo
      ),
    join = sf::st_within,
    left = FALSE
  ) |>
    sf::st_join(
      arq_final,
      join = sf::st_within,
      left = FALSE
    ) |>
    sf::st_drop_geometry() |>
    dplyr::count(
      cd_uorg,
      name = "n_focos_em_ti"
    )

  # Consolidação dos indicadores de queimadas
  tb_focos_fogo <- focos_fogo_cr |>
    dplyr::left_join(
      focos_fogo_ti,
      by = dplyr::join_by(cd_uorg)
    )

  # Inclusão de queimadas
  tabela_dados_final <- tabela_dados_final |>
    mutate(sigla_join = toupper(trimws(gsub(' ', "", sigla_cr)))) |>
    dplyr::left_join(
      tb_focos_fogo,
      by = dplyr::join_by(cd_uorg)
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("n_"),
        ~ dplyr::coalesce(.x, 0)
      )
    ) |> select(-sigla_join)


  # Indicadores derivados
  tabela_dados_final <- tabela_dados_final |>
    dplyr::mutate(
      across(starts_with('autorizado_'),  ~ dplyr::coalesce(.x, 0))
    ) |>
    dplyr::mutate(
      area_por_cce = ifelse(total_pts_cce == 0, NA_real_, total_area_km2 / total_pts_cce),

      autorizado_por_pi_2023 = ifelse(total_pi == 0, NA_real_, autorizado_2023 / total_pi),
      autorizado_por_pi_2024 = ifelse(total_pi == 0, NA_real_, autorizado_2024 / total_pi),
      autorizado_por_pi_2025 = ifelse(total_pi == 0, NA_real_, autorizado_2025 / total_pi),
      autorizado_por_pi_2026 = ifelse(total_pi == 0, NA_real_, autorizado_2026 / total_pi),

      autorizado_por_pi_rural_2023 = ifelse(total_pi_rural == 0, NA_real_, autorizado_2023 / total_pi_rural),
      autorizado_por_pi_rural_2024 = ifelse(total_pi_rural == 0, NA_real_, autorizado_2024 / total_pi_rural),
      autorizado_por_pi_rural_2025 = ifelse(total_pi_rural == 0, NA_real_, autorizado_2025 / total_pi_rural),
      autorizado_por_pi_rural_2026 = ifelse(total_pi_rural == 0, NA_real_, autorizado_2026 / total_pi_rural),

      autorizado_por_pi_urbano_2023 = ifelse(total_pi_urbana == 0, NA_real_, autorizado_2023 / total_pi_urbana),
      autorizado_por_pi_urbano_2024 = ifelse(total_pi_urbana == 0, NA_real_, autorizado_2024 / total_pi_urbana),
      autorizado_por_pi_urbano_2025 = ifelse(total_pi_urbana == 0, NA_real_, autorizado_2025 / total_pi_urbana),
      autorizado_por_pi_urbano_2026 = ifelse(total_pi_urbana == 0, NA_real_, autorizado_2026 / total_pi_urbana)
    )

  # Organizando variáveis

  tabela_dados_final <- tabela_dados_final |>
    dplyr::relocate(
      dplyr::starts_with("v0"),
      .after = dplyr::last_col()
    ) |>
    dplyr::relocate(
      geom,
      .after = dplyr::last_col()
    )

  return(tabela_dados_final)
}

#' @title Trata o sigilo estatístico do IBGE ("X") como zero
#' @description Converte um vetor de uma variável agregada do Censo -- com o marcador de sigilo do
#' IBGE ("X": célula suprimida por conter um valor real maior que zero, porém abaixo do limiar de
#' divulgação) -- tratando "X" como 0. Convenção usada para SOMAS de população (o resultado é uma
#' subestimativa conhecida e documentada em nt_jurisdicoes.qmd, seção "Limitações da Proposta por
#' Setores Censitários"). NÃO deve ser usada para construir uma TAXA (numerador/denominador): a
#' mesma substituição distorceria o resultado numa direção ou noutra, a depender de qual variável
#' da fórmula foi suprimida -- nesse caso, usar `setor_sem_sigilo()` para excluir o setor inteiro
#' do cálculo daquele indicador.
#' @param x Vetor de caracteres com valores numéricos e o marcador "X" do IBGE.
#' @return Vetor numérico, com "X" convertido para 0.
limpar_sigilo_ibge <- function(x) {
  as.numeric(ifelse(x == "X", "0", as.character(x)))
}

#' @title Identifica linhas sem sigilo do IBGE nas variáveis de um indicador
#' @description Para um indicador de taxa (numerador/denominador construídos a partir de
#' variáveis do Censo), identifica quais setores censitários NÃO têm o marcador de sigilo do IBGE
#' ("X") em nenhuma das colunas relevantes -- usado para excluir o setor inteiro do cálculo
#' daquele indicador, em vez de tratar a supressão como 0 (ver `limpar_sigilo_ibge()`).
#' @param df Data.frame/tibble com as colunas de `cols`.
#' @param cols Vetor de nomes de colunas relevantes para o indicador.
#' @return Vetor lógico, `TRUE` para os setores sem sigilo em nenhuma das `cols`.
#' @details Usa `df[[cl]]` (nunca `df[cols]`) porque `df` pode chegar como `data.table` -- para um
#' `data.table`, `df[cols]` com `cols` character é interpretado como join por `i`, não seleção de
#' coluna, e falha pedindo o argumento `on=`.
setor_sem_sigilo <- function(df, cols) {
  rowSums(vapply(cols, function(cl) df[[cl]] == "X", logical(nrow(df)))) == 0
}

#' @title Calcula uma taxa por CR, com piso mínimo de amostra
#' @description Divide o numerador pelo denominador de um indicador de taxa por CR (já somados
#' só sobre setores sem sigilo do IBGE, ver `setor_sem_sigilo()`), retornando `NA` quando a CR
#' não tem setores válidos suficientes para publicar uma taxa estável (ver
#' `LIMIAR_AMOSTRA_MINIMA_INDICADOR` em R/config.R) ou quando o denominador é zero.
#' `criar_mapa_coropletico_cr()` já trata esse `NA` como "sem dado" no mapa.
#' @param num,den Vetores numéricos do numerador e do denominador (já somados só sobre setores
#'   válidos, ver `n_setores_validos_*`/`num_*`/`den_*` em `tabela_dados_malha_basica()`).
#' @param n_setores_validos Vetor com a contagem de setores válidos usada para somar `num`/`den`.
#' @param limiar Piso mínimo de setores válidos. Padrão: `LIMIAR_AMOSTRA_MINIMA_INDICADOR`.
#' @return Vetor numérico com a taxa em percentual (0-100), ou `NA` abaixo do piso de amostra.
calcular_taxa_indicador <- function(num, den, n_setores_validos, limiar = LIMIAR_AMOSTRA_MINIMA_INDICADOR) {
  dplyr::if_else(n_setores_validos >= limiar & den > 0, num * 100 / den, NA_real_)
}

#' @title Calcula população indígena por CR via método híbrido SIDRA (TI + Município "fora de TI")
#' @description Substitui a agregação geométrica por setor censitário (soma de `v01690` por CR em
#' `unir_poligonos()`) por importação direta dos agregados do Censo 2022 no SIDRA: população por
#' Terra Indígena (agregado 9719) + população por Município "fora de terras indígenas" (agregado
#' 8175, classificação `c2661 = 12870`). Como TI e Município se sobrepõem geograficamente na malha
#' de jurisdição, somar "população da TI" com a população TOTAL do município que a contém
#' contaria a mesma pessoa duas vezes -- por isso o lado do município usa especificamente o corte
#' "fora de TI" do SIDRA (não `tb_pop_pi`, que é o Total e serve a outro propósito: denominador de
#' `pct_cadunico`). A atribuição de cada território (TI ou Município) à CR responsável reaproveita
#' `malha_basica_bruta` (mesma fonte da camada "Malha Básica" do projeto QGIS AACR), em vez de
#' repetir a lógica de join ad hoc.
#'
#' TIs sem código oficial do Censo 2022 no IBGE (SIDRA não as reconhece) mantêm o método de
#' estimativa por setor censitário já usado em `grafico_situacao_atual_sii()` -- mas aqui essa
#' estimativa fica isolada em `total_pi_ti_nao_oficial_estimada`, **sem ser somada a `total_pi`**:
#' como o SIDRA não sabe que essas TIs existem, seus residentes já estão implicitamente incluídos
#' no "fora de TI" do município hospedeiro, e somar a estimativa por cima duplicaria essas pessoas.
#' A coluna fica disponível só para nota metodológica/QA (ver `verificar_populacao_indigena_cr_sidra()`).
#' @param arq_malha_basica_bruta Caminho do gpkg de `calcular_malha_basica_bruta()` (alvo
#'   `malha_basica_bruta`; colunas `cd_territorio`, `tipo_territorio`, `cd_uorg`).
#' @param tb_pop_ti_sidra Tabela bruta de população por TI do SIDRA (alvo `tb_pop_ti`).
#' @param tb_pop_mun_fora_ti_sidra Tabela bruta de população por município "fora de TI" do SIDRA
#'   (alvo `tb_pop_mun_fora_ti`).
#' @param df_tis_ibge De-para de códigos TI FUNAI vs IBGE (alvo `arq_tis_ibge`).
#' @param df_sc_agregado Dados por setor censitário (alvo `censo_agregado_sc_pi_dados`) -- usado só
#'   para a estimativa de QA das TIs sem código IBGE.
#' @param df_mapeamento_setores Resultado de `mapear_setores_ti()` (alvo
#'   `mapeamento_setores_estimativa`).
#' @return tibble com `cd_uorg`, `total_pi` (= `total_pi_ti_oficial` + `total_pi_mun_fora_ti`),
#'   `total_pi_ti_oficial`, `total_pi_mun_fora_ti`, `total_pi_ti_nao_oficial_estimada` (não somada
#'   a `total_pi`, só para conferência).
calcular_populacao_indigena_cr_sidra <- function(
    arq_malha_basica_bruta,
    tb_pop_ti_sidra,
    tb_pop_mun_fora_ti_sidra,
    df_tis_ibge,
    df_sc_agregado,
    df_mapeamento_setores
) {

  # arq_malha_basica_bruta = targets::tar_read(malha_basica_bruta)
  # tb_pop_ti_sidra = targets::tar_read(tb_pop_ti)
  # tb_pop_mun_fora_ti_sidra = targets::tar_read(tb_pop_mun_fora_ti)
  # df_tis_ibge = targets::tar_read(arq_tis_ibge) |> sf::st_drop_geometry()
  # df_sc_agregado = targets::tar_read(censo_agregado_sc_pi_dados)
  # df_mapeamento_setores = targets::tar_read(mapeamento_setores_estimativa)

  malha <- sf::st_read(arq_malha_basica_bruta, quiet = TRUE) |> sf::st_drop_geometry()

  malha_ti <- malha |>
    dplyr::filter(tipo_territorio == "Terra Indígena") |>
    dplyr::mutate(terrai_codigo = as.integer(cd_territorio))

  malha_mun <- malha |>
    dplyr::filter(tipo_territorio == "Município") |>
    dplyr::mutate(cd_mun = as.character(cd_territorio))

  # --- Terras Indígenas: SIDRA oficial (agregado 9719) para TIs com código IBGE ---
  df_tis_ibge_clean <- df_tis_ibge |>
    dplyr::select(cd_ti_ibge = cd_ti, cd_funai) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))

  malha_ti_ibge <- malha_ti |>
    dplyr::left_join(df_tis_ibge_clean, by = dplyr::join_by(terrai_codigo == cd_funai))

  pop_ti_oficial <- tb_pop_ti_sidra |>
    dplyr::filter(D1C == 1432) |>
    dplyr::transmute(
      cd_ti_ibge = as.integer(D2C),
      pop_ti_oficial = limpar_sigilo_ibge(V)
    )

  ti_oficial <- malha_ti_ibge |>
    dplyr::filter(!is.na(cd_ti_ibge)) |>
    dplyr::left_join(pop_ti_oficial, by = "cd_ti_ibge") |>
    dplyr::mutate(pop_ti_oficial = dplyr::coalesce(pop_ti_oficial, 0)) |>
    dplyr::group_by(cd_uorg) |>
    dplyr::summarise(total_pi_ti_oficial = sum(pop_ti_oficial, na.rm = TRUE), .groups = "drop")

  # --- Terras Indígenas sem código IBGE: estimativa por setor, só para QA (NÃO somada ao total) ---
  tis_sem_codigo <- malha_ti_ibge |> dplyr::filter(is.na(cd_ti_ibge)) |> dplyr::pull(terrai_codigo)

  colunas_pop_censo <- c("v01690", "v0001", "v02101")
  nome_col_pop <- intersect(names(df_sc_agregado), colunas_pop_censo)[1]

  colunas_ti_possiveis <- c("terrai_codigo", "cod_ti", "cd_ti", "codigo_ti", "cd_funai", "id_ti")
  df_mapeamento <- df_mapeamento_setores
  if (inherits(df_mapeamento, "sf")) df_mapeamento <- sf::st_drop_geometry(df_mapeamento)
  nome_col_ti <- intersect(names(df_mapeamento), colunas_ti_possiveis)[1]
  df_mapeamento <- df_mapeamento |> dplyr::rename(terrai_codigo = dplyr::all_of(nome_col_ti))

  estimativa_ti_nao_oficial <- df_sc_agregado |>
    dplyr::mutate(cd_setor = as.character(cd_setor)) |>
    dplyr::inner_join(df_mapeamento, by = "cd_setor") |>
    dplyr::filter(terrai_codigo %in% tis_sem_codigo) |>
    dplyr::group_by(terrai_codigo) |>
    dplyr::summarise(pop_estimada = sum(as.numeric(.data[[nome_col_pop]]), na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(malha_ti_ibge |> dplyr::select(terrai_codigo, cd_uorg), by = "terrai_codigo") |>
    dplyr::group_by(cd_uorg) |>
    dplyr::summarise(total_pi_ti_nao_oficial_estimada = sum(pop_estimada, na.rm = TRUE), .groups = "drop")

  # --- Municípios: SIDRA "fora de TI" (agregado 8175, c2661 = 12870) ---
  pop_mun_fora_ti <- tb_pop_mun_fora_ti_sidra |>
    dplyr::slice(-1) |>
    dplyr::transmute(
      cd_mun = as.character(D1C),
      pop_mun_fora_ti = limpar_sigilo_ibge(V)
    )

  mun_fora_ti <- malha_mun |>
    dplyr::left_join(pop_mun_fora_ti, by = "cd_mun") |>
    dplyr::group_by(cd_uorg) |>
    dplyr::summarise(total_pi_mun_fora_ti = sum(pop_mun_fora_ti, na.rm = TRUE), .groups = "drop")

  # --- Consolidação por CR ---
  malha |>
    dplyr::distinct(cd_uorg) |>
    dplyr::left_join(ti_oficial, by = "cd_uorg") |>
    dplyr::left_join(mun_fora_ti, by = "cd_uorg") |>
    dplyr::left_join(estimativa_ti_nao_oficial, by = "cd_uorg") |>
    dplyr::mutate(
      dplyr::across(
        c(total_pi_ti_oficial, total_pi_mun_fora_ti, total_pi_ti_nao_oficial_estimada),
        ~ dplyr::coalesce(.x, 0)
      ),
      total_pi = total_pi_ti_oficial + total_pi_mun_fora_ti
    )
}

#' @title Tabela de dados por CR usando a malha básica (situação atual, sem ajustes manuais)
#' @description Equivalente a `tabela_dados_sc()`, mas usando `malha_basica_bruta`/
#' `malha_basica_dissolvida` -- a jurisdição "como está" (TI + Município, decisão da Diretoria
#' Colegiada de 28/07/2026) -- em vez de `arquivo_final_sc_v2_msol`, que reflete um estágio
#' intermediário do processo de imputação manual, anterior à malha institucionalmente mais
#' recente. `malha_basica_bruta` não tem uma malha própria de setores censitários (opera a nível
#' de TI/Município), então população urbana/rural e as variáveis `v0*` do Censo (usadas por
#' alfabetização, saneamento e registro civil) são obtidas atribuindo cada setor
#' censitário à CR que a malha básica já definiu para seu território, por join de atributo
#' (setor -> TI, via `df_setores_em_ti`; TI/Município -> CR, via `malha_basica_bruta`) -- a mesma
#' lógica territorial (TI tem precedência sobre Município) que `calcular_malha_basica_bruta()` já
#' resolveu, sem repetir um join espacial. `total_pi` usa o método híbrido SIDRA de
#' `calcular_populacao_indigena_cr_sidra()` (a mesma fonte que `tabela_dados_municipio()` já usa),
#' mais preciso que somar `v01690` por setor porque evita dupla contagem entre TI e o município
#' que a contém.
#' @param arq_malha_basica_dissolvida Caminho do gpkg de malha básica dissolvida por CR (alvo
#'   `malha_basica_dissolvida`) -- usado como `arq_final` para os joins espaciais (queimadas, TI).
#' @param arq_malha_basica_bruta Caminho do gpkg de malha básica por TI/Município (alvo
#'   `malha_basica_bruta`) -- usado por `calcular_populacao_indigena_cr_sidra()` e para o join de
#'   atributo setor -> CR.
#' @param arq_setores_censitarios Objeto sf com a malha de setores censitários (alvo
#'   `arq_setores_censitarios`), usado apenas por seus atributos (`cd_setor`, `cd_sit`, `cd_mun`).
#' @param df_setores_em_ti Associação setor censitário -> Terra Indígena já calculada com limiar
#'   de ruído de borda (alvo `df_setores_em_ti`, `mapear_setores_ti()` em `R/define_divisoes_cr.R`).
#' @param agg_sc Agregados do Censo 2022 por setor censitário (alvo `censo_agregado_sc_pi_dados`).
#' @param arq_siorg,arq_siorg_estrutura,arq_siop,arq_cargos,arq_ug,arq_bdqueimadas,arq_tis
#'   Mesmos parâmetros de `tabela_dados_sc()`.
#' @param tb_pop_ti_sidra,tb_pop_mun_fora_ti_sidra,df_tis_ibge,df_mapeamento_setores Mesmos
#'   parâmetros homônimos de `tabela_dados_municipio()`/`calcular_populacao_indigena_cr_sidra()`.
#' @return `sf` com uma feição por CR, mesmo formato de saída de `tabela_dados_sc()`. Inclui
#' também, para os 4 indicadores de taxa consumidos em _targets.R (registro civil, alfabetização,
#' saneamento, água), os campos `num_*`/`den_*` (numerador/denominador já somados só sobre
#' setores sem sigilo do IBGE em nenhuma variável daquele indicador) e `n_setores_validos_*`
#' (contagem desses setores válidos, usada para aplicar `LIMIAR_AMOSTRA_MINIMA_INDICADOR`).
tabela_dados_malha_basica <- function(
    arq_malha_basica_dissolvida,
    arq_malha_basica_bruta,
    arq_setores_censitarios,
    df_setores_em_ti,
    agg_sc,
    arq_siorg,
    arq_siorg_estrutura,
    arq_siop,
    arq_cargos,
    arq_ug,
    arq_bdqueimadas,
    arq_tis,
    tb_pop_ti_sidra,
    tb_pop_mun_fora_ti_sidra,
    df_tis_ibge,
    df_mapeamento_setores
) {

  arq_final <- sf::st_read(arq_malha_basica_dissolvida, quiet = TRUE) |>
    dplyr::mutate(cd_uorg = as.character(cd_uorg), sigla = trimws(sigla))
  arq_bdqueimadas <- sf::st_transform(arq_bdqueimadas, sf::st_crs(arq_final))
  arq_tis <- sf::st_transform(arq_tis, sf::st_crs(arq_final))

  # --- Orçamento (SIOP), cargos (SIORG) e pontos de CCE por CR -- idêntico a tabela_dados_sc() ---
  arq_siop_periodo <- arq_siop |>
    dplyr::filter(ano %in% c(2023, 2024, 2025, 2026))

  dados_orcamento <- arq_siop_periodo |>
    dplyr::group_by(ano, ug_resp) |>
    dplyr::summarise(autorizado = sum(autorizado, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = ano, values_from = autorizado, names_prefix = "autorizado_") |>
    dplyr::left_join(
      arq_ug |> dplyr::mutate(ug = as.character(ug), cd_uorg = as.character(cd_uorg)),
      by = dplyr::join_by(ug_resp == ug)
    )

  vetor_crs <- arq_siorg |>
    dplyr::filter(grepl("^CR-", trimws(sigla))) |>
    dplyr::pull(codigoUnidade)

  cr_unidade <- arq_siorg |>
    dplyr::filter(codigoUnidade %in% vetor_crs | codigoUnidadePai %in% vetor_crs) |>
    dplyr::mutate(
      coord_regional = dplyr::if_else(codigoUnidadePai == 173, codigoUnidade, codigoUnidadePai)
    ) |>
    dplyr::select(coord_regional, codigoUnidade) |>
    dplyr::left_join(
      arq_siorg |> dplyr::select(codigoUnidade, nome_cr = nome, sigla_cr = sigla) |> dplyr::distinct(),
      by = dplyr::join_by(coord_regional == codigoUnidade)
    )

  vetor_unidades <- arq_siorg |>
    dplyr::filter(codigoUnidade %in% vetor_crs | codigoUnidadePai %in% vetor_crs) |>
    dplyr::pull(codigoUnidade)

  unidades_cr <- arq_siorg_estrutura |>
    dplyr::filter(`Código Unidade` %in% vetor_unidades) |>
    dplyr::mutate(cargo = paste(`Tipo do Cargo`, Categoria, sprintf("%02d", Nível))) |>
    dplyr::left_join(arq_cargos, by = dplyr::join_by(cargo)) |>
    dplyr::left_join(
      cr_unidade |> dplyr::mutate(codigoUnidade = as.integer(codigoUnidade)) |> dplyr::distinct(),
      by = dplyr::join_by(`Código Unidade` == codigoUnidade)
    )

  tabela_dados <- unidades_cr |>
    dplyr::group_by(coord_regional, nome_cr, sigla_cr) |>
    dplyr::summarise(total_pts_cce = sum(cce_unitario * Quantidade, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(coord_regional = as.character(coord_regional)) |>
    dplyr::left_join(dados_orcamento |> dplyr::select(-ug_ds), by = dplyr::join_by(coord_regional == cd_uorg))

  tabela_dados_final <- arq_final |>
    dplyr::left_join(
      tabela_dados |> dplyr::select(-nome_cr, sigla_cr),
      by = dplyr::join_by(cd_uorg == coord_regional)
    )

  # --- Focos de queimadas (por CR e em TI) -- idêntico a tabela_dados_sc() ---
  focos_fogo_cr <- sf::st_join(arq_bdqueimadas, arq_final, join = sf::st_within, left = FALSE) |>
    sf::st_drop_geometry() |>
    dplyr::count(cd_uorg, name = "n_focos")

  focos_fogo_ti <- sf::st_join(
    arq_bdqueimadas,
    arq_tis |> dplyr::select(terrai_nome, terrai_codigo),
    join = sf::st_within,
    left = FALSE
  ) |>
    sf::st_join(arq_final, join = sf::st_within, left = FALSE) |>
    sf::st_drop_geometry() |>
    dplyr::count(cd_uorg, name = "n_focos_em_ti")

  tb_focos_fogo <- focos_fogo_cr |> dplyr::left_join(focos_fogo_ti, by = "cd_uorg")

  tabela_dados_final <- tabela_dados_final |>
    dplyr::mutate(sigla_join = toupper(trimws(gsub(" ", "", sigla)))) |>
    dplyr::left_join(tb_focos_fogo, by = "cd_uorg") |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("n_"), ~ dplyr::coalesce(.x, 0))) |>
    dplyr::select(-sigla_join)

  # --- População total: método híbrido SIDRA (TI oficial + município "fora de TI"), evita dupla
  # contagem -- mesma fonte usada por tabela_dados_municipio() ---
  populacao_sidra <- calcular_populacao_indigena_cr_sidra(
    arq_malha_basica_bruta = arq_malha_basica_bruta,
    tb_pop_ti_sidra = tb_pop_ti_sidra,
    tb_pop_mun_fora_ti_sidra = tb_pop_mun_fora_ti_sidra,
    df_tis_ibge = df_tis_ibge,
    df_sc_agregado = agg_sc,
    df_mapeamento_setores = df_mapeamento_setores
  ) |>
    dplyr::mutate(cd_uorg = as.character(cd_uorg))

  tabela_dados_final <- tabela_dados_final |>
    dplyr::left_join(populacao_sidra |> dplyr::select(cd_uorg, total_pi), by = "cd_uorg")

  # --- Área, população urbana/rural e demais variáveis v0* do Censo: atribui cada setor
  # censitário à CR que a malha básica já definiu para o seu território (TI ou Município), por
  # join de atributo -- reaproveita a associação setor -> TI já calculada com limiar de ruído de
  # borda por mapear_setores_ti() (alvo `df_setores_em_ti`), em vez de repetir um join espacial
  # (st_point_on_surface() + st_join()) que reconstrói uma lógica já resolvida em outro lugar do
  # pipeline. A TI prevalece sobre o Município onde as duas colidem, replicando exatamente a regra
  # de sobreposição aplicada em calcular_malha_basica_bruta().
  malha_bruta_attr <- sf::st_read(arq_malha_basica_bruta, quiet = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(cd_territorio = as.character(cd_territorio))

  ti_para_cr <- malha_bruta_attr |>
    dplyr::filter(tipo_territorio == "Terra Indígena") |>
    dplyr::transmute(id_ti = as.integer(cd_territorio), cd_uorg_ti = cd_uorg)

  mun_para_cr <- malha_bruta_attr |>
    dplyr::filter(tipo_territorio == "Município") |>
    dplyr::transmute(cd_mun = cd_territorio, cd_uorg_mun = cd_uorg)

  # Um setor pode ter mais de uma TI empatada em pct_sc_contido (raro); mantém a primeira para
  # preservar a exclusividade setor -> CR.
  setor_ti <- df_setores_em_ti |>
    dplyr::distinct(cd_setor, .keep_all = TRUE) |>
    dplyr::transmute(cd_setor = as.character(cd_setor), id_ti)

  setor_cr <- arq_setores_censitarios |>
    sf::st_drop_geometry() |>
    dplyr::transmute(cd_setor = as.character(cd_setor), cd_sit, cd_mun = as.character(cd_mun)) |>
    dplyr::left_join(setor_ti, by = "cd_setor") |>
    dplyr::left_join(ti_para_cr, by = "id_ti") |>
    dplyr::left_join(mun_para_cr, by = "cd_mun") |>
    dplyr::mutate(cd_uorg = dplyr::coalesce(cd_uorg_ti, cd_uorg_mun))

  # Variáveis brutas dos 4 indicadores de taxa por CR (registro civil, alfabetização,
  # saneamento, água) consumidos em _targets.R via criar_mapa_coropletico_cr(). Para essas
  # variáveis -- ao contrário do total_pi/demais colunas v0*, onde tratar o sigilo do IBGE ("X")
  # como 0 apenas subestima uma soma -- o mesmo "X" -> 0 distorce a TAXA em qualquer direção:
  # se a variável suprimida está no numerador, a taxa cai artificialmente; se está no
  # denominador, sobe. A correção correta não é trocar "X" por NA (equivalente a 0 numa soma com
  # na.rm = TRUE), e sim excluir o setor inteiro -- numerador e denominador -- do cálculo daquele
  # indicador específico quando qualquer uma de suas variáveis estiver sigilosa naquele setor.
  # Ver nota explicativa em nt_jurisdicoes.qmd, seção "Limitações da Proposta por Setores
  # Censitários".
  cols_reg  <- c("v02200", "v02201", "v02202")
  cols_alf  <- c("v02101", "v02137")
  cols_esg  <- paste0("v0", 1664:1671)
  cols_agua <- paste0("v0", 1558:1565)
  # Únicas colunas v0* de fato consumidas a partir daqui (os 4 indicadores de taxa + total_pi
  # urbano/rural, via v01690) -- nenhum outro `v0*` chega a ser lido por um nome literal ou por
  # dplyr::starts_with("v0") em nenhum consumidor downstream (_targets.R, montar_dados_relatorio_cr()
  # em R/relatorio_cr.R, ou os .qmd). Por isso a conversão/soma abaixo usa dplyr::all_of() com essa
  # lista, em vez de starts_with("v0") sobre as ~1029 colunas v0* da tabela agregada do Censo.
  cols_v0_usadas <- c(cols_reg, cols_alf, cols_esg, cols_agua, "v01690")

  agg_sc_v0 <- agg_sc |>
    dplyr::mutate(
      valido_reg  = setor_sem_sigilo(agg_sc, cols_reg),
      valido_alf  = setor_sem_sigilo(agg_sc, cols_alf),
      valido_esg  = setor_sem_sigilo(agg_sc, cols_esg),
      valido_agua = setor_sem_sigilo(agg_sc, cols_agua),
      cd_setor = as.character(cd_setor),
      dplyr::across(dplyr::all_of(cols_v0_usadas), limpar_sigilo_ibge)
    )

  agg_por_cr <- setor_cr |>
    dplyr::left_join(agg_sc_v0, by = "cd_setor") |>
    dplyr::mutate(flag_urbano = cd_sit %in% c(1, 2, 3)) |>
    dplyr::group_by(cd_uorg) |>
    dplyr::summarise(
      total_pi_urbana = sum(v01690[flag_urbano], na.rm = TRUE),
      total_pi_rural = sum(v01690[!flag_urbano], na.rm = TRUE),

      # Numerador/denominador dos 4 indicadores de taxa, somados só sobre setores sem sigilo nas
      # variáveis daquele indicador -- precisa vir ANTES do across(all_of(cols_v0_usadas), sum)
      # logo abaixo, que sobrescreve v02200/v01664/etc. pelos totais já agregados da CR.
      n_setores_validos_reg = sum(valido_reg, na.rm = TRUE),
      den_reg = sum((v02200 + v02201 + v02202)[valido_reg], na.rm = TRUE),
      num_reg = sum((v02201 + v02202)[valido_reg], na.rm = TRUE),

      n_setores_validos_alf = sum(valido_alf, na.rm = TRUE),
      den_alf = sum(v02101[valido_alf], na.rm = TRUE),
      num_alf = sum(v02137[valido_alf], na.rm = TRUE),

      n_setores_validos_esg = sum(valido_esg, na.rm = TRUE),
      den_esg = sum((v01664 + v01665 + v01666 + v01667 + v01668 + v01669 + v01670 + v01671)[valido_esg], na.rm = TRUE),
      num_esg = sum((v01667 + v01668 + v01669)[valido_esg], na.rm = TRUE),

      n_setores_validos_agua = sum(valido_agua, na.rm = TRUE),
      den_agua = sum((v01558 + v01559 + v01560 + v01561 + v01562 + v01563 + v01564 + v01565)[valido_agua], na.rm = TRUE),
      num_agua = sum(v01564[valido_agua], na.rm = TRUE),

      dplyr::across(dplyr::all_of(cols_v0_usadas), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  tabela_dados_final <- tabela_dados_final |>
    dplyr::left_join(agg_por_cr, by = "cd_uorg") |>
    dplyr::mutate(total_area_km2 = area_ha / 100)

  # --- Indicadores derivados -- idêntico a tabela_dados_sc() ---
  tabela_dados_final <- tabela_dados_final |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("autorizado_"), ~ dplyr::coalesce(.x, 0))) |>
    dplyr::mutate(
      area_por_cce = ifelse(total_pts_cce == 0, NA_real_, total_area_km2 / total_pts_cce),

      autorizado_por_pi_2023 = ifelse(total_pi == 0, NA_real_, autorizado_2023 / total_pi),
      autorizado_por_pi_2024 = ifelse(total_pi == 0, NA_real_, autorizado_2024 / total_pi),
      autorizado_por_pi_2025 = ifelse(total_pi == 0, NA_real_, autorizado_2025 / total_pi),
      autorizado_por_pi_2026 = ifelse(total_pi == 0, NA_real_, autorizado_2026 / total_pi),

      autorizado_por_pi_rural_2023 = ifelse(total_pi_rural == 0, NA_real_, autorizado_2023 / total_pi_rural),
      autorizado_por_pi_rural_2024 = ifelse(total_pi_rural == 0, NA_real_, autorizado_2024 / total_pi_rural),
      autorizado_por_pi_rural_2025 = ifelse(total_pi_rural == 0, NA_real_, autorizado_2025 / total_pi_rural),
      autorizado_por_pi_rural_2026 = ifelse(total_pi_rural == 0, NA_real_, autorizado_2026 / total_pi_rural),

      autorizado_por_pi_urbano_2023 = ifelse(total_pi_urbana == 0, NA_real_, autorizado_2023 / total_pi_urbana),
      autorizado_por_pi_urbano_2024 = ifelse(total_pi_urbana == 0, NA_real_, autorizado_2024 / total_pi_urbana),
      autorizado_por_pi_urbano_2025 = ifelse(total_pi_urbana == 0, NA_real_, autorizado_2025 / total_pi_urbana),
      autorizado_por_pi_urbano_2026 = ifelse(total_pi_urbana == 0, NA_real_, autorizado_2026 / total_pi_urbana)
    )

  tabela_dados_final |>
    dplyr::relocate(dplyr::starts_with("v0"), .after = dplyr::last_col()) |>
    dplyr::relocate(geom, .after = dplyr::last_col())
}

tabela_dados_municipio <- function(
    arq_final,
    arq_final_sc,
    agg_mun,
    arq_siorg,
    arq_siorg_estrutura,
    arq_siop,
    arq_cargos,
    arq_ug,
    arq_cadu,
    arq_etnias,
    arq_linguas,
    arq_bdqueimadas,
    arq_tis,
    arq_malha_basica_bruta,
    tb_pop_ti_sidra,
    tb_pop_mun_fora_ti_sidra,
    df_tis_ibge,
    df_sc_agregado,
    df_mapeamento_setores
){
  
  # arq_final = tar_read(arquivo_final_sc_v3)
  # arq_final_sc = tar_read(arquivo_gpkg_suavizado_v3)
  # agg_mun = tar_read(tb_pop_pi)
  # arq_siorg = tar_read(arq_siorg)
  # arq_siorg_estrutura = tar_read(arq_siorg_estrutura)
  # arq_siop = tar_read(arq_siop)
  # arq_cargos = tar_read(arq_cargos)
  # arq_ug = tar_read(arq_uorg_ug)
  # arq_cadu = tar_read(arq_cadu)
  # arq_etnias = tar_read(arq_etnias)
  # arq_linguas = tar_read(arq_linguas)
  # arq_bdqueimadas <- tar_read(arq_bdqueimadas)
  # arq_malha_basica_bruta = tar_read(malha_basica_bruta)
  # tb_pop_ti_sidra = tar_read(tb_pop_ti)
  # tb_pop_mun_fora_ti_sidra = tar_read(tb_pop_mun_fora_ti)
  # df_tis_ibge = tar_read(arq_tis_ibge) |> sf::st_drop_geometry()
  # df_sc_agregado = tar_read(censo_agregado_sc_pi_dados)
  # df_mapeamento_setores = tar_read(mapeamento_setores_estimativa)
  
  arq_final = sf::st_read(arq_final, quiet = T) |> dplyr::mutate(sigla = trimws(sigla))
  arq_bdqueimadas <- sf::st_transform(arq_bdqueimadas, sf::st_crs(arq_final))
  arq_tis <- sf::st_transform(arq_tis, sf::st_crs(arq_final))
  
  colnames(arq_etnias) <- c('cd_uf', 'nm_uf', 'cd_mun', 'nm_mun', 'cd_etnia', 'nm_etnia', 'qt_pessoas')
  
  arq_siop <- arq_siop |> filter(ano == 2025)
  
  dados_orcamento <- arq_siop |> group_by(ug_resp) |>
    summarise(autorizado_2025 = sum(autorizado, na.rm = T)) |>
    left_join(
      arq_ug |> mutate(
        ug = as.character(ug),
        cd_uorg = as.character(cd_uorg)
        ),
      by = join_by(ug_resp == ug)
    ) 
  
  vetor_crs <- arq_siorg |>
    dplyr::filter(grepl('^CR-', trimws(sigla))) |>
    dplyr::select(codigoUnidade) |>
    dplyr::pull()
  
  cr_unidade <- arq_siorg |> 
    dplyr::filter(codigoUnidade %in% vetor_crs | codigoUnidadePai %in% vetor_crs) |>
    dplyr::mutate(coord_regional = dplyr::if_else(codigoUnidadePai == 173, codigoUnidade, codigoUnidadePai)) |>
    dplyr::select(coord_regional, codigoUnidade, nome, sigla) |>
    dplyr::left_join(
      arq_siorg |> dplyr::select(codigoUnidade, nome_cr = nome, sigla_cr = sigla) |> dplyr::distinct(),
      by = dplyr::join_by(coord_regional == codigoUnidade)
    ) |> 
    dplyr::select(codigoUnidade, coord_regional, nome_cr, sigla_cr)
  
  vetor_unidades <- arq_siorg |> dplyr::filter(codigoUnidade %in% vetor_crs | codigoUnidadePai %in% vetor_crs) |> dplyr::select(codigoUnidade) |> dplyr::pull()
  
  unidades_cr <- arq_siorg_estrutura |> dplyr::filter(`Código Unidade` %in% vetor_unidades) |>
    dplyr::mutate(
      cargo = paste(`Tipo do Cargo`, Categoria, sprintf("%02d", Nível))
    ) |>
    dplyr::left_join(
      arq_cargos,
      by = dplyr::join_by(cargo)
    ) |>
    dplyr::left_join(
      cr_unidade |> mutate(codigoUnidade = as.integer(codigoUnidade)) |> 
        dplyr::distinct(),
      by = dplyr::join_by(`Código Unidade` == codigoUnidade)
    )
  
  tabela_dados <- unidades_cr |>
    dplyr::group_by(coord_regional, nome_cr, sigla_cr) |>
    dplyr::summarise(
      total_pts_cce = sum(
        cce_unitario * Quantidade
        , na.rm = T), .groups = 'drop'
    ) |>
    dplyr::mutate(coord_regional = as.character(coord_regional)) |>
    left_join(
      dados_orcamento |> select(-ug_ds),
      by = join_by(coord_regional == cd_uorg)
    ) |>
    select(coord_regional, ug_resp, sigla_cr, total_pts_cce, autorizado_2025)
  
  tabela_dados_final <- arq_final |>
    left_join(
      tabela_dados |> select(-sigla_cr),
      by = join_by(cd_uorg == coord_regional)
    )

  # total_pi (geometria/setor) é substituído pelo método híbrido SIDRA (TI oficial + município
  # "fora de TI"), evitando dupla contagem -- ver calcular_populacao_indigena_cr_sidra(). Área,
  # população urbana/rural continuam vindo da agregação por setor (não fazem parte desta troca).
  populacao_sidra <- calcular_populacao_indigena_cr_sidra(
    arq_malha_basica_bruta = arq_malha_basica_bruta,
    tb_pop_ti_sidra = tb_pop_ti_sidra,
    tb_pop_mun_fora_ti_sidra = tb_pop_mun_fora_ti_sidra,
    df_tis_ibge = df_tis_ibge,
    df_sc_agregado = df_sc_agregado,
    df_mapeamento_setores = df_mapeamento_setores
  )

  tabela_dados_final <- tabela_dados_final |>
    dplyr::select(-total_pi) |>
    dplyr::left_join(populacao_sidra, by = "cd_uorg")

  df <- sf::st_read(
    arq_final_sc,
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
                       FROM {sf::st_layers(arq_final_sc)$name}"),
    geometry_column = NA
  )
  
  
  # TABELA DE MUNICIPIOS E NUMERO DE MUNICÍPIOS COMPARTILHADOS
  tb_mun <- df |>
    distinct(cd_setor, .keep_all = TRUE) |>
    group_by(cd_mun) |>
    mutate(
      n_uorgs = n_distinct(cd_uorg),
      unico = max(n_uorgs) == 1,
      area_mun = sum(area_km2, na.rm = T)
    ) |>
    ungroup() |>
    group_by(cd_uorg, nome_uorg, sigla, cd_mun, unico, area_mun) |>
    summarise(
      area_uorg_mun = sum(area_km2, na.rm = T),
      .groups = 'drop'
    ) |>
    mutate(
      area_compartilhada_km2 = if_else(
        !unico,
        area_mun - area_uorg_mun,
        0
      )) |> ungroup() |>
    group_by(cd_uorg, nome_uorg, sigla) |>
    summarise(
      n_mun = n_distinct(cd_mun),
      n_mun_compartilhados = n_distinct(cd_mun[!unico]),
      n_mun_n_compartilhados = n_distinct(cd_mun[unico]),
      area_total_km2 = sum(area_mun, na.rm = TRUE),
      area_compartilhada_km2 = sum(area_compartilhada_km2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      pct_compartilhado = round(n_mun_compartilhados * 100 / n_mun, 2),
      pct_area_compartilhada = round(area_compartilhada_km2 * 100 / area_total_km2, 4)
    )
  
  # OBTEM RELAÇÃO ATRIBUINDO MUNICÍPIO A CR ÚNICA E JUNTANDO DADOS DA DESAGREGAÇÃO MUNICIPAL (MENOR PERDA POR ANONIMIZAÇÃO)
  # arq_cadu (indigenR::visdata_importa_racor_indigena()) traz codigo_ibge no formato de 6 dígitos
  # com ponto decorativo (ex. "120.001", equivalente ao cd_mun de 7 dígitos sem o dígito
  # verificador) -- remove o ponto para juntar com cd_mun_cad, e filtra pelo período mais recente
  # em vez de um literal fixo (referencia muda a cada atualização do CadÚnico).
  cr_mun <- df |> st_drop_geometry() |> arrange(desc(area_km2)) |>
    distinct(cd_mun, .keep_all = T) |>
    distinct(cd_uorg, nm_mun, cd_mun) |>
    mutate(
      cd_mun_cad = substr(cd_mun, 1, nchar(cd_mun) - 1)
    ) |>
    left_join(
      arq_cadu |>
        filter(referencia == max(referencia)) |>
        transmute(
          cd_mun_cad = gsub("\\.", "", codigo_ibge),
          qtd_indigenas_cad = n_pess_indigena_cad,
          qtd_familias_indigenas_cad = n_fam_indigena_cad
        ),
      by = join_by(cd_mun_cad)
    ) |> select(-cd_mun_cad) |>
    left_join(
      agg_mun |> slice(-1) |> select(cd_mun = D1C, pop_pi_mun = V) |> mutate(cd_mun = as.character(cd_mun), pop_pi_mun = as.integer(pop_pi_mun)),
      by = join_by(cd_mun == cd_mun)
    )
  
  # cr_mun |> group_by(cd_mun, nm_mun) |> 
  #   mutate(N = n()) |> filter(N > 1)
  
  # PREPARANDO DADOS DE ETNIAS E LINGUAS
  
  arq_etnias_cr <- arq_etnias |>
    left_join(
      cr_mun |> 
        select(cd_mun, cd_uorg) |> 
        mutate(cd_mun = as.numeric(cd_mun)),
      by = join_by(cd_mun)
    ) |>
    group_by(cd_uorg) |>
    summarise(
      qtd_etnias = n_distinct(cd_etnia),
      qtd_etnias_15 = n_distinct(cd_etnia[qt_pessoas >= 15])
    ) |>
    filter(!is.na(cd_uorg))
  
  arq_linguas_cr <- arq_linguas |>
    left_join(
      cr_mun |>
        select(cd_mun, cd_uorg) |>
        mutate(cd_mun = as.numeric(cd_mun)),
      by = join_by(cd_mun)
    ) |>
    group_by(cd_uorg) |>
    summarise(
      qtd_linguas = n_distinct(cd_lingua),
      qtd_linguas_15 = n_distinct(cd_lingua[qtd >= 15])
    ) |>
    filter(!is.na(cd_uorg))
  
  
  cr_mun <- cr_mun |> group_by(cd_uorg) |>
    summarise(
      qtd_indigenas_cad = sum(qtd_indigenas_cad, na.rm = TRUE),
      qtd_familias_indigenas_cad = sum(qtd_familias_indigenas_cad, na.rm = TRUE),
      total_pi_mun = sum(pop_pi_mun, na.rm = TRUE)
    )
  
  # CONSOLIDANDO DADOS NA TABELA FINAL
  tabela_dados_final <- tabela_dados_final |>
    left_join(
      cr_mun,
      by = join_by(cd_uorg)
    ) |>
    left_join(
      tb_mun |> select(-sigla, -nome_uorg),
      by = join_by(cd_uorg)
    )
  
  tabela_dados_final <- tabela_dados_final |> left_join(
    arq_etnias_cr,
    by = join_by(cd_uorg)
  ) |>
    left_join(
      arq_linguas_cr,
      by = join_by(cd_uorg)
    )
  
  # DADOS DE QUEIMADAS
  # DADOS DE QUEIMADAS
  # FOGOD DE FOGO POR ÁREA DA CR (arq_final_sc_v2)
  focos_fogo_cr <- sf::st_join(
    arq_bdqueimadas,
    arq_final,
    join = sf::st_within,
    left = FALSE
  ) |> st_drop_geometry() |> 
    group_by(
      cd_uorg
    ) |>
    summarise(
      n_focos = n()
    )
  
  focos_fogo_ti <- sf::st_join(
    arq_bdqueimadas,
    arq_tis |> select(terrai_nome, terrai_codigo),
    join = sf::st_within,
    left = FALSE
  ) |> sf::st_join(
    arq_final,
    join = sf::st_within,
    left = FALSE
  ) |> st_drop_geometry() |>
    group_by(
      cd_uorg
    ) |>
    summarise(
      n_focos_em_ti = n()
    )
  
  tb_focos_fogo <- left_join(
    focos_fogo_cr,
    focos_fogo_ti,
    by = join_by(cd_uorg)
  ) |> mutate(
    across(starts_with('n_'), dplyr::coalesce, 0)
  )
  
  tabela_dados_final <- tabela_dados_final |> left_join(
    tb_focos_fogo,
    by = join_by(cd_uorg)
  )
  
  tabela_dados_final <- tabela_dados_final |>
    mutate(
      autorizado_por_pi = autorizado_2025 / total_pi,
      area_por_cce = total_area_km2 / total_pts_cce,
      pct_cadunico = qtd_indigenas_cad / total_pi_mun
    )
  
  return(tabela_dados_final)
}



#' @title Gerar Gráfico de Situação Atual da População Indígena (SII)
#' @description Consolida dados oficiais do Censo 2022 e estimativas por setores censitários para TIs não oficializadas.
#' @param df_sc_agregado Tibble/Data.frame com os dados agregados dos setores (censo_agregado_sc_pi).
#' @param sf_tis Objeto sf com todas as Terras Indígenas (arq_tis).
#' @param df_tis_ibge De-para de códigos TI FUNAI vs IBGE (arq_tis_ibge).
#' @param tb_pop_ti_sidra Tabela bruta de população do SIDRA (tb_pop_ti).
#' @param df_mapeamento_setores Resultado da função mapear_setores_ti().
grafico_situacao_atual_sii <- function(
    df_sc_agregado, 
    sf_tis, 
    df_tis_ibge, 
    tb_pop_ti_sidra, 
    df_mapeamento_setores
) {
  
  # df_sc_agregado = targets::tar_read(censo_agregado_sc_pi_dados) 
  # sf_tis = targets::tar_read(arq_tis)
  # df_tis_ibge = targets::tar_read(arq_tis_ibge) |> sf::st_drop_geometry()
  # tb_pop_ti_sidra = targets::tar_read(tb_pop_ti)
  # df_mapeamento_setores = targets::tar_read(mapeamento_setores_estimativa
  
  # 1. PREPARAÇÃO DOS DADOS
  df_tis_base <- sf_tis |> sf::st_drop_geometry()
  df_tis_ibge_clean <- df_tis_ibge |> 
    dplyr::select(cd_ti_ibge = cd_ti, cd_funai) |> 
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))
  
  df_tis_identificadas <- df_tis_base |>
    dplyr::left_join(df_tis_ibge_clean, by = dplyr::join_by(terrai_codigo == cd_funai))
  
  # 2. POPULAÇÃO OFICIAL (IBGE 2022)
  df_pop_oficial <- df_tis_identificadas |>
    dplyr::filter(!is.na(cd_ti_ibge)) |>
    dplyr::left_join(
      tb_pop_ti_sidra |> 
        dplyr::filter(D1C == 1432) |> 
        dplyr::select(cd_ti = D2C, pop_ti_oficial = V) |> 
        dplyr::mutate(cd_ti = as.integer(cd_ti)),
      by = dplyr::join_by(cd_ti_ibge == cd_ti)
    ) |>
    dplyr::mutate(
      pop_indigena = limpar_sigilo_ibge(pop_ti_oficial),
      metodo_estimativa = "Agregado por Terra Indígena"
    )
  
  # 3. POPULAÇÃO ESTIMADA (POR SETORES CENSITÁRIOS)
  tis_para_estimar <- df_tis_identificadas |> 
    dplyr::filter(is.na(cd_ti_ibge)) |> 
    dplyr::pull(terrai_codigo)
  
  if (is.character(df_mapeamento_setores) && length(df_mapeamento_setores) == 1) {
    if (grepl("\\.(gpkg|shp)$", df_mapeamento_setores)) {
      df_mapeamento_setores <- sf::st_read(df_mapeamento_setores, quiet = TRUE)
    } else {
      df_mapeamento_setores <- data.table::fread(df_mapeamento_setores) |> as.data.frame()
    }
  }
  
  if (inherits(df_mapeamento_setores, "sf")) {
    df_mapeamento_setores <- sf::st_drop_geometry(df_mapeamento_setores)
  }
  
  colunas_ti_possiveis <- c("terrai_codigo", "cod_ti", "cd_ti", "codigo_ti", "cd_funai", "id_ti")
  nome_col_ti <- intersect(names(df_mapeamento_setores), colunas_ti_possiveis)
  
  if (length(nome_col_ti) == 0) {
    rlang::abort(paste(
      "A tabela de mapeamento de setores não possui uma coluna reconhecida para a TI.",
      "Colunas disponíveis:", paste(names(df_mapeamento_setores), collapse = ", ")
    ))
  }
  
  colunas_pop_censo <- c("v01690", "v0001", "v02101")
  nome_col_pop <- intersect(names(df_sc_agregado), colunas_pop_censo)
  
  if (length(nome_col_pop) == 0) {
    rlang::abort("Não foi encontrada uma coluna de população (v01690, v0001) nos agregados do IBGE.")
  }
  
  df_mapeamento_limpo <- df_mapeamento_setores |>
    dplyr::rename(terrai_codigo = dplyr::all_of(nome_col_ti[1]))
  
  pop_estimada_sc <- df_sc_agregado |>
    dplyr::mutate(cd_setor = as.character(cd_setor)) |>
    dplyr::inner_join(df_mapeamento_limpo, by = dplyr::join_by(cd_setor)) |>
    dplyr::filter(terrai_codigo %in% tis_para_estimar) |>
    dplyr::group_by(terrai_codigo) |>
    dplyr::summarise(
      pop_indigena = sum(as.numeric(.data[[nome_col_pop[1]]]), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(metodo_estimativa = "Estimativa por Setores Censitários")
  
  df_pop_estimada <- df_tis_identificadas |>
    dplyr::filter(terrai_codigo %in% tis_para_estimar) |>
    dplyr::left_join(pop_estimada_sc, by = dplyr::join_by(terrai_codigo)) |>
    dplyr::mutate(
      pop_indigena = dplyr::coalesce(pop_indigena, 0),
      metodo_estimativa = dplyr::coalesce(metodo_estimativa, "Estimativa por Setores Censitários")
    )
  
  # 4. CONSOLIDAÇÃO E PREPARAÇÃO DO GRÁFICO
  df_final <- dplyr::bind_rows(df_pop_oficial, df_pop_estimada)
  
  resumo_plot <- df_final |>
    dplyr::filter(!is.na(undadm_sigla) & undadm_sigla != "NA") |> 
    dplyr::group_by(undadm_sigla, metodo_estimativa) |>
    dplyr::summarise(
      pop_segmento = sum(pop_indigena, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(undadm_sigla) |>
    dplyr::mutate(
      pop_total_cr = sum(pop_segmento),
      metodo_estimativa = factor(
        metodo_estimativa, 
        levels = c("Estimativa por Setores Censitários", "Agregado por Terra Indígena")
      )
    ) |>
    dplyr::ungroup()
  
  # Base auxiliar de totais para evitar que o geom_text plote o texto duplicado por segmento
  df_totais <- resumo_plot |>
    dplyr::distinct(undadm_sigla, pop_total_cr)
  
  # 5. GERAÇÃO DO GRÁFICO OTIMIZADO
  cores_funai <- c(
    "Agregado por Terra Indígena" = CORES_FUNAI[["principal"]],
    "Estimativa por Setores Censitários" = CORES_FUNAI[["estimativa_sc"]]
  )
  
  caption_1 <- stringr::str_wrap(
    "Elaboração própria com dados do SII e os Resultados do Universo e dos Agregados por Setor Censitário (IBGE, 2022).",
    width = 115
  )
  
  caption_2 <- stringr::str_wrap(
    "Para as Terras Indígenas delimitadas que não entraram na publicação oficial do Censo Demográfico de 2022, foi realizada uma estimativa com base nos dados dos setores censitários que intersectam ou estão contidos no polígono da respectiva Terra Indígena.",
    width = 150
  )
  
  caption_text <- paste0(
    caption_1,
    "\n",
    caption_2
  )
  
  # grafico_populacao_sii$labels$caption <- caption_text
  # grafico_populacao_sii + theme(
  #   plot.caption = element_text(
  #     hjust = 0,        # <<< tudo à direita
  #     lineheight = 1.2
  #   )
  # )
  
  g <- ggplot2::ggplot(resumo_plot) +
    ggplot2::aes(
      x = stats::reorder(undadm_sigla, pop_total_cr), 
      y = pop_segmento, 
      fill = metodo_estimativa
    ) +
    # Desenha os blocos empilhados (mantém cores originais)
    ggplot2::geom_col(width = 0.7) +
    
    # Adiciona o rótulo com o TOTAL consolidado estritamente ao lado externo da barra
    ggplot2::geom_text(
      data = df_totais,
      ggplot2::aes(
        x = undadm_sigla,
        y = pop_total_cr,
        label = scales::label_number(
          big.mark = ".",
          decimal.mark = ","
        )(round(pop_total_cr)),
        fill = NULL # Limpa herança de preenchimento para evitar avisos no console
      ),
      hjust = -0.15,      # Desloca sutilmente para a direita do fim da barra
      color = "black",
      size = 3,
      fontface = "bold"
    ) +
    
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = cores_funai) +
    
    # Expande a área contínua em 15% à direita para evitar que os totais das barras fiquem cortados
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    
    ggplot2::labs(
      title = "População Indígena em Terras Indígenas por Coordenação Regional",
      subtitle = "Distribuição da população indígena em terras indígenas por Coordenação Regional de acordo com a vinculação no SII",
      x = NULL,
      y = "População Indígena",
      fill = "Origem do Dado",
      caption = caption_text
    ) +
    
    ggplot2::theme_minimal(base_size = 11) +
    obter_tema(tipo = 'barra', cor = CORES_FUNAI[["principal"]]) +
    theme(
      plot.caption = element_text(
        hjust = 0,        # <<< tudo à direita
        lineheight = 1.2
      )
    )
  
  exporta_grafico(g, "situacao_atual_sii")
  
  return(g)
}

#' @title Gráfico de dispersão genérico com linha de tendência
#' @description Cria um gráfico de dispersão relacionando duas variáveis numéricas, 
#' adicionando rótulos aos pontos (com repulsão visual) e uma linha de tendência linear (lm).
#' @param df_plot Data.frame ou objeto sf contendo os dados.
#' @param var_x Variável do eixo X (sem aspas).
#' @param var_y Variável do eixo Y (sem aspas).
#' @param var_label Variável categórica para os rótulos dos pontos (sem aspas). Default: sigla.
#' @param titulo,subtitulo,caption,xlab,ylab Elementos textuais do gráfico.
#' @param cor Cor principal dos pontos e rótulos.
#' @param arquivo Nome do arquivo para exportação.
#' @return Objeto ggplot.
criar_grafico_dispersao <- function(
    df_plot,
    var_x,
    var_y,
    var_label = sigla,
    titulo,
    subtitulo = NULL,
    caption = NULL,
    xlab = NULL,
    ylab = NULL,
    cor = CORES_FUNAI[["principal"]],
    arquivo = NULL
) {
  
  # Captura as variáveis nuas (sem aspas)
  var_x <- rlang::enquo(var_x)
  var_y <- rlang::enquo(var_y)
  var_label <- rlang::enquo(var_label)
  
  # Remove a geometria caso os dados entrem como objeto sf
  if (inherits(df_plot, "sf")) {
    df_plot <- sf::st_drop_geometry(df_plot)
  }
  
  # Filtra valores ausentes apenas nas variáveis que irão para os eixos
  df_plot <- df_plot |>
    dplyr::filter(!is.na(!!var_x), !is.na(!!var_y))
  
  g <- ggplot2::ggplot(df_plot) +
    ggplot2::aes(x = !!var_x, y = !!var_y, label = !!var_label) +
    
    # Reta de tendência linear no fundo
    ggplot2::geom_smooth(
      method = "lm", 
      se = FALSE, 
      color = "gray50", 
      linetype = "dashed", 
      linewidth = 0.8,
      alpha = 0.6
    ) +
    
    # Camada de Pontos
    ggplot2::geom_point(color = cor, size = 3.5, alpha = 0.8) +
    
    # Rótulos com algoritmo de repulsão (evita sobreposição)
    ggrepel::geom_label_repel(
      color = cor,
      label.size = 0,
      alpha = 0.3,
      size = 3,
      seed = 42
    ) +
    
    ggplot2::labs(
      title = titulo,
      subtitle = subtitulo,
      caption = caption,
      x = xlab,
      y = ylab
    ) +
    
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", color = cor),
      plot.subtitle = ggplot2::element_text(color = "grey40"),
      axis.text = ggplot2::element_text(color = "black")
    )
  
  if (!is.null(arquivo)) {
    exporta_grafico(g, arquivo)
  }
  
  return(g)
}


#' @title Gráfico de Calor: Orçamento Discricionário Descentralizado por PI
#' @param df_tabela Data.frame/sf com os dados de População Indígena (PI)
#' @param arq_siop Data.frame do SIOP
#' Não usada por nenhum alvo do pipeline no momento -- mantida fora de _targets.R para facilitar
#' eventual reinserção futura (ver mesmo raciocínio em tabela_dados_sc(), acima).
criar_grafico_orcamento_heatmap <- function(df_tabela, arq_siop) {

  # 1. Preparação da base de dados
  df_pi <- sf::st_drop_geometry(df_tabela) |>
    dplyr::filter(!is.na(total_pi) & total_pi > 0) |>
    dplyr::select(cd_uorg, ug_resp, nome_uorg, sigla, total_pi)

  df_plot <- arq_siop |>
    dplyr::group_by(ano, ug_resp) |>
    dplyr::summarise(
      autorizado = sum(autorizado, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::inner_join(df_pi, by = dplyr::join_by(ug_resp)) |>
    dplyr::mutate(
      autorizado_por_pi = autorizado / total_pi
    ) |>
    dplyr::filter(!is.na(autorizado_por_pi) & !is.na(ano)) |>
    # Ordenar o eixo Y pela média histórica
    dplyr::group_by(sigla) |>
    dplyr::mutate(media_historica = mean(autorizado_por_pi, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    # Determina a cor de contraste do rótulo com base na posição real de cada
    # valor na escala de cores viridis (mako) usada no preenchimento dos
    # azulejos, e não em relação à média bruta dos dados
    dplyr::mutate(
      posicao_escala = (autorizado_por_pi - min(autorizado_por_pi, na.rm = TRUE)) /
        (max(autorizado_por_pi, na.rm = TRUE) - min(autorizado_por_pi, na.rm = TRUE)),
      cor_tile = viridisLite::mako(
        n = 1000,
        begin = 0.1,
        direction = -1
      )[pmax(1, pmin(1000, round(posicao_escala * 999) + 1))],
      luminancia_tile = {
        rgb_tile <- grDevices::col2rgb(cor_tile) / 255
        0.2126 * rgb_tile["red", ] + 0.7152 * rgb_tile["green", ] + 0.0722 * rgb_tile["blue", ]
      },
      cor_texto = ifelse(luminancia_tile < 0.5, "white", "black")
    )

  # 2. Construção do Gráfico
  g <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(
      x = as.factor(ano),
      y = stats::reorder(sigla, media_historica),
      fill = autorizado_por_pi
    )
  ) +
    # Desenha os azulejos do heatmap
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +

    # Adiciona os rótulos de texto dentro de cada célula com CONTRASTE DINÂMICO
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::label_number(
          prefix = "R$ ",
          big.mark = ".",
          decimal.mark = ",",
          accuracy = 1
        )(autorizado_por_pi),

        # LÓGICA DE CONTRASTE: cor derivada da luminância real da cor
        # (escala viridis "mako") atribuída ao azulejo, calculada acima
        # em cor_texto - não mais a média bruta dos dados.
        color = cor_texto
      ),
      size = 3,
      fontface = "bold"
    ) +

    # FORÇA O GGPLOT A LER AS STRINGS "white" e "black" COMO CORES REAIS
    ggplot2::scale_color_identity() +

    # Escala de cores Viridis
    ggplot2::scale_fill_viridis_c(
      option = "mako",
      direction = -1,
      begin = 0.1,
      labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
    ) +

    # Textos e Títulos
    ggplot2::labs(
      title = "Evolução do Orçamento Descentralizado por Pessoa Indígena",
      subtitle = "Orçamento discricionário por indígena para cada CR por ano, desde 2023",
      x = "Ano",
      y = NULL,
      fill = "Orçamento\nper capita",
      caption = "Fonte: SIOP (RPs 2 a 9) e IBGE (2022)"
    ) +

    # Tema limpo
    ggplot2::theme_minimal(base_size = 11) +
    obter_tema(
      tipo = 'heatmap',
      cor = CORES_FUNAI[["principal"]]
    )
    # ggplot2::theme(
    #   panel.grid = ggplot2::element_blank(),
    #   plot.title = ggplot2::element_text(face = "bold", color = "#005a5a"),
    #   axis.text.y = ggplot2::element_text(size = 9, color = "black"),
    #   axis.text.x = ggplot2::element_text(size = 10, color = "black"),
    #   legend.position = "top",
    #   legend.key.width = ggplot2::unit(1.5, "cm")
    # )

  exporta_grafico(g, "heatmap_orcamento_descentralizado_por_pi")

  return(g)
}

#' @title Mapa coroplético evolutivo por Coordenação Regional
#' @description Gera uma série de mapas coropléticos em facetas para visualizar
#' a evolução temporal de um indicador entre Coordenações Regionais. Cada
#' variável informada em `variaveis` é convertida em uma faceta, permitindo a
#' comparação espacial do indicador entre diferentes anos ou períodos.
#'
#' @param df_plot Objeto sf contendo a geometria das Coordenações Regionais e
#' as variáveis a serem representadas nos mapas.
#' @param variaveis Vetor de nomes de colunas que serão transformadas em
#' facetas. Cada coluna representa um período distinto.
#' @param titulo Título principal do gráfico.
#' @param subtitulo Subtítulo do gráfico.
#' @param caption Texto exibido no rodapé do gráfico.
#' @param titulo_legenda Título da legenda da escala de cores.
#' @param cor Cor principal utilizada no gradiente de preenchimento.
#' @param prefixo Prefixo aplicado aos valores da legenda e dos rótulos (ex: "R$ ").
#' @param sufixo Sufixo aplicado aos valores da legenda e dos rótulos (ex: "%").
#' @param arquivo Nome do arquivo para exportação. Caso `NULL`, o gráfico não é exportado.
#' @param mostrar_sigla_cr Se `TRUE`, exibe as siglas das Coordenações Regionais.
#' @param tamanho_sigla_cr Tamanho do texto das siglas das Coordenações Regionais.
#' @param mostrar_valor Se `TRUE`, exibe os valores numéricos no centro de cada polígono.
#' @param tamanho_valor Tamanho do texto numérico exibido no mapa.
#' @param accuracy Precisão decimal para arredondamento dos rótulos numéricos (ex: 1, 0.1, 0.01).
#' @param nrow Número de linhas utilizadas na disposição das facetas.
#'
#' @return Objeto ggplot contendo o conjunto de mapas coropléticos facetados.
#'
#' @export
criar_mapa_coropletico_evolucao_cr <- function(
    df_plot,
    variaveis,
    titulo,
    subtitulo = NULL,
    caption = NULL,
    titulo_legenda,
    cor = CORES_FUNAI[["principal"]],
    prefixo = "",
    sufixo = "",
    arquivo = NULL,
    mostrar_sigla_cr = TRUE,
    tamanho_sigla_cr = 1.5,
    mostrar_valor = FALSE,
    tamanho_valor = 2.5,
    accuracy = 1,
    nrow = 2,
    tipo_malha = 'sc'
) {
  
  possui_labels <- !is.null(names(variaveis)) &&
    any(nzchar(names(variaveis)))
  
  if (possui_labels) {
    
    labels_faceta <- stats::setNames(
      names(variaveis),
      unname(variaveis)
    )
    
    variaveis <- unname(variaveis)
    
  } else {
    
    labels_faceta <- NULL
    
  }
  
  # Pivota os dados para formato longo (necessário para o facet_wrap)
  df_plot <- df_plot |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(variaveis),
        ~ dplyr::na_if(.x, 0)
      )
    ) |>
    dplyr::select(
      sigla,
      all_of(variaveis)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variaveis),
      names_to = "variavel",
      values_to = "valor"
    )
  
  # 1. Define qual função de centróides usar baseado no parâmetro
  centroides <- if (tipo_malha == "sc") {
    centroides_rotulos_sc(df_plot)
  } else if (tipo_malha == "mun") {
    centroides_rotulos_mun(df_plot) # Certifique-se de que esta função exista no seu script
  } else {
    rlang::abort("O parâmetro 'tipo_malha' deve ser 'sc' (Setores Censitários) ou 'mun' (Municípios).")
  }
  
  
  g <- ggplot2::ggplot(df_plot) +
    ggplot2::geom_sf(
      fill = "white",
      color = NA
    ) +
    
    ggplot2::geom_sf(
      ggplot2::aes(fill = valor),
      color = "white",
      linewidth = 0.2
    )
  
  # Sigla e valor combinados num único rótulo, com posição resolvida por ggrepel (respeitando
  # os painéis do facet_wrap) em vez de deslocamentos fixos por sigla -- ver mesma decisão em
  # criar_mapa_coropletico_cr().
  if (mostrar_sigla_cr || mostrar_valor) {
    centroides <- centroides |>
      dplyr::mutate(
        rotulo = dplyr::case_when(
          mostrar_sigla_cr && mostrar_valor ~ paste0(
            sigla, "\n",
            scales::label_number(prefix = prefixo, suffix = sufixo, big.mark = ".", decimal.mark = ",", accuracy = accuracy)(valor)
          ),
          mostrar_sigla_cr ~ sigla,
          mostrar_valor ~ scales::label_number(prefix = prefixo, suffix = sufixo, big.mark = ".", decimal.mark = ",", accuracy = accuracy)(valor),
          TRUE ~ NA_character_
        )
      )

    g <- g +
      # Mesmo redesenho de criar_mapa_coropletico_cr(): rótulo em "chip" sem borda dura, cantos
      # arredondados, texto na cor do mapa e linha guia numa versão translúcida dessa cor.
      ggrepel::geom_label_repel(
        data = centroides,
        ggplot2::aes(geometry = geometry, label = rotulo),
        stat = "sf_coordinates",
        size = if (mostrar_valor) tamanho_valor else tamanho_sigla_cr,
        color = cor,
        fontface = "bold",
        lineheight = 0.95,
        label.size = 0,
        label.r = grid::unit(0.35, "lines"),
        label.padding = grid::unit(0.25, "lines"),
        fill = scales::alpha("white", 0.9),
        seed = 42,
        max.overlaps = Inf,
        min.segment.length = 0,
        segment.size = 0.25,
        segment.color = scales::alpha(cor, 0.5)
      )
  }
  
  # Continuação da construção do gráfico (Facetas, Escalas e Tema)
  g <- g +
    
    ggplot2::facet_wrap(
      ~variavel,
      labeller = ggplot2::as_labeller(labels_faceta),
      nrow = nrow
    ) +
    
    ggplot2::scale_fill_gradient(
      low = extremo_claro_hcl(cor),
      high = cor,
      labels = scales::label_number(
        prefix = prefixo,
        suffix = sufixo,
        big.mark = ".",
        decimal.mark = ","
      ),
      name = titulo_legenda
    ) +
    
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        barwidth = grid::unit(8, "cm"),
        barheight = grid::unit(0.5, "cm"),
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    
    ggplot2::labs(
      title = titulo,
      subtitle = subtitulo,
      caption = caption
    ) +
    
    ggplot2::theme_minimal(base_size = 11) +
    
    obter_tema(
      tipo = "mapa",
      cor = cor
    ) +
    
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 12,
        color = cor
      ),
      legend.title = ggplot2::element_text(
        margin = ggplot2::margin(b = -4)
      ),
      # Ajuste útil para facetas de mapas: remove margens soltas
      plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
      panel.spacing = grid::unit(0, "lines")
    )
  
  if (!is.null(arquivo)) {
    exporta_grafico(g, arquivo)
  }
  
  return(g)
}



#' @title Mapa coroplético por Coordenação Regional
#' @description Cria um mapa coroplético com rótulos de sigla e valores.
#'
#' @param df_plot Objeto sf contendo a variável a ser mapeada.
#' @param variavel Nome da variável numérica (sem aspas).
#' @param titulo Título do gráfico.
#' @param subtitulo Subtítulo do gráfico.
#' @param caption Caption do gráfico.
#' @param titulo_legenda Título da legenda.
#' @param cor Cor principal da escala.
#' @param prefixo Prefixo dos valores exibidos.
#' @param sufixo Sufixo dos valores exibidos.
#' @param arquivo Nome do arquivo para exportação.
#' @param mostrar_sigla Exibe siglas das CRs.
#' @param mostrar_valor Exibe valores numéricos.
#' @param tamanho_sigla_cr Tamanho do texto das siglas.
#' @param tamanho_valor Tamanho do texto dos valores.
#' @param accuracy Precisão dos rótulos numéricos.
#'
#' @return Objeto ggplot.
criar_mapa_coropletico_cr <- function(
    df_plot,
    variavel,
    titulo,
    subtitulo = NULL,
    caption = NULL,
    titulo_legenda,
    cor = CORES_FUNAI[["verde_escuro"]],
    prefixo = "",
    sufixo = "",
    arquivo = NULL,
    mostrar_sigla = TRUE,
    mostrar_valor = TRUE,
    tamanho_sigla_cr = 2.5,
    tamanho_valor = 3,
    accuracy = 1,
    tipo_malha = 'sc'
) {
  
  variavel <- rlang::enquo(variavel)
  
  # 1. Define qual função de centróides usar baseado no parâmetro
  centroides_base <- if (tipo_malha == "sc") {
    centroides_rotulos_sc(df_plot)
  } else if (tipo_malha == "mun") {
    centroides_rotulos_mun(df_plot) # Certifique-se de que esta função exista no seu script
  } else {
    rlang::abort("O parâmetro 'tipo_malha' deve ser 'sc' (Setores Censitários) ou 'mun' (Municípios).")
  }
  
  # 2. Aplica a mutação normalmente. Sigla e valor são combinados num único rótulo (em vez de
  # duas camadas de geom_sf_text() com deslocamentos fixos por sigla): CRs pequenas/agrupadas
  # (ex.: Amazônia, na malha básica) ficam com centróides muito próximos, e um deslocamento fixo
  # calibrado para uma malha específica não generaliza para outra. ggrepel resolve a sobreposição
  # automaticamente, com uma linha guia até o centróide quando precisa afastar o rótulo.
  # CR sem amostra suficiente (ver LIMIAR_AMOSTRA_MINIMA_INDICADOR) chega como NA em `valor` --
  # mostra "sem dado" em vez do "NA" literal que scales::label_number() produziria.
  formatar_valor <- scales::label_number(prefix = prefixo, suffix = sufixo, big.mark = ".", decimal.mark = ",", accuracy = accuracy)

  centroides <- centroides_base |>
    dplyr::mutate(
      valor = !!variavel,
      valor_rotulo = dplyr::if_else(is.na(valor), "sem dado", formatar_valor(valor)),
      rotulo = dplyr::case_when(
        mostrar_sigla && mostrar_valor ~ paste0(sigla, "\n", valor_rotulo),
        mostrar_sigla ~ sigla,
        mostrar_valor ~ valor_rotulo,
        TRUE ~ NA_character_
      )
    )

  g <- ggplot2::ggplot(df_plot) +

    ggplot2::geom_sf(
      fill = "white",
      color = NA
    ) +

    ggplot2::geom_sf(
      ggplot2::aes(fill = !!variavel),
      color = "white",
      linewidth = 0.2
    )

  if (mostrar_sigla || mostrar_valor) {

    g <- g +
      # Rótulo em formato de "chip": sem borda dura, cantos bem arredondados, texto na própria
      # cor do mapa (em vez de cinza genérico) para amarrar visualmente rótulo e paleta -- e linha
      # guia numa versão translúcida da mesma cor, em vez de cinza neutro.
      ggrepel::geom_label_repel(
        data = centroides,
        ggplot2::aes(geometry = geometry, label = rotulo),
        stat = "sf_coordinates",
        size = tamanho_valor,
        color = cor,
        fontface = "bold",
        lineheight = 0.95,
        label.size = 0,
        label.r = grid::unit(0.35, "lines"),
        label.padding = grid::unit(0.25, "lines"),
        fill = scales::alpha("white", 0.9),
        seed = 42,
        max.overlaps = Inf,
        min.segment.length = 0,
        segment.size = 0.25,
        segment.color = scales::alpha(cor, 0.5)
      )

  }

  g <- g +

    ggplot2::scale_fill_gradient(
      low = extremo_claro_hcl(cor),
      high = cor,
      na.value = "grey85", # CR sem amostra suficiente (ver LIMIAR_AMOSTRA_MINIMA_INDICADOR)
      labels = scales::label_number(
        prefix = prefixo,
        suffix = sufixo,
        big.mark = ".",
        decimal.mark = ","
      ),
      name = titulo_legenda
    ) +
    
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        barwidth = grid::unit(8, "cm"),
        barheight = grid::unit(0.5, "cm"),
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    
    ggplot2::labs(
      title = titulo,
      subtitle = subtitulo,
      caption = caption
    ) +
    
    ggplot2::theme_minimal(base_size = 11) +
    
    obter_tema(
      tipo = "mapa",
      cor = cor
    ) +
    
    ggplot2::theme(
      legend.title = ggplot2::element_text(
        margin = ggplot2::margin(b = -4)
      ),
      plot.margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0,
        unit = "cm"
      )
    )
  
  if (!is.null(arquivo)) {
    exporta_grafico(g, arquivo)
  }
  
  g
  
}

#' @title Gera tabela de TIs inteiramente contidas e divididas por CR
#' @description Calcula a interseção geométrica para classificar TIs como contidas (>= 99%) ou divididas.
#' @param arq_poligonos_cr Objeto sf ou caminho do geopackage dos polígonos das CRs.
#' @param arq_tis Objeto sf contendo os polígonos das Terras Indígenas.
#' @return Caminho do arquivo xlsx gerado.
criar_tabela_tis_cr <- function(arq_poligonos_cr, arq_tis) {
  
  crs_calc <- CRS_ALBERS_BR
  
  cr_polys <- arq_poligonos_cr |> 
    sf::st_transform(crs_calc)
  
  tis <- arq_tis |>
    sf::st_transform(crs_calc)

  tis <- tis |>
    dplyr::mutate(area_ti = as.numeric(sf::st_area(tis)))

  # Calcula a interseção entre TIs e Jurisdições
  ints <- sf::st_intersection(
    tis,
    cr_polys |> dplyr::select(cd_uorg, sigla, nome_uorg)
  )

  ints <- ints |>
    dplyr::mutate(
      area_int = as.numeric(sf::st_area(ints)),
      pct_int = area_int / area_ti
    ) |>
    sf::st_drop_geometry()
  
  # Define status global da TI (Se em alguma parte pct_int < 0.99, ela é dividida)
  tis_status <- ints |>
    dplyr::group_by(terrai_codigo) |>
    dplyr::summarise(
      is_divided = max(pct_int, na.rm = TRUE) < 0.99
    )
  
  # Consolida a tabela por CR filtrando resíduos topológicos (áreas < 1%)
  tabela_final <- ints |>
    dplyr::filter(pct_int > 0.01) |>
    dplyr::left_join(tis_status, by = "terrai_codigo") |>
    dplyr::group_by(sigla) |>
    dplyr::summarise(
      tis_inteiramente_contidas = sum(!is_divided),
      tis_divididas = sum(is_divided),
      total_tis = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(sigla)
  
  # Adiciona linha de total
  tabela_final <- dplyr::bind_rows(
    tabela_final,
    dplyr::summarise(
      tabela_final,
      sigla = "TOTAL",
      tis_inteiramente_contidas = sum(tis_inteiramente_contidas, na.rm = TRUE),
      tis_divididas = sum(tis_divididas, na.rm = TRUE),
      total_tis = sum(total_tis, na.rm = TRUE)
    )
  )
  
  # Mantém apenas colunas de exibição
  tabela_excel <- tabela_final |>
    dplyr::select(
      sigla,
      tis_inteiramente_contidas,
      tis_divididas,
      total_tis
    )
  
  dir.create(DIR_TABELAS, showWarnings = FALSE)
  caminho_arquivo <- file.path(DIR_TABELAS, "tabela_tis_por_cr.xlsx")
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TIs por CR")
  
  # Estilos
  estilo_cabecalho <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    border = "TopBottomLeftRight",
    wrapText = TRUE,
    fgFill = "#D9EAD3"
  )
  
  estilo_dados <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    wrapText = TRUE
  )
  
  estilo_total <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    border = "TopBottom"
  )
  
  # Cabeçalhos multinível
  cab1 <- c(
    "Coordenação Regional",
    "Terras Indígenas",
    NA,
    NA
  )
  
  cab2 <- c(
    "",
    "Totalmente contidas",
    "Divididas",
    "Total"
  )
  
  openxlsx::writeData(
    wb,
    "TIs por CR",
    x = matrix(cab1, nrow = 1),
    startRow = 1,
    colNames = FALSE
  )
  
  openxlsx::writeData(
    wb,
    "TIs por CR",
    x = matrix(cab2, nrow = 1),
    startRow = 2,
    colNames = FALSE
  )
  
  # Mesclas
  openxlsx::mergeCells(wb, "TIs por CR", cols = 1, rows = 1:2)
  openxlsx::mergeCells(wb, "TIs por CR", cols = 2:4, rows = 1)
  
  # Dados
  openxlsx::writeData(
    wb,
    "TIs por CR",
    tabela_excel,
    startRow = 3,
    colNames = FALSE
  )
  
  n_linhas <- nrow(tabela_excel) + 2
  n_colunas <- ncol(tabela_excel)
  
  # Cabeçalhos
  openxlsx::addStyle(
    wb,
    "TIs por CR",
    estilo_cabecalho,
    rows = 1:2,
    cols = 1:n_colunas,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # Dados
  openxlsx::addStyle(
    wb,
    "TIs por CR",
    estilo_dados,
    rows = 3:n_linhas,
    cols = 1:n_colunas,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # Linha TOTAL em negrito
  linha_total <- n_linhas
  
  openxlsx::addStyle(
    wb,
    "TIs por CR",
    estilo_total,
    rows = linha_total,
    cols = 1:n_colunas,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # Autoajuste
  openxlsx::setColWidths(
    wb,
    "TIs por CR",
    cols = 1:n_colunas,
    widths = "auto"
  )
  
  # Altura dos cabeçalhos
  openxlsx::setRowHeights(
    wb,
    "TIs por CR",
    rows = 1:2,
    heights = 35
  )
  
  # Congela cabeçalho
  openxlsx::freezePane(
    wb,
    "TIs por CR",
    firstActiveRow = 3
  )
  
  openxlsx::saveWorkbook(
    wb,
    caminho_arquivo,
    overwrite = TRUE
  )
  
  return(caminho_arquivo)
}

#' @title Anexa o código SII (undadm_codigo) à tabela de correspondência UORG/UG
#' @description O sistema SII da FUNAI identifica a CR responsável por cada TI através de
#' `undadm_codigo` (coluna de `arq_tis`), cujo valor segue o padrão observado nos dados:
#' prefixo fixo "30202" seguido do código SIAPE da CR (`codigo_siape`) com 6 dígitos
#' (zero-padded à esquerda). Como `arq_uorg_ug` (dados/brutos/tb_uorg_ug.xlsx) não traz esse
#' código, ele é reconstruído aqui a partir do dicionário de CRs (dados/imputacoes/dicionario_cr.rds),
#' cujo `codigo_siorg` corresponde a `arq_uorg_ug$cd_uorg`. Some TIs trazem em `undadm_codigo` um
#' `codigo_siape` antigo, anterior a um recadastramento da CR no SIAPE -- `ALIASES_SIAPE_CR`
#' (R/config.R) soma esses códigos antigos aos vigentes, para que o join a jusante (via
#' `undadm_codigo == cd_sii`) resolva a TI pelo código antigo também, e ela não apareça como "sem
#' vinculação no SII" só por causa da defasagem de cadastro.
#' @param arq_uorg_ug Data.frame de correspondência cd_uorg/ug/ug_ds (tar_target `arq_uorg_ug`).
#' @param dicionario_cr Data.frame com o dicionário de CRs (tar_target `dicionario_cr`), contendo
#'   `codigo_siape` e `codigo_siorg`.
#' @return `arq_uorg_ug` com uma coluna adicional `cd_sii` (character); CRs com alias em
#'   `ALIASES_SIAPE_CR` aparecem em duas linhas (uma por `cd_sii` possível).
adicionar_cd_sii <- function(arq_uorg_ug, dicionario_cr) {

  cd_sii_por_uorg <- dicionario_cr |>
    dplyr::transmute(
      codigo_siorg = as.numeric(codigo_siorg),
      cd_sii = paste0("30202", sprintf("%06d", as.integer(codigo_siape)))
    ) |>
    dplyr::bind_rows(
      ALIASES_SIAPE_CR |>
        dplyr::transmute(
          codigo_siorg = as.numeric(codigo_siorg),
          cd_sii = paste0("30202", sprintf("%06d", as.integer(codigo_siape_antigo)))
        )
    ) |>
    dplyr::distinct()

  arq_uorg_ug |>
    dplyr::left_join(
      cd_sii_por_uorg,
      by = dplyr::join_by(cd_uorg == codigo_siorg)
    )
}

#' @title Gera tabela de concordância de TIs (Proposta vs SII)
#' @description Compara a alocação atual (SII) com a proposta espacial, quantificando
#' divergências antes (detalhando se a perda foi para CR nova ou existente) e depois.
#' @param arq_poligonos_cr Objeto sf contendo os polígonos das CRs (Proposta).
#' @param arq_tis Objeto sf contendo os polígonos das Terras Indígenas (com undadm_codigo).
#' @param arq_uorg_ug Tabela de correspondência para recuperar o cd_uorg a partir do código do SII.
#' @param dicionario_cr Dicionário de CRs usado para derivar `cd_sii` (ver `adicionar_cd_sii()`).
#' @return Caminho do arquivo xlsx gerado.
criar_tabela_concordancia_sii <- function(arq_poligonos_cr, arq_tis, arq_uorg_ug, dicionario_cr) {

  # arq_poligonos_cr = targets::tar_read(tabela_dados_final_malha_basica)
  # arq_tis = targets::tar_read(arq_tis)
  # arq_uorg_ug = targets::tar_read(arq_uorg_ug)
  # dicionario_cr = targets::tar_read(dicionario_cr)

  crs_calc <- CRS_ALBERS_BR
  cr_polys <- arq_poligonos_cr |> sf::st_transform(crs_calc)
  arq_uorg_ug <- adicionar_cd_sii(arq_uorg_ug, dicionario_cr)

  tis <- arq_tis |>
    sf::st_transform(crs_calc)

  tis <- tis |>
    dplyr::mutate(
      area_ti = as.numeric(sf::st_area(tis)),
      undadm_codigo = as.character(undadm_codigo)
      ) |>
    dplyr::left_join(
      arq_uorg_ug |> dplyr::select(cd_sii, uorg_ti_sii = cd_uorg),
      by = dplyr::join_by(undadm_codigo == cd_sii)
    ) |>
    dplyr::filter(

    )
  
  # Descobre a CR proposta baseando-se em onde a TI tem a maior área de intersecção
  ints <- sf::st_intersection(
    tis,
    cr_polys |> dplyr::select(cd_uorg_proposta = cd_uorg, sigla_cr_proposta = sigla)
  )

  ints <- ints |>
    dplyr::mutate(
      area_int = as.numeric(sf::st_area(ints)),
      pct_int = area_int / area_ti
      ) |>
    dplyr::filter(pct_int >= 0.99) |>
    sf::st_drop_geometry()
  
  # Vetor auxiliar de padronização das novas CRs
  novas_crs <- c("CR-RR II", "CR-PE", "CR-MJUR", "CR-MSOL", "CR-Mjur", "CR-MSol")
  
  ti_proposta <- ints |>
    dplyr::group_by(terrai_codigo, terrai_nome, uorg_ti_sii) |>
    dplyr::slice_max(area_int, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      uorg_ti_sii = as.character(uorg_ti_sii),
      cd_uorg_proposta = as.character(cd_uorg_proposta),
      divergente = (uorg_ti_sii != cd_uorg_proposta) | is.na(uorg_ti_sii),
      # Cria flag indicando se a CR de destino na proposta é uma CR nova
      destino_e_nova_cr = sigla_cr_proposta %in% novas_crs
    )
  
  # Contagem de divergências baseada na CR Antiga (Perspectiva de quem 'perdeu' a TI)
  
  conv_antes <- ti_proposta |>
    dplyr::filter(!divergente) |>
    dplyr::count(
      cd_uorg = uorg_ti_sii,
      name = "convergencias"
    ) |>
    dplyr::filter(!is.na(cd_uorg))
  
  div_antes <- ti_proposta |>
    dplyr::filter(divergente) |>
    dplyr::group_by(cd_uorg = uorg_ti_sii) |>
    dplyr::summarise(
      divergencias_total = dplyr::n(),
      div_para_cr_existente = sum(!destino_e_nova_cr, na.rm = TRUE),
      div_para_cr_nova = sum(destino_e_nova_cr, na.rm = TRUE)
    ) |>
    dplyr::filter(!is.na(cd_uorg))
  
  todas_crs <- cr_polys |> 
    sf::st_drop_geometry() |> 
    dplyr::select(cd_uorg, sigla, nome_uorg) |>
    dplyr::mutate(
      nova_cr_reestruturacao = dplyr::if_else(
        sigla %in% novas_crs, 
        "Sim", "Não"
      )
    )
  
  # Consolidação da Tabela Final
  tabela_final <- todas_crs |>
    dplyr::left_join(div_antes, by = "cd_uorg") |>
    dplyr::left_join(conv_antes, by = "cd_uorg") |>
    dplyr::mutate(
      convergencias = dplyr::coalesce(convergencias, 0L),
      divergencias_total = dplyr::coalesce(divergencias_total, 0L),
      div_para_cr_existente = dplyr::coalesce(div_para_cr_existente, 0L),
      div_para_cr_nova = dplyr::coalesce(div_para_cr_nova, 0L)
    ) |>
    dplyr::select(
      sigla,
      nome_uorg,
      nova_cr_reestruturacao,
      convergencias,
      divergencias_total,
      div_para_cr_existente,
      div_para_cr_nova
    ) |>
    dplyr::filter(divergencias_total > 0) |>
    dplyr::arrange(desc(divergencias_total), sigla)
  
  # Linha de total
  tabela_final <- dplyr::bind_rows(
    tabela_final,
    dplyr::summarise(
      tabela_final,
      sigla = "TOTAL",
      nome_uorg = NA_character_,
      nova_cr_reestruturacao = NA_character_,
      convergencias = sum(convergencias, na.rm = TRUE),
      divergencias_total = sum(divergencias_total, na.rm = TRUE),
      div_para_cr_existente = sum(div_para_cr_existente, na.rm = TRUE),
      div_para_cr_nova = sum(div_para_cr_nova, na.rm = TRUE)
    )
  )
  
  tabela_final <- tabela_final |>
    dplyr::rename(
      `Coordenação Regional` = sigla,
      `Criada na Reestruturação?` = nova_cr_reestruturacao,
      Convergente = convergencias,
      Divergente = divergencias_total,
      `Divergente p/ CR já existente` = div_para_cr_existente,
      `Divergente p/ CR Nova` = div_para_cr_nova
    ) |>
    dplyr::select(
      `Coordenação Regional`,
      `Criada na Reestruturação?`,
      Convergente,
      Divergente,
      `Divergente p/ CR já existente`,
      `Divergente p/ CR Nova`
    )
  
  dir.create(DIR_TABELAS, showWarnings = FALSE)
  caminho_arquivo <- file.path(DIR_TABELAS, "tabela_concordancia_sii.xlsx")
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Concordância")
  
  # Estilo centralizado
  estilo_centralizado <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE,
    border = "TopBottomLeftRight"
  )
  
  estilo_dados <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    wrapText = TRUE
  )
  
  # Cabeçalho nível 1
  cab1 <- c(
    "Coordenação Regional",
    "Criada na Reestruturação?",
    "Situação da Terra Indígena Contida",
    NA,
    NA,
    NA
  )
  
  # Cabeçalho nível 2
  cab2 <- c(
    "",
    "",
    "Convergente",
    "Divergente",
    "Divergente p/ CR já existente",
    "Divergente p/ CR Nova"
  )
  
  openxlsx::writeData(
    wb,
    "Concordância",
    x = matrix(cab1, nrow = 1),
    startRow = 1,
    colNames = FALSE
  )
  
  openxlsx::writeData(
    wb,
    "Concordância",
    x = matrix(cab2, nrow = 1),
    startRow = 2,
    colNames = FALSE
  )
  
  # Mesclas
  openxlsx::mergeCells(wb, "Concordância", cols = 1, rows = 1:2)
  openxlsx::mergeCells(wb, "Concordância", cols = 2, rows = 1:2)
  openxlsx::mergeCells(wb, "Concordância", cols = 3:6, rows = 1)
  
  # Dados
  openxlsx::writeData(
    wb,
    "Concordância",
    tabela_final,
    startRow = 3,
    colNames = FALSE
  )
  
  # Aplicar estilos
  n_linhas <- nrow(tabela_final) + 2
  n_colunas <- ncol(tabela_final)
  
  # Cabeçalho
  openxlsx::addStyle(
    wb,
    "Concordância",
    estilo_centralizado,
    rows = 1:2,
    cols = 1:n_colunas,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # Dados
  openxlsx::addStyle(
    wb,
    "Concordância",
    estilo_dados,
    rows = 3:n_linhas,
    cols = 1:n_colunas,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # Autoajuste das colunas
  openxlsx::setColWidths(
    wb,
    "Concordância",
    cols = 1:n_colunas,
    widths = "auto"
  )
  
  # Altura dos cabeçalhos
  openxlsx::setRowHeights(
    wb,
    "Concordância",
    rows = 1:2,
    heights = 35
  )
  
  # Congela cabeçalho
  openxlsx::freezePane(
    wb,
    "Concordância",
    firstActiveRow = 3
  )
  
  openxlsx::saveWorkbook(
    wb,
    caminho_arquivo,
    overwrite = TRUE
  )
  
  return(caminho_arquivo)
}

#' @title Gera tabela de Terras Indígenas por CR com percentual de área
#' @description Realiza interseção espacial entre TIs e CRs para determinar a 
#' proporção da área de cada TI que recai sobre as jurisdições territoriais.
#' @param arq_poligonos_cr Objeto sf contendo os polígonos das CRs (Proposta).
#' @param arq_tis Objeto sf contendo os polígonos das Terras Indígenas.
#' @return Caminho do arquivo xlsx gerado.
criar_tabela_ti_por_cr <- function(arq_poligonos_cr, arq_tis) {
  
  # DEBUG START: Uncomment and run lines below for interactive debugging
  # arq_poligonos_cr <- targets::tar_read(tabela_dados_final_sc)
  # arq_tis          <- targets::tar_read(arq_tis)
  # =========================================================================
  
  # Preparações Iniciais e Padronização de CRS ----
  crs_calc <- CRS_ALBERS_BR
  
  cr_polys <- arq_poligonos_cr |> 
    sf::st_transform(crs_calc) |> 
    dplyr::select(sigla, nome_uorg)
  
  tis <- arq_tis |> 
    sf::st_transform(crs_calc) |> 
    dplyr::mutate(
      area_total_ti_ha = as.numeric(sf::st_area(geometry)) / 10000
    )
  
  # Interseção Espacial e Cálculo de Áreas ----
  # TIs que se sobrepõem a mais de uma CR gerarão múltiplas feições
  ints <- sf::st_intersection(tis, cr_polys)
  
  tabela_dados <- ints |> 
    dplyr::mutate(
      area_int_ha = as.numeric(sf::st_area(geometry)) / 10000,
      pct_contido = area_int_ha / area_total_ti_ha
    ) |> 
    sf::st_drop_geometry() |> 
    # Remoção de slivers (resquícios topológicos menores que 0.01%)
    dplyr::filter(pct_contido >= 0.05) |> 
    dplyr::arrange(terrai_codigo, desc(pct_contido)) |> 
    dplyr::select(
      `Coordenação Regional` = sigla,
      `Nome da CR` = nome_uorg,
      `Código TI` = terrai_codigo,
      `Nome TI` = terrai_nome,
      `CR de Origem (SII)` = undadm_nome,
      `Sigla CR de Origem (SII)` = undadm_sigla,
      `Fase` = fase_ti,
      `Modalidade` = modalidade_ti,
      `Área Total TI (ha)` = area_total_ti_ha,
      `Área na CR (ha)` = area_int_ha,
      `% Contida na CR` = pct_contido
    )
  
  # Exportação para Excel ----
  dir.create(DIR_TABELAS, showWarnings = FALSE)
  caminho_arquivo <- file.path(DIR_TABELAS, "tabela_ti_por_cr.xlsx")
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TI por CR")
  
  # Definição de Estilos
  estilo_cabecalho <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE,
    border = "TopBottomLeftRight",
    fgFill = CORES_FUNAI[["principal"]],
    fontColour = "white"
  )
  
  estilo_texto <- openxlsx::createStyle(
    valign = "center",
    wrapText = TRUE
  )
  
  estilo_numero <- openxlsx::createStyle(
    valign = "center",
    halign = "right",
    numFmt = "#,##0.00"
  )
  
  estilo_percentual <- openxlsx::createStyle(
    valign = "center",
    halign = "right",
    numFmt = "0.00%"
  )
  
  # Escrita de Dados e Aplicação de Estilos
  openxlsx::writeData(
    wb,
    "TI por CR",
    tabela_dados,
    headerStyle = estilo_cabecalho
  )
  
  n_linhas <- nrow(tabela_dados) + 1
  n_colunas <- ncol(tabela_dados)
  
  openxlsx::addStyle(wb, "TI por CR", estilo_texto, rows = 2:n_linhas, cols = 1:8, gridExpand = TRUE)
  openxlsx::addStyle(wb, "TI por CR", estilo_numero, rows = 2:n_linhas, cols = 9:10, gridExpand = TRUE)
  openxlsx::addStyle(wb, "TI por CR", estilo_percentual, rows = 2:n_linhas, cols = 11, gridExpand = TRUE)
  
  openxlsx::setColWidths(wb, "TI por CR", cols = 1:n_colunas, widths = "auto")
  openxlsx::setColWidths(wb, "TI por CR", cols = 4, widths = 35) # Fixa largura maior para Nome TI
  openxlsx::freezePane(wb, "TI por CR", firstActiveRow = 2)

  openxlsx::saveWorkbook(wb, caminho_arquivo, overwrite = TRUE)

  return(caminho_arquivo)
}

#' @title Gera anexo em xlsx da proposta atualizada (Terra Indígena, Município, Distrito, UF)
#' @description Exporta a jurisdição por CR da proposta (malha básica: uma CR por Terra
#' Indígena e por Município, ver `calcular_malha_basica_bruta()`) em 4 abas de um único
#' workbook, para consulta fora do pipeline por quem não usa R/QGIS -- mesmo material de apoio
#' já anunciado no texto da seção "Encaminhamentos" ("planilha eletrônica com a relação dos
#' municípios, distritos e Terras Indígenas sob sua responsabilidade na proposta").
#' @details Terra Indígena e Município são o grão nativo da malha básica (uma linha = uma CR).
#' Distrito é derivado, herdando a CR do município que o contém (um distrito pertence sempre a
#' um único município, então a herança é direta, sem ambiguidade). UF só aparece na aba
#' correspondente quando TODOS os municípios da UF pertencem à mesma CR -- caso contrário
#' "a CR da UF" não teria resposta única e a linha seria enganosa; mesmo critério de
#' `ufs_com_cr_unica` já usado em `dissolver_malha_basica_por_cr()` (R/geoprocessamento.R).
#' @param arq_malha_basica_bruta Caminho do gpkg de malha básica por TI/Município (alvo
#'   `malha_basica_bruta`).
#' @param arq_municipios Objeto sf dos municípios brasileiros (alvo `arq_municipios`), colunas
#'   `cd_mun`, `nm_mun`, `cd_uf`, `nm_uf` -- referência de UF/nome (vintage 2025, cobre
#'   municípios novos que a malha distrital do Censo 2022 ainda não conhece).
#' @param arq_distritos Objeto sf da malha distrital do IBGE 2022 (alvo `arq_distritos`),
#'   colunas `cd_mun`, `cd_dist`, `nm_dist`.
#' @param arq_jurisdicao_atual_bruta Caminho do gpkg gerado por `calcular_jurisdicao_atual_bruta()`
#'   (alvo `jurisdicao_atual_bruta`), com a vinculação já institucionalizada (SII para TI;
#'   declaração SEI/site institucional para Município) -- usado para preencher a coluna de
#'   convergência/divergência frente à proposta.
#' @param dir_saida Diretório de saída (padrão: `DIR_TABELAS`).
#' @return Caminho do arquivo xlsx gerado.
gerar_anexos_proposta_xlsx <- function(
    arq_malha_basica_bruta,
    arq_municipios,
    arq_distritos,
    arq_jurisdicao_atual_bruta,
    dir_saida = DIR_TABELAS
) {

  malha <- sf::st_read(arq_malha_basica_bruta, quiet = TRUE) |> sf::st_drop_geometry()

  # Vinculação já institucionalizada (situação atual, não a proposta) -- mesma fonte usada em
  # criar_mapa_jurisdicao_atual(). Usada abaixo só para a coluna de convergência/divergência, em
  # cada aba, frente à sigla da CR da proposta (`malha`).
  jurisdicao_atual <- sf::st_read(arq_jurisdicao_atual_bruta, quiet = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::transmute(cd_territorio, tipo_territorio, sigla_atual = sigla)

  # Resume a comparação proposta x situação institucional vigente numa única coluna de texto,
  # já com a sigla de origem embutida quando divergente (senão "divergente" sozinho não é
  # acionável). Reaproveitada nas quatro abas.
  rotular_convergencia <- function(sigla_proposta, sigla_atual, rotulo_origem) {
    dplyr::case_when(
      is.na(sigla_atual) | sigla_atual == "Sem vinculação declarada" ~
        paste0("Sem vinculação institucional (", rotulo_origem, ")"),
      sigla_atual == sigla_proposta ~ "Convergente",
      TRUE ~ paste0("Divergente (", rotulo_origem, ": ", sigla_atual, ")")
    )
  }

  tabela_ti <- malha |>
    dplyr::filter(tipo_territorio == "Terra Indígena") |>
    dplyr::left_join(
      jurisdicao_atual |> dplyr::filter(tipo_territorio == "Terra Indígena") |>
        dplyr::select(cd_territorio, sigla_atual),
      by = "cd_territorio"
    ) |>
    dplyr::transmute(
      `Coordenação Regional` = sigla,
      `Nome da CR` = nome_uorg,
      `Código TI` = cd_territorio,
      `Nome da TI` = nm_territorio,
      `Situação Institucional` = rotular_convergencia(sigla, sigla_atual, "SII")
    ) |>
    dplyr::arrange(`Coordenação Regional`, `Nome da TI`)

  municipios <- malha |>
    dplyr::filter(tipo_territorio == "Município") |>
    dplyr::left_join(
      jurisdicao_atual |> dplyr::filter(tipo_territorio == "Município") |>
        dplyr::select(cd_territorio, sigla_atual),
      by = c("cd_territorio")
    ) |>
    dplyr::transmute(
      cd_mun = as.character(cd_territorio), cd_uorg, nome_uorg, sigla,
      situacao_institucional = rotular_convergencia(sigla, sigla_atual, "Declarado")
    )

  # arq_municipios (não arq_distritos) é a referência de UF/nome: mesma vintage (2025) da malha
  # básica, cobre municípios novos que a malha distrital do Censo 2022 ainda não conhece (ex.:
  # Boa Esperança do Norte/MT, ver overrides manuais em calcular_malha_basica_bruta()).
  ref_mun <- arq_municipios |>
    sf::st_drop_geometry() |>
    dplyr::mutate(cd_mun = as.character(cd_mun)) |>
    dplyr::distinct(cd_uf, nm_uf, cd_mun, nm_mun)

  municipios_uf <- municipios |> dplyr::left_join(ref_mun, by = "cd_mun")

  tabela_municipio <- municipios_uf |>
    dplyr::transmute(
      `Coordenação Regional` = sigla,
      `Nome da CR` = nome_uorg,
      UF = nm_uf,
      `Código Município` = cd_mun,
      `Nome do Município` = nm_mun,
      `Situação Institucional` = situacao_institucional
    ) |>
    dplyr::arrange(`Coordenação Regional`, UF, `Nome do Município`)

  # Distrito: sempre pertence a um único município (herança direta) -- inner_join descarta só o
  # município acima (sem distrito na malha do Censo 2022 ainda).
  ref_dist <- arq_distritos |>
    sf::st_drop_geometry() |>
    dplyr::mutate(cd_mun = as.character(cd_mun)) |>
    dplyr::distinct(cd_mun, cd_dist, nm_dist)

  tabela_distrito <- municipios_uf |>
    dplyr::inner_join(ref_dist, by = "cd_mun") |>
    dplyr::transmute(
      `Coordenação Regional` = sigla,
      `Nome da CR` = nome_uorg,
      UF = nm_uf,
      `Nome do Município` = nm_mun,
      `Código Distrito` = cd_dist,
      `Nome do Distrito` = nm_dist,
      `Situação Institucional` = situacao_institucional
    ) |>
    dplyr::arrange(`Coordenação Regional`, UF, `Nome do Município`, `Nome do Distrito`)

  ufs_com_cr_unica <- municipios_uf |>
    dplyr::distinct(cd_uf, cd_uorg) |>
    dplyr::count(cd_uf, name = "n_crs_distintas") |>
    dplyr::filter(n_crs_distintas == 1) |>
    dplyr::pull(cd_uf)

  # Situação Institucional da UF: agrega a situação por município (já calculada acima) -- só faz
  # sentido "Convergente" quando TODOS os municípios da UF são também institucionalmente
  # vinculados à mesma CR da proposta (mesmo critério de unicidade de `ufs_com_cr_unica`).
  situacao_uf <- municipios_uf |>
    dplyr::filter(cd_uf %in% ufs_com_cr_unica) |>
    dplyr::group_by(cd_uf) |>
    dplyr::summarise(
      situacao_institucional = dplyr::case_when(
        all(situacao_institucional == "Convergente") ~ "Convergente",
        all(startsWith(situacao_institucional, "Sem vinculação institucional")) ~ "Sem vinculação institucional (Declarado)",
        TRUE ~ "Divergente (parcial ou integralmente, ver aba Município)"
      ),
      .groups = "drop"
    )

  tabela_uf <- municipios_uf |>
    dplyr::filter(cd_uf %in% ufs_com_cr_unica) |>
    dplyr::distinct(cd_uf, nm_uf, cd_uorg, nome_uorg, sigla) |>
    dplyr::left_join(situacao_uf, by = "cd_uf") |>
    dplyr::transmute(
      `Coordenação Regional` = sigla, `Nome da CR` = nome_uorg, UF = nm_uf,
      `Situação Institucional` = situacao_institucional
    ) |>
    dplyr::arrange(`Coordenação Regional`)

  # Exportação para Excel ----
  dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)
  caminho_arquivo <- file.path(dir_saida, "anexos_proposta_atualizada.xlsx")

  wb <- openxlsx::createWorkbook()

  estilo_cabecalho <- openxlsx::createStyle(
    halign = "center", valign = "center", textDecoration = "bold", wrapText = TRUE,
    border = "TopBottomLeftRight", fgFill = CORES_FUNAI[["principal"]], fontColour = "white"
  )
  estilo_texto <- openxlsx::createStyle(valign = "center", wrapText = TRUE)

  escrever_aba <- function(nome_aba, dados, largura_extra = NULL) {
    openxlsx::addWorksheet(wb, nome_aba)
    openxlsx::writeData(wb, nome_aba, dados, headerStyle = estilo_cabecalho)
    n_linhas <- nrow(dados) + 1
    openxlsx::addStyle(wb, nome_aba, estilo_texto, rows = 2:n_linhas, cols = 1:ncol(dados), gridExpand = TRUE)
    openxlsx::setColWidths(wb, nome_aba, cols = 1:ncol(dados), widths = "auto")
    if (!is.null(largura_extra)) {
      openxlsx::setColWidths(wb, nome_aba, cols = largura_extra, widths = 35)
    }
    openxlsx::freezePane(wb, nome_aba, firstActiveRow = 2)
  }

  escrever_aba("Terra Indígena", tabela_ti, largura_extra = 4)
  escrever_aba("Município", tabela_municipio, largura_extra = 5)
  escrever_aba("Distrito", tabela_distrito, largura_extra = 6)
  escrever_aba("UF", tabela_uf)

  openxlsx::saveWorkbook(wb, caminho_arquivo, overwrite = TRUE)

  caminho_arquivo
}

#' @title Gera anexo em xlsx dos municípios com presença indígena sem nenhuma CR declarada
#' @description Lista os municípios com concentração relevante de população indígena no Censo 2022
#' (SIDRA) para os quais nenhuma Coordenação Regional declarou atendimento -- nem via processo
#' SEI, nem via site institucional da Funai (mesma fonte de `obter_municipios_declarados_cr()`,
#' alvo `municipios_declarados_cr`). Mesmo recorte de "carência de atendimento declarado" e mesmo
#' corte de concentração mínima (`limiar_concentracao`, padrão 100 indígenas) já usados em
#' `criar_mapa_pi_carencia_atendimento()` -- sem esse corte, a listagem incluiria a maioria dos
#' municípios do país (praticamente todos têm ao menos 1 pessoa autodeclarada indígena no Censo,
#' incluindo capitais como Salvador e Brasília), descaracterizando o anexo como uma lista
#' acionável de lacunas de atendimento.
#' @param arq_municipios Objeto sf dos municípios brasileiros (alvo `arq_municipios`), colunas
#'   `cd_mun`, `nm_mun`, `cd_uf`, `nm_uf`.
#' @param tb_pop_mun Tabela bruta do SIDRA de população indígena por município (alvo `tb_pop_pi`).
#' @param df_declarados Município-CR declarados, já desempatado por maior número SEI (alvo
#'   `municipios_declarados_cr`).
#' @param limiar_concentracao População indígena mínima para o município entrar na lista (padrão:
#'   100, mesmo valor de `criar_mapa_pi_carencia_atendimento()`).
#' @param dir_saida Diretório de saída (padrão: `DIR_TABELAS`).
#' @return Caminho do arquivo xlsx gerado.
gerar_anexo_municipios_sem_atendimento_xlsx <- function(
    arq_municipios,
    tb_pop_mun,
    df_declarados,
    limiar_concentracao = 100,
    dir_saida = DIR_TABELAS
) {

  pop_mun <- obter_pop_indigena_municipio(tb_pop_mun)

  ref_mun <- arq_municipios |>
    sf::st_drop_geometry() |>
    dplyr::mutate(cd_mun = as.character(cd_mun)) |>
    dplyr::distinct(cd_uf, nm_uf, cd_mun, nm_mun)

  cd_mun_declarados <- unique(as.character(df_declarados$cd_mun))

  tabela <- ref_mun |>
    dplyr::left_join(pop_mun, by = "cd_mun") |>
    dplyr::filter(!is.na(pop_pi), pop_pi >= limiar_concentracao, !cd_mun %in% cd_mun_declarados) |>
    dplyr::transmute(
      UF = nm_uf,
      `Código Município` = cd_mun,
      `Nome do Município` = nm_mun,
      `População Indígena (Censo 2022)` = pop_pi
    ) |>
    dplyr::arrange(dplyr::desc(`População Indígena (Censo 2022)`))

  dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)
  caminho_arquivo <- file.path(dir_saida, "anexo_municipios_sem_atendimento.xlsx")

  wb <- openxlsx::createWorkbook()

  estilo_cabecalho <- openxlsx::createStyle(
    halign = "center", valign = "center", textDecoration = "bold", wrapText = TRUE,
    border = "TopBottomLeftRight", fgFill = CORES_FUNAI[["principal"]], fontColour = "white"
  )
  estilo_texto <- openxlsx::createStyle(valign = "center", wrapText = TRUE)
  estilo_numero <- openxlsx::createStyle(numFmt = "#,##0")

  nome_aba <- "Municípios Sem Atendimento"
  openxlsx::addWorksheet(wb, nome_aba)
  openxlsx::writeData(wb, nome_aba, tabela, headerStyle = estilo_cabecalho)
  n_linhas <- nrow(tabela) + 1
  openxlsx::addStyle(wb, nome_aba, estilo_texto, rows = 2:n_linhas, cols = 1:3, gridExpand = TRUE)
  openxlsx::addStyle(wb, nome_aba, estilo_numero, rows = 2:n_linhas, cols = 4, gridExpand = TRUE)
  openxlsx::setColWidths(wb, nome_aba, cols = 1:4, widths = "auto")
  openxlsx::freezePane(wb, nome_aba, firstActiveRow = 2)

  openxlsx::saveWorkbook(wb, caminho_arquivo, overwrite = TRUE)

  caminho_arquivo
}

#' @title Mapa de TIs com divergência jurisdicional (SII vs. Proposta)
#' @description Identifica as Terras Indígenas cuja CR registrada no SII difere da CR
#' jurisdicionante na proposta de reestruturação territorial. TIs convergentes aparecem em
#' cinza claro como referência; TIs divergentes são destacadas (mesmo esquema de cor
#' Convergente/Divergente de `criar_mapa_divergencia_municipio_declarado()`, para manter as
#' duas seções de "Situação dos Municípios e Terras Indígenas" visualmente consistentes) e
#' rotuladas em formato de "chip" (mesmo estilo de `criar_mapa_coropletico_cr()`): nome da TI,
#' CR na proposta e CR no SII. Substitui a versão anterior, que codificava a CR de origem (SII)
#' por cor numa paleta de até 30 cores -- ilegível com muitas TIs divergentes de CRs
#' diferentes, e menos preciso do que o rótulo (que já traz as duas siglas por extenso).
#'
#' @param arq_poligonos_cr Objeto sf com os polígonos das CRs da proposta.
#'   Deve conter as colunas `cd_uorg` e `sigla`.
#' @param arq_tis Objeto sf com os polígonos das Terras Indígenas.
#'   Deve conter `terrai_codigo`, `terrai_nome` e `undadm_codigo`.
#' @param arq_uorg_ug Data.frame de correspondência entre código SII (`cd_sii`) e
#'   código UORG (`cd_uorg`). Usado para traduzir `undadm_codigo` -> CR do SII.
#' @param dicionario_cr Dicionário de CRs usado para derivar `cd_sii` (ver `adicionar_cd_sii()`).
#' @param arquivo Nome do arquivo de saída (sem extensão). Se `NULL`, não exporta.
#' @param threshold_pct Percentual mínimo de sobreposição para considerar uma TI
#'   como contida em uma CR da proposta (padrão: 99%).
#' @return Objeto ggplot com o mapa gerado.
#'
#' @export
#' @title Situação de convergência/divergência de cada TI entre o SII e a proposta
#' @description Correspondência espacial entre a vinculação vigente no SII e a CR majoritária de
#' cada Terra Indígena na proposta de reestruturação territorial. Extraída de
#' `criar_mapa_divergencia_ti_sii()` para ser reaproveitada também por
#' `montar_tabela_divergencia_ti_sii()` (quantificação numérica), sem duplicar a lógica de
#' correspondência entre as duas.
#' @param arq_poligonos_cr Objeto sf com os polígonos das CRs da proposta.
#'   Deve conter as colunas `cd_uorg` e `sigla`.
#' @param arq_tis Objeto sf com os polígonos das Terras Indígenas.
#'   Deve conter `terrai_codigo`, `terrai_nome` e `undadm_codigo`.
#' @param arq_uorg_ug Data.frame de correspondência entre código SII (`cd_sii`) e
#'   código UORG (`cd_uorg`). Usado para traduzir `undadm_codigo` -> CR do SII.
#' @param dicionario_cr Dicionário de CRs usado para derivar `cd_sii` (ver `adicionar_cd_sii()`).
#' @return Tibble não espacial, uma linha por TI: `terrai_codigo`, `terrai_nome`, `area_ti`,
#'   `uorg_sii`, `cd_uorg_proposta`, `sigla_cr_sii`, `sigla_cr_proposta`, `divergente`.
calcular_divergencia_ti_sii <- function(arq_poligonos_cr, arq_tis, arq_uorg_ug, dicionario_cr) {

  # crs_calc = SIRGAS 2000 / Albers Equal Area (IBGE, adequado para áreas nacionais)
  crs_calc <- 5880
  arq_uorg_ug <- adicionar_cd_sii(arq_uorg_ug, dicionario_cr)

  # 1. PREPARAÇÃO ESPACIAL
  cr_polys <- arq_poligonos_cr |>
    sf::st_make_valid() |>
    sf::st_transform(crs_calc) |>
    dplyr::select(cd_uorg, sigla, nome_uorg)

  tis <- arq_tis |>
    sf::st_make_valid() |>
    sf::st_transform(crs_calc)

  tis <- tis |>
    dplyr::mutate(area_ti = as.numeric(sf::st_area(tis)), undadm_codigo = as.character(undadm_codigo)) |>
    dplyr::left_join(
      arq_uorg_ug |>
        dplyr::select(cd_sii, uorg_sii = cd_uorg) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)),
      by = dplyr::join_by(undadm_codigo == cd_sii)
    )

  # 2. INTERSEÇÃO TI × CR DA PROPOSTA
  # Calcula a CR que detém a maior área de cada TI na proposta
  ints <- sf::st_intersection(tis, cr_polys |> dplyr::select(cd_uorg_proposta = cd_uorg, sigla_cr_proposta = sigla))

  ints <- ints |>
    dplyr::mutate(
      area_int  = as.numeric(sf::st_area(ints)),
      pct_int   = area_int / area_ti
    ) |>
    sf::st_drop_geometry()

  # Elege a CR majoritária da proposta para cada TI (descarta resíduos topológicos < 1%)
  ti_proposta <- ints |>
    dplyr::filter(pct_int >= 0.01) |>
    dplyr::group_by(terrai_codigo, terrai_nome, uorg_sii) |>
    dplyr::slice_max(area_int, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      uorg_sii          = as.character(uorg_sii),
      cd_uorg_proposta  = as.character(cd_uorg_proposta),
      divergente        = !is.na(uorg_sii) & (uorg_sii != cd_uorg_proposta)
    )

  # 3. RESGATE DA SIGLA DA CR DE ORIGEM (SII)
  lookup_sigla <- arq_poligonos_cr |>
    sf::st_drop_geometry() |>
    dplyr::select(cd_uorg, sigla) |>
    dplyr::mutate(cd_uorg = as.character(cd_uorg)) |>
    dplyr::distinct()

  ti_proposta |>
    dplyr::left_join(
      lookup_sigla |> dplyr::rename(sigla_cr_sii = sigla),
      by = dplyr::join_by(uorg_sii == cd_uorg)
    )
}

#' @title Tabela-síntese de divergência entre a proposta e a vinculação SII, por TI
#' @description Quantifica quantas Terras Indígenas -- e qual população -- mudam de CR entre a
#' vinculação institucional vigente (SII) e a proposta de reestruturação territorial, agregando
#' por par (CR de origem no SII, CR de destino na proposta), com a população oficial do Censo
#' 2022 por TI (agregado 9719 do SIDRA). Mesma correspondência espacial usada por
#' `criar_mapa_divergencia_ti_sii()` (via `calcular_divergencia_ti_sii()`), para as duas nunca
#' divergirem entre si.
#' @param divergencia_ti Tibble retornado por `calcular_divergencia_ti_sii()`.
#' @param tb_pop_ti_sidra Tabela bruta do SIDRA de população por TI (alvo `tb_pop_ti`).
#' @param df_tis_ibge De-para de códigos TI FUNAI vs IBGE (alvo `arq_tis_ibge`).
#' @return Tibble com uma linha por par (CR no SII -> CR na proposta) entre TIs divergentes, mais
#'   uma linha de total, com número de TIs e população oficial afetada. TIs sem código oficial do
#'   IBGE entram na contagem de TIs, mas não têm população oficial (tratada como 0 no total, já
#'   que o Censo/SIDRA não as reconhece -- mesma limitação documentada em
#'   `calcular_populacao_indigena_cr_sidra()`). Os percentuais em relação ao total nacional de TIs
#'   e de população oficial ficam nos atributos `pct_tis_divergentes`/`pct_pop_divergente`.
montar_tabela_divergencia_ti_sii <- function(divergencia_ti, tb_pop_ti_sidra, df_tis_ibge) {

  df_tis_ibge_clean <- df_tis_ibge |>
    sf::st_drop_geometry() |>
    dplyr::select(cd_ti_ibge = cd_ti, cd_funai) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))

  pop_ti_oficial <- tb_pop_ti_sidra |>
    dplyr::filter(D1C == 1432) |>
    dplyr::transmute(
      cd_ti_ibge = as.integer(D2C),
      pop_ti_oficial = limpar_sigilo_ibge(V)
    )

  divergencia_pop <- divergencia_ti |>
    dplyr::mutate(terrai_codigo = as.integer(terrai_codigo)) |>
    dplyr::left_join(df_tis_ibge_clean, by = dplyr::join_by(terrai_codigo == cd_funai)) |>
    dplyr::left_join(pop_ti_oficial, by = "cd_ti_ibge") |>
    dplyr::mutate(pop_ti_oficial = dplyr::coalesce(pop_ti_oficial, 0))

  total_tis <- nrow(divergencia_pop)
  total_pop <- sum(divergencia_pop$pop_ti_oficial, na.rm = TRUE)

  tabela <- divergencia_pop |>
    dplyr::filter(divergente) |>
    dplyr::group_by(`CR no SII` = sigla_cr_sii, `CR na Proposta` = sigla_cr_proposta) |>
    dplyr::summarise(
      `Nº de TIs` = dplyr::n(),
      `População Indígena (Censo 2022)` = sum(pop_ti_oficial, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `% do Total de TIs` = 100 * `Nº de TIs` / total_tis,
      `% da População Oficial em TI` = 100 * `População Indígena (Censo 2022)` / total_pop
    ) |>
    dplyr::arrange(dplyr::desc(`Nº de TIs`))

  linha_total <- tibble::tibble(
    `CR no SII` = "Total",
    `CR na Proposta` = "-",
    `Nº de TIs` = sum(tabela$`Nº de TIs`),
    `População Indígena (Censo 2022)` = sum(tabela$`População Indígena (Censo 2022)`),
    `% do Total de TIs` = 100 * sum(tabela$`Nº de TIs`) / total_tis,
    `% da População Oficial em TI` = 100 * sum(tabela$`População Indígena (Censo 2022)`) / total_pop
  )

  tabela_final <- dplyr::bind_rows(tabela, linha_total)

  attr(tabela_final, "total_tis") <- total_tis
  attr(tabela_final, "total_pop") <- total_pop
  attr(tabela_final, "pct_tis_divergentes") <- round(100 * linha_total$`Nº de TIs` / total_tis, 1)
  attr(tabela_final, "pct_pop_divergente") <- round(100 * linha_total$`População Indígena (Censo 2022)` / total_pop, 1)

  tabela_final
}

#' @title Mapa de TIs com divergência jurisdicional (SII vs. Proposta)
#' @description Identifica as Terras Indígenas cuja CR registrada no SII difere da CR
#' jurisdicionante na proposta de reestruturação territorial. TIs convergentes aparecem em
#' cinza claro como referência; TIs divergentes são destacadas (mesmo esquema de cor
#' Convergente/Divergente de `criar_mapa_divergencia_municipio_declarado()`, para manter as
#' duas seções de "Situação dos Municípios e Terras Indígenas" visualmente consistentes) e
#' rotuladas em formato de "chip" (mesmo estilo de `criar_mapa_coropletico_cr()`): nome da TI,
#' CR na proposta e CR no SII.
#' @param arq_poligonos_cr Objeto sf com os polígonos das CRs da proposta.
#'   Deve conter as colunas `cd_uorg` e `sigla`.
#' @param arq_tis Objeto sf com os polígonos das Terras Indígenas.
#'   Deve conter `terrai_codigo`, `terrai_nome` e `undadm_codigo`.
#' @param arq_uorg_ug Data.frame de correspondência entre código SII (`cd_sii`) e
#'   código UORG (`cd_uorg`). Usado para traduzir `undadm_codigo` -> CR do SII.
#' @param dicionario_cr Dicionário de CRs usado para derivar `cd_sii` (ver `adicionar_cd_sii()`).
#' @param arquivo Nome do arquivo de saída (sem extensão). Se `NULL`, não exporta.
#' @return Objeto ggplot com o mapa gerado.
#'
#' @export
criar_mapa_divergencia_ti_sii <- function(
    arq_poligonos_cr,
    arq_tis,
    arq_uorg_ug,
    dicionario_cr,
    arquivo = "mapa_divergencia_ti_sii"
) {

  crs_calc <- 5880

  ti_proposta <- calcular_divergencia_ti_sii(arq_poligonos_cr, arq_tis, arq_uorg_ug, dicionario_cr)

  cr_polys <- arq_poligonos_cr |>
    sf::st_make_valid() |>
    sf::st_transform(crs_calc) |>
    dplyr::select(cd_uorg, sigla, nome_uorg)

  tis <- arq_tis |>
    sf::st_make_valid() |>
    sf::st_transform(crs_calc)

  # 4. ENRIQUECIMENTO GEOMÉTRICO
  # Divide as TIs em dois grupos (convergentes e divergentes) mantendo geometria
  tis_base <- tis |>
    sf::st_drop_geometry() |>
    dplyr::select(terrai_codigo) |>
    dplyr::left_join(
      ti_proposta |> dplyr::select(terrai_codigo, divergente, sigla_cr_proposta, sigla_cr_sii),
      by = "terrai_codigo"
    ) |>
    dplyr::mutate(
      divergente        = dplyr::coalesce(divergente, FALSE),
      sigla_cr_proposta = dplyr::if_else(divergente, sigla_cr_proposta, NA_character_),
      sigla_cr_sii      = dplyr::if_else(divergente, sigla_cr_sii, NA_character_)
    )

  tis_sf <- tis |>
    dplyr::left_join(tis_base, by = "terrai_codigo")

  tis_convergentes <- tis_sf |> dplyr::filter(!divergente)
  tis_divergentes  <- tis_sf |> dplyr::filter(divergente)

  n_div <- nrow(tis_divergentes)
  cli::cli_alert_info("{n_div} TI{?s} com diverg{?ência/ências} identificada{?s}.")

  # 5. STATUS (mesmo esquema de cor Convergente/Divergente de
  # criar_mapa_divergencia_municipio_declarado(), para as duas seções ficarem consistentes).
  cores_status <- c("Convergente" = "#2A9D8F", "Divergente" = "#E63946")

  # Rótulo em "chip" (nome da TI + CR do SII -> CR da proposta -- origem -> destino, situação
  # hoje -> situação na proposta, não o contrário), mesmo estilo de criar_mapa_coropletico_cr():
  # posição resolvida por ggrepel sobre o ponto interno da TI.
  centroides_divergentes <- tis_divergentes |>
    sf::st_transform(4326) |>
    sf::st_point_on_surface() |>
    dplyr::mutate(rotulo = paste0(terrai_nome, "\n", sigla_cr_sii, " → ", sigla_cr_proposta))

  # aes(geometry = geometry) abaixo espera a coluna com esse nome -- arq_tis usa "geom" (ver
  # armadilha de nome de coluna de geometria em CLAUDE.md), então renomeia explicitamente (mesmo
  # padrão de calcular_malha_basica_bruta() em R/geoprocessamento.R).
  sf_col_div <- attr(centroides_divergentes, "sf_column")
  names(centroides_divergentes)[names(centroides_divergentes) == sf_col_div] <- "geometry"
  sf::st_geometry(centroides_divergentes) <- "geometry"

  # 6. CONSTRUÇÃO DO MAPA
  g <- ggplot2::ggplot() +

    # 6.1 Camada de fundo: contornos das CRs da proposta
    ggplot2::geom_sf(
      data = cr_polys |> sf::st_transform(4326),
      fill  = NA,
      color = "#2B2B2B",
      linewidth = 0.35,
      linetype  = "solid"
    ) +

    # 6.2 TIs convergentes em cinza claro (referência)
    ggplot2::geom_sf(
      data = tis_convergentes |> sf::st_transform(4326),
      fill      = "#D6D6D6",
      color     = "#AAAAAA",
      linewidth = 0.05,
      alpha     = 0.6
    ) +

    # 6.3 TIs divergentes em destaque (status Convergente/Divergente)
    ggplot2::geom_sf(
      data = tis_sf |> sf::st_transform(4326) |> dplyr::mutate(status = ifelse(divergente, "Divergente", "Convergente")) |> dplyr::filter(divergente),
      ggplot2::aes(fill = status),
      color     = "#1A1A1A",
      linewidth = 0.2,
      alpha     = 0.88
    ) +

    # 6.4 Reforça o contorno das CRs sobre as TIs divergentes
    ggplot2::geom_sf(
      data = cr_polys |> sf::st_transform(4326),
      fill      = NA,
      color     = "#2B2B2B",
      linewidth = 0.5
    ) +

    # 6.5 Rótulos das siglas das CRs da proposta
    ggplot2::geom_sf_text(
      data = cr_polys |>
        sf::st_transform(4326) |>
        sf::st_point_on_surface(),
      ggplot2::aes(label = sigla),
      size      = 1.8,
      color     = "#333333",
      fontface  = "bold",
      check_overlap = TRUE
    ) +

    # 6.6 Rótulos-chip das TIs divergentes: nome da TI + CR do SII -> CR da proposta. Com 100+
    # TIs divergentes concentradas em algumas regiões (ex.: Amazonas), a letra menor e um
    # orçamento de iteração/força maior no ggrepel são necessários para o algoritmo conseguir
    # espalhar os rótulos o suficiente e ainda assim caber todos sem sobrepor uns aos outros.
    ggrepel::geom_label_repel(
      data = centroides_divergentes,
      ggplot2::aes(geometry = geometry, label = rotulo),
      stat = "sf_coordinates",
      size = 1.4,
      color = cores_status[["Divergente"]],
      fontface = "bold",
      lineheight = 0.9,
      label.size = 0,
      label.r = grid::unit(0.25, "lines"),
      label.padding = grid::unit(0.15, "lines"),
      fill = scales::alpha("white", 0.9),
      seed = 42,
      max.overlaps = Inf,
      min.segment.length = 0,
      segment.size = 0.2,
      segment.color = scales::alpha(cores_status[["Divergente"]], 0.5),
      box.padding = 0.2,
      point.padding = 0.05,
      force = 3,
      force_pull = 0.5,
      max.iter = 20000,
      max.time = 3
    ) +

    # 6.7 Escala de cores (status)
    ggplot2::scale_fill_manual(values = cores_status, name = "Situação") +

    # 6.8 Coordenadas e projeção
    ggplot2::coord_sf(crs = sf::st_crs(4326), expand = FALSE) +

    # 6.9 Textos
    ggplot2::labs(
      title    = "Terras Indígenas com divergência de jurisdição entre o SII e a proposta de reestruturação",
      subtitle = paste0(
        "TIs cuja Coordenação Regional atualmente responsável no SII difere da CR definida pela proposta territorial",
        "\n(", n_div, " TI", ifelse(n_div == 1, " divergente", "s divergentes"), " identificada",
        ifelse(n_div == 1, "", "s"), ")"
      ),
      caption  = "Fonte: SII (FUNAI), Geoserver FUNAI e proposta de reestruturação territorial"
    ) +

    # 6.10 Tema
    ggplot2::theme_minimal(base_size = 10) +
    obter_tema(tipo = "mapa", cor = CORES_FUNAI[["principal"]]) +
    ggplot2::theme(
      legend.position      = "right",
      legend.title         = ggplot2::element_text(face = "bold", size = 8),
      legend.text          = ggplot2::element_text(size = 7.5),
      legend.key.size      = grid::unit(0.4, "cm"),
      legend.margin        = ggplot2::margin(t = -4),
      plot.title           = ggplot2::element_text(size = 10, face = "bold", color = CORES_FUNAI[["principal"]]),
      plot.subtitle        = ggplot2::element_text(size = 8,  color = "grey35"),
      plot.caption         = ggplot2::element_text(size = 7,  color = "grey50", hjust = 1),
      plot.margin          = ggplot2::margin(6, 6, 6, 6)
    )

  if (!is.null(arquivo)) {
    exporta_grafico(g, arquivo)
  }

  return(g)
}

#' @title Mapa de convergência entre a proposta e os municípios declarados por CR
#' @description Compara, para cada município, a CR responsável na malha básica da proposta (TI +
#' Município) com a CR que o declarou formalmente atendido no processo SEI n°
#' 08620.010286/2021-89. Classifica cada município em convergente (mesma CR), divergente (CR
#' diferente) ou sem declaração (município não consta na planilha de declarações). Ao contrário de
#' `criar_mapa_divergencia_ti_sii()`, não precisa de interseção espacial: `malha_basica_bruta` já
#' tem uma única CR por município.
#' @details O status é calculado sobre `malha_basica_bruta` (cujas geometrias de município têm a
#' área de TI sobreposta recortada, ver `calcular_malha_basica_bruta()` em
#' `R/geoprocessamento.R`), mas é plotado sobre o contorno municipal COMPLETO (`arq_municipios`):
#' o status é atributo do município inteiro (mesmo `cd_uorg`, calculado antes do recorte),
#' independente de qual geometria é usada para desenhá-lo -- só a geometria muda. Mostrar o
#' contorno recortado deixava "buracos" no mapa nos municípios que contêm TI, uma leitura visual
#' enganosa (parece área sem status, quando na verdade é só o pedaço de TI que a camada de TI já
#' cobre por cima).
#' @param arq_malha_basica_bruta Caminho do gpkg de malha básica por TI/Município (alvo
#'   `malha_basica_bruta`).
#' @param arq_municipios Objeto sf com o contorno municipal completo, sem recorte de TI (alvo
#'   `arq_municipios`), coluna `cd_mun`.
#' @param df_declarados Município-CR declarados (alvo `municipios_declarados_cr`, ver
#'   `obter_municipios_declarados_cr()`).
#' @title Situação de convergência/divergência de cada município entre a declaração (SEI) e a proposta
#' @description Correspondência entre a CR que declarou formalmente atender cada município
#' (processo SEI n° 08620.010286/2021-89) e a CR responsável por ele na malha básica da proposta.
#' Extraída de `criar_mapa_divergencia_municipio_declarado()` para ser reaproveitada também por
#' `montar_tabela_divergencia_municipio_declarado()` (quantificação numérica), sem duplicar a lógica de
#' correspondência entre as duas.
#' @param arq_malha_basica_bruta Caminho do gpkg de malha básica por TI/Município (alvo
#'   `malha_basica_bruta`).
#' @param df_declarados Município-CR declarados (alvo `municipios_declarados_cr`, ver
#'   `obter_municipios_declarados_cr()`), já com `sigla`.
#' @return Tibble não espacial, uma linha por município: `cd_mun`, `cd_uorg`, `sigla_cr_proposta`,
#'   `cd_uorg_declarado`, `sigla_cr_sei`, `status` (Convergente/Divergente/Sem declaração).
calcular_divergencia_municipio_declarado <- function(arq_malha_basica_bruta, df_declarados) {

  status_mun <- sf::st_read(arq_malha_basica_bruta, quiet = TRUE) |>
    dplyr::filter(tipo_territorio == "Município") |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      cd_mun = as.character(cd_territorio),
      cd_uorg = as.character(cd_uorg),
      sigla_cr_proposta = sigla
    )

  declarados <- df_declarados |>
    dplyr::transmute(
      cd_mun = as.character(cd_mun),
      cd_uorg_declarado = as.character(codigoUnidade),
      sigla_cr_sei = sigla
    ) |>
    dplyr::distinct()

  status_mun |>
    dplyr::left_join(declarados, by = "cd_mun") |>
    dplyr::mutate(
      status = dplyr::case_when(
        is.na(cd_uorg_declarado) ~ "Sem declaração",
        cd_uorg == cd_uorg_declarado ~ "Convergente",
        TRUE ~ "Divergente"
      )
    )
}

#' @title Tabela-síntese de divergência entre a proposta e a declaração de atendimento (SEI), por município
#' @description Quantifica quantos municípios -- e qual população indígena -- mudam de CR entre a
#' declaração formal de atendimento (processo SEI n° 08620.010286/2021-89) e a proposta de
#' reestruturação territorial, agregando por par (CR declarante, CR na proposta). Usa a população
#' indígena "fora de TI" do SIDRA (agregado 8175, corte `c2661 = 12870`) -- o mesmo corte de
#' `calcular_populacao_indigena_cr_sidra()` -- para não contar duas vezes a população indígena
#' residente em Terra Indígena: uma vez aqui (dentro do município que a contém) e outra na própria
#' TI (`montar_tabela_divergencia_ti_sii()`). Segue a mesma convenção desta última: municípios sem
#' declaração (sem CR de origem para comparar) entram no denominador do total nacional, mas não na
#' contagem de divergentes -- não há "mudança de CR" sem uma CR de origem. Mesma correspondência
#' usada por `criar_mapa_divergencia_municipio_declarado()` (via
#' `calcular_divergencia_municipio_declarado()`), para as duas nunca divergirem entre si.
#' @param divergencia_mun Tibble retornado por `calcular_divergencia_municipio_declarado()`.
#' @param tb_pop_mun_fora_ti_sidra Tabela bruta do SIDRA de população indígena "fora de TI" por
#'   município (alvo `tb_pop_mun_fora_ti`).
#' @param total_pop_fora_ti_nacional Total nacional oficial de população indígena fora de TI
#'   (Tabelas 2 e 3 da nota técnica), usado como denominador de `% da População Fora de TI`. A soma
#'   municipal de `tb_pop_mun_fora_ti_sidra` fica abaixo desse total porque os municípios com dado
#'   suprimido por sigilo ("X") entram como 0. `NULL` usa a soma municipal.
#' @return Tibble com uma linha por par (CR declarante -> CR na proposta) entre municípios
#'   divergentes, mais uma linha de total. Os percentuais em relação ao total nacional de
#'   municípios e de população ficam nos atributos `pct_mun_divergentes`/`pct_pop_divergente`.
montar_tabela_divergencia_municipio_declarado <- function(
    divergencia_mun,
    tb_pop_mun_fora_ti_sidra,
    total_pop_fora_ti_nacional = NULL
) {

  pop_mun_fora_ti <- tb_pop_mun_fora_ti_sidra |>
    dplyr::slice(-1) |>
    dplyr::transmute(
      cd_mun = as.character(D1C),
      pop_mun_fora_ti = limpar_sigilo_ibge(V)
    )

  divergencia_pop <- divergencia_mun |>
    dplyr::left_join(pop_mun_fora_ti, by = "cd_mun") |>
    dplyr::mutate(pop_mun_fora_ti = dplyr::coalesce(pop_mun_fora_ti, 0))

  total_mun <- nrow(divergencia_pop)
  total_pop <- if (is.null(total_pop_fora_ti_nacional)) {
    sum(divergencia_pop$pop_mun_fora_ti, na.rm = TRUE)
  } else {
    total_pop_fora_ti_nacional
  }

  tabela <- divergencia_pop |>
    dplyr::filter(status == "Divergente") |>
    dplyr::group_by(`CR Declarante (SEI)` = sigla_cr_sei, `CR na Proposta` = sigla_cr_proposta) |>
    dplyr::summarise(
      `Nº de Municípios` = dplyr::n(),
      `População Indígena Fora de TI (Censo 2022)` = sum(pop_mun_fora_ti, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `% do Total de Municípios` = 100 * `Nº de Municípios` / total_mun,
      `% da População Fora de TI` = 100 * `População Indígena Fora de TI (Censo 2022)` / total_pop
    ) |>
    dplyr::arrange(dplyr::desc(`Nº de Municípios`))

  linha_total <- tibble::tibble(
    `CR Declarante (SEI)` = "Total",
    `CR na Proposta` = "-",
    `Nº de Municípios` = sum(tabela$`Nº de Municípios`),
    `População Indígena Fora de TI (Censo 2022)` = sum(tabela$`População Indígena Fora de TI (Censo 2022)`),
    `% do Total de Municípios` = 100 * sum(tabela$`Nº de Municípios`) / total_mun,
    `% da População Fora de TI` = 100 * sum(tabela$`População Indígena Fora de TI (Censo 2022)`) / total_pop
  )

  tabela_final <- dplyr::bind_rows(tabela, linha_total)

  attr(tabela_final, "total_mun") <- total_mun
  attr(tabela_final, "total_pop") <- total_pop
  attr(tabela_final, "pct_mun_divergentes") <- round(100 * linha_total$`Nº de Municípios` / total_mun, 1)
  attr(tabela_final, "pct_pop_divergente") <- round(100 * linha_total$`População Indígena Fora de TI (Censo 2022)` / total_pop, 1)

  tabela_final
}

#' @param arquivo Nome do arquivo de saída (sem extensão). Se `NULL`, não exporta.
#' @return Objeto ggplot com o mapa gerado; o atributo `contagens` traz o resumo por status.
criar_mapa_divergencia_municipio_declarado <- function(
    arq_malha_basica_bruta,
    arq_municipios,
    df_declarados,
    arquivo = "mapa_divergencia_municipio_declarado"
) {

  status_mun <- calcular_divergencia_municipio_declarado(arq_malha_basica_bruta, df_declarados) |>
    dplyr::select(cd_mun, status)

  # Junta o status (calculado sobre a geometria recortada) ao contorno municipal completo.
  # Simplifica para exibição (ver simplificar_para_mapa()) -- os ~5.570 municípios do Brasil,
  # sem simplificar, produzem um objeto ggplot de mais de 1GB (geometria de resolução nativa do
  # IBGE, sem uso visual num mapa de extensão nacional).
  malha_mun <- arq_municipios |>
    dplyr::mutate(cd_mun = as.character(cd_mun)) |>
    dplyr::inner_join(status_mun, by = "cd_mun") |>
    simplificar_para_mapa()

  contagens <- malha_mun |> sf::st_drop_geometry() |> dplyr::count(status, name = "n")
  contar <- function(rotulo) {
    valor <- contagens$n[contagens$status == rotulo]
    if (length(valor) == 0) 0 else valor
  }
  n_conv <- contar("Convergente")
  n_div <- contar("Divergente")
  n_sem <- contar("Sem declaração")

  cores_status <- c(
    "Convergente" = "#2A9D8F",
    "Divergente" = "#E63946",
    "Sem declaração" = "#B0B0B0"
  )

  g <- ggplot2::ggplot(malha_mun) +
    ggplot2::geom_sf(ggplot2::aes(fill = status), color = "white", linewidth = 0.05) +
    ggplot2::scale_fill_manual(values = cores_status, name = "Situação") +
    ggplot2::labs(
      title = "Convergência entre a proposta e os municípios declarados por CR",
      subtitle = paste0(
        "Comparação entre a CR responsável na malha básica (TI e Município) e a CR que declarou o município (processo SEI n° 08620.010286/2021-89)\n",
        n_conv, " convergentes, ", n_div, " divergentes e ", n_sem, " sem declaração"
      ),
      caption = "Fonte: malha básica da proposta; declaração de atendimento por CR (processo SEI n° 08620.010286/2021-89)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    obter_tema(tipo = "mapa", cor = CORES_FUNAI[["principal"]])

  if (!is.null(arquivo)) exporta_grafico(g, arquivo)

  attr(g, "contagens") <- contagens
  g
}

#' @title Mapa de sobreposição de declarações de atendimento por município
#' @description Identifica os municípios declarados por mais de uma Coordenação Regional,
#' simultaneamente, na fonte bruta de declaração de atendimento (processo SEI n°
#' 08620.010286/2021-89 e site institucional da Funai, aba `UORG_MUN` de `cr_municipio.xlsx`,
#' ver `ARQ_ATENDIDOS_DECLARADOS`) -- antes do desempate por maior número de documento SEI
#' aplicado em `obter_municipios_declarados_cr()`/`imputar_atendimento_declarado()` para
#' resolver uma única CR por município. Município convergente (só uma CR declarante) fica em
#' cinza claro como referência; município sobreposto é destacado e rotulado em formato de
#' "chip" (mesmo estilo de `criar_mapa_divergencia_ti_sii()`): nome do município e a lista de
#' siglas das CRs que o declararam.
#' @param arq_tabela_declarados Caminho do xlsx da declaração (alvo `atendidos_declarados`),
#'   com a aba `UORG_MUN` (`cd_mun`, `codigoUnidade`, `sei`).
#' @param arq_municipios Objeto sf dos municípios brasileiros (alvo `arq_municipios`).
#' @param arq_siorg Data.frame do SIORG (alvo `arq_siorg`), usado para obter a sigla da CR.
#' @param arquivo Nome do arquivo de saída (sem extensão). Se `NULL`, não exporta.
#' @return Objeto ggplot com o mapa gerado.
criar_mapa_sobreposicao_municipios_declarados <- function(
    arq_tabela_declarados,
    arq_municipios,
    arq_siorg,
    arquivo = "mapa_sobreposicao_municipios_declarados"
) {

  lookup_sigla <- arq_siorg |>
    dplyr::transmute(codigoUnidade = as.numeric(codigoUnidade), sigla) |>
    dplyr::distinct()

  # Uma linha por (município, CR declarante) -- descarta reenvios/atualizações do mesmo par
  # (ex.: mesma CR redeclarando o mesmo município com um número de SEI mais recente), que não
  # configuram sobreposição de fato.
  declaracoes_unicas <- readxl::read_excel(arq_tabela_declarados, sheet = "UORG_MUN") |>
    dplyr::mutate(cd_mun = as.character(cd_mun), codigoUnidade = as.numeric(codigoUnidade)) |>
    dplyr::distinct(cd_mun, codigoUnidade) |>
    dplyr::left_join(lookup_sigla, by = "codigoUnidade")

  sobrepostos <- declaracoes_unicas |>
    dplyr::group_by(cd_mun) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::summarise(
      n_crs = dplyr::n(),
      siglas = paste(sort(trimws(sigla)), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::mutate(status = paste0(n_crs, " CRs"))

  n_sobrepostos <- nrow(sobrepostos)
  cli::cli_alert_info("{n_sobrepostos} munic{?ípio/ípios} com declara{?ção/ções} sobreposta{?s}.")

  malha_mun <- arq_municipios |>
    dplyr::mutate(cd_mun = as.character(cd_mun)) |>
    dplyr::left_join(sobrepostos, by = "cd_mun")

  mun_sobrepostos <- malha_mun |> dplyr::filter(!is.na(status))

  cores_status <- c("2 CRs" = CORES_FUNAI[["principal"]], "3 CRs" = CORES_FUNAI[["critico"]])

  # Rótulo em "chip" (nome do município + siglas das CRs declarantes), mesmo estilo de
  # criar_mapa_divergencia_ti_sii() -- posição resolvida por ggrepel sobre o ponto interno do
  # município.
  centroides_sobrepostos <- mun_sobrepostos |>
    sf::st_transform(4326) |>
    sf::st_point_on_surface() |>
    dplyr::mutate(rotulo = paste0(nm_mun, "\n", siglas))

  sf_col_sob <- attr(centroides_sobrepostos, "sf_column")
  names(centroides_sobrepostos)[names(centroides_sobrepostos) == sf_col_sob] <- "geometry"
  sf::st_geometry(centroides_sobrepostos) <- "geometry"

  g <- ggplot2::ggplot() +

    # Municípios convergentes (referência, cinza claro)
    ggplot2::geom_sf(
      data = malha_mun |> sf::st_transform(4326) |> dplyr::filter(is.na(status)),
      fill = "#D6D6D6",
      color = "#EAEAEA",
      linewidth = 0.03
    ) +

    # Municípios sobrepostos, coloridos por número de CRs declarantes
    ggplot2::geom_sf(
      data = mun_sobrepostos |> sf::st_transform(4326),
      ggplot2::aes(fill = status),
      color = "#1A1A1A",
      linewidth = 0.15
    ) +

    ggrepel::geom_label_repel(
      data = centroides_sobrepostos,
      ggplot2::aes(geometry = geometry, label = rotulo, color = status),
      stat = "sf_coordinates",
      size = 1.5,
      fontface = "bold",
      lineheight = 0.9,
      label.size = 0,
      label.r = grid::unit(0.25, "lines"),
      label.padding = grid::unit(0.15, "lines"),
      fill = scales::alpha("white", 0.9),
      seed = 42,
      max.overlaps = Inf,
      min.segment.length = 0,
      segment.size = 0.2,
      box.padding = 0.25,
      point.padding = 0.05,
      force = 3,
      force_pull = 0.5,
      max.iter = 20000,
      max.time = 3
    ) +

    ggplot2::scale_fill_manual(values = cores_status, name = "Nº de CRs declarantes") +
    ggplot2::scale_color_manual(values = cores_status, guide = "none") +

    ggplot2::coord_sf(crs = sf::st_crs(4326), expand = FALSE) +

    ggplot2::labs(
      title = "Municípios com declaração de atendimento sobreposta entre Coordenações Regionais",
      subtitle = paste0(
        "Municípios declarados por mais de uma CR na fonte bruta (processo SEI e site institucional), antes do desempate\n",
        "(", n_sobrepostos, " município", ifelse(n_sobrepostos == 1, "", "s"), " sobreposto",
        ifelse(n_sobrepostos == 1, "", "s"), " identificado", ifelse(n_sobrepostos == 1, "", "s"), ")"
      ),
      caption = "Fonte: declaração de atendimento por CR (processo SEI n° 08620.010286/2021-89 e site institucional da Funai)"
    ) +

    ggplot2::theme_minimal(base_size = 10) +
    obter_tema(tipo = "mapa", cor = CORES_FUNAI[["principal"]]) +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 8),
      legend.text = ggplot2::element_text(size = 7.5),
      legend.key.size = grid::unit(0.4, "cm"),
      legend.margin = ggplot2::margin(t = -4),
      plot.title = ggplot2::element_text(size = 10, face = "bold", color = CORES_FUNAI[["principal"]]),
      plot.subtitle = ggplot2::element_text(size = 8, color = "grey35")
    )

  if (!is.null(arquivo)) exporta_grafico(g, arquivo)

  g
}

#' @title Mapa Coroplético de Perda Logística (Impacto das Imputações)
#' @description Calcula e mapeia a perda em tempo de viagem (custo adicional)
#' decorrente das imputações manuais e suavizações topológicas.
#' @param arq_suavizado Caminho para o geopackage da malha suavizada final.
#' @param df_otimo Tabela com a correspondência ótima algorítmica (df_imputacao).
#' @param df_roteamento Tabela de roteamento filtrada contendo cd_setor, cd_uorg e duracao.
#' @param sf_pontos Objeto sf contendo os pontos das CRs.
#' @return Objeto ggplot com o mapa.
#' Não usada por nenhum alvo do pipeline no momento -- mantida fora de _targets.R para facilitar
#' eventual reinserção futura (ver mesmo raciocínio em tabela_dados_sc(), acima).
criar_mapa_perda_logistica <- function(arq_suavizado, df_otimo, df_roteamento, sf_pontos) {

  #' =========================================================================
  # DEBUG START: Selecione e rode as linhas abaixo para debug interativo
  # arq_suavizado <- targets::tar_read(arquivo_gpkg_suavizado_v2)
  # df_otimo      <- targets::tar_read(df_imputacao)
  # df_roteamento <- arrow::open_dataset(targets::tar_read(tabela_osrm_consolidada)) |>
  #   dplyr::select(cd_setor, cd_uorg, duracao) |> dplyr::collect()
  # sf_pontos     <- targets::tar_read(sf_pontos_filtrados)
  #' =========================================================================

  crs_projeto <- CRS_ALBERS_BR

  # 1. Carrega a malha final de setores e projeta para Albers
  sf_setores_final <- sf::st_read(arq_suavizado, quiet = TRUE) |>
    sf::st_transform(crs_projeto)

  # 2. Isola o baseline ótimo (menor tempo do algoritmo)
  df_baseline <- df_otimo |>
    dplyr::select(cd_setor, duracao_otima = duracao) |>
    dplyr::mutate(cd_setor = as.character(cd_setor))

  # 3. Prepara a tabela com as durações OSRM já coletadas da memória
  df_duracoes <- df_roteamento |>
    dplyr::select(cd_setor, cd_uorg, duracao_final = duracao) |>
    dplyr::mutate(
      cd_setor = as.character(cd_setor),
      cd_uorg = as.character(cd_uorg)
    )

  # 4. Cruzamento de dados e cálculo da perda
  sf_plot <- sf_setores_final |>
    dplyr::mutate(
      cd_setor = as.character(cd_setor),
      cd_uorg = as.character(cd_uorg)
    ) |>
    dplyr::left_join(df_baseline, by = "cd_setor") |>
    dplyr::left_join(df_duracoes, by = c("cd_setor", "cd_uorg")) |>
    dplyr::mutate(
      # Fallback preventivo
      duracao_final = dplyr::coalesce(duracao_final, duracao_otima),

      perda_min = duracao_final - duracao_otima,
      perda_min = dplyr::if_else(perda_min < 0, 0, perda_min),
      perda_horas = perda_min / 60,

      classe_perda = dplyr::case_when(
        perda_horas <= 0.1 ~ "1. Sem perda (Ótimo)",
        perda_horas <= 4   ~ "2. Até 4h (Meio dia)",
        perda_horas <= 8   ~ "3. 4h a 8h (1 dia)",
        perda_horas <= 16  ~ "4. 8h a 16h (2 dias)",
        perda_horas <= 24  ~ "5. 16h a 24h (3 dias)",
        TRUE               ~ "6. Mais de 24h (> 3 dias)"
      )
    ) |>
    dplyr::mutate(
      classe_perda = factor(classe_perda, levels = c(
        "1. Sem perda (Ótimo)",
        "2. Até 4h (Meio dia)",
        "3. 4h a 8h (1 dia)",
        "4. 8h a 16h (2 dias)",
        "5. 16h a 24h (3 dias)",
        "6. Mais de 24h (> 3 dias)"
      ))
    )

  # 5. Renderização
  g <- ggplot2::ggplot() +

    ggplot2::geom_sf(
      data = sf_plot,
      ggplot2::aes(fill = classe_perda),
      color = "transparent"
    ) +

    ggplot2::geom_sf(
      data = sf_pontos |> sf::st_transform(crs_projeto),
      color = "black",
      fill = "white",
      shape = 21,
      size = 1.5,
      stroke = 0.5
    ) +

    # Rampa sequencial de um único matiz (vermelho), clara -> escura, validada com o skill
    # dataviz (scripts/validate_palette.js --ordinal): a versão anterior misturava um cinza
    # neutro na 1a classe com uma rampa vermelha nas demais (spread de matiz de 63°, falhando o
    # teste de "matiz único") e não era monotônica em luminosidade (a 2a classe, #fee5d9, era
    # mais CLARA que a 1a, #e0e0e0 -- a leitura "clara = pouca perda, escura = muita perda"
    # ficava invertida entre as duas primeiras classes).
    ggplot2::scale_fill_manual(
      values = c(
        "1. Sem perda (Ótimo)" = "#fee5d9",
        "2. Até 4h (Meio dia)" = "#fcae91",
        "3. 4h a 8h (1 dia)"   = "#fb6a4a",
        "4. 8h a 16h (2 dias)" = "#de2d26",
        "5. 16h a 24h (3 dias)"= "#a50f15",
        "6. Mais de 24h (> 3 dias)" = "#67000d"
      ),
      name = "Aumento no Tempo de Viagem\n(Jornada de 8h)",
      drop = FALSE
    ) +

    ggplot2::labs(
      title = "Impacto Logístico das Readequações Territoriais (Versão Final)",
      subtitle = "Custo adicional em horas/dias de viagem gerado pelas imputações e correções manuais",
      caption = "Fonte: Algoritmo OSRM e Malha IBGE (Censo 2022)"
    ) +

    obter_tema(tipo = "mapa", cor = CORES_FUNAI[["critico"]]) +

    ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9)
    )

  if (exists("exporta_grafico")) {
    exporta_grafico(g, "mapa_perda_logistica_imputacoes")
  }

  return(g)
}

#' @title Gráfico da série histórica de população indígena em TIs (SIASI)
#' @description Gráfico de linha com a evolução anual da população indígena em Terras
#' Indígenas, segundo os extratos do SIASI (ver `importar_siasi_serie_historica()`).
#' @param df_serie Tibble com colunas `ano` e `populacao_indigena` (alvo `serie_historica_pi_siasi`).
#' @return Objeto ggplot.
grafico_serie_historica_pi_siasi <- function(df_serie) {

  # Eixo Y deliberadamente restrito ao intervalo dos dados (não ancorado em 0): a série varia
  # numa faixa estreita (~9%) e um eixo iniciado em 0 achataria a variação ano a ano a ponto de
  # torná-la imperceptível.
  faixa <- range(df_serie$populacao_indigena)
  folga <- diff(faixa) * 0.15

  # Evento: ADPF 709, medida cautelar concedida ad referendum em 08/07/2020 e posteriormente
  # referendada pelo min. rel. Roberto Barroso, que determinou, entre outras medidas, a imediata
  # extensão dos serviços do Subsistema de Atenção à Saúde Indígena aos povos aldeados situados em
  # terras não homologadas e, quando verificada barreira de acesso ao SUS geral, aos povos
  # indígenas não aldeados. Posicionada na fração do ano correspondente a 8 de julho (dia 190 de
  # 366 em 2020, ano bissexto), já que o eixo X é contínuo por ano, não categórico.
  data_evento <- 2020 + 190 / 366

  g <- ggplot2::ggplot(df_serie, ggplot2::aes(x = ano, y = populacao_indigena)) +
    ggplot2::geom_vline(
      xintercept = data_evento,
      linetype = "dashed",
      color = "grey40",
      linewidth = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x = data_evento + 0.08,
      y = faixa[2] + folga * 1.35,
      label = "ADPF 709 (MC)\n08/07/2020",
      hjust = 0,
      vjust = 1,
      lineheight = 0.9,
      size = 2.6,
      color = "grey30"
    ) +
    ggplot2::geom_line(color = CORES_FUNAI[["principal"]], linewidth = 1) +
    ggplot2::geom_point(color = CORES_FUNAI[["principal"]], size = 2) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::label_number(big.mark = ".", decimal.mark = ",")(round(populacao_indigena))
      ),
      vjust = -1,
      size = 3,
      fontface = "bold",
      color = CORES_FUNAI[["principal"]]
    ) +
    ggplot2::scale_x_continuous(breaks = df_serie$ano) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      limits = c(faixa[1] - folga, faixa[2] + folga * 1.6)
    ) +
    ggplot2::labs(
      title = "População Indígena em Terras Indígenas (2020-2026)",
      subtitle = "Série histórica da população indígena aldeada, segundo o SIASI",
      x = NULL,
      y = "População Indígena",
      caption = "Fonte: Sistema de Informação da Atenção à Saúde Indígena (SIASI/SasiSUS)."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    obter_tema(tipo = "barra", cor = CORES_FUNAI[["principal"]])

  exporta_grafico(g, "serie_historica_pi_siasi")

  return(g)
}

#' @title Tabela de Terras Indígenas por Coordenação Regional (SII), desagregada por fase
#' @description Conta o número de Terras Indígenas vinculadas a cada Coordenação Regional
#' segundo a vinculação atualmente registrada no SII, desagregando por fase (Em Estudo,
#' Delimitada, Declarada, Homologada, Regularizada, Encaminhada RI), com o total geral na
#' última coluna e uma linha de total Brasil.
#'
#' Além da camada de polígonos (`arq_tis`), incorpora `arq_ti_estudo` -- a camada de PONTOS de
#' TIs ainda sem delimitação geométrica alguma (portarias de constituição de Grupo Técnico), que
#' fica de fora de `arq_tis` por definição (não tem polígono para entrar numa camada poligonal).
#' Todo registro de `arq_ti_estudo` tem `fase_ti == "Em Estudo"`, então entra somando nessa coluna.
#'
#' Cuidado com dupla contagem: `arq_ti_estudo` também contém "reestudos" -- pedidos de revisão de
#' TIs que já têm polígono e já estão em fase avançada (observado: todos os casos cruzados por
#' nome/código com `arq_tis` estavam "Regularizada"). Contar esses reestudos somaria a mesma TI
#' duas vezes (uma pela fase real, outra como "Em Estudo"). O campo `reestudo_ti` sinaliza esse
#' caso (valor "Reestudo", case-insensitive) e é usado para excluir essas linhas -- únicos
#' registros de `arq_ti_estudo` mantidos são os de primeiro estudo (`reestudo_ti` em branco,
#' "principal"/"Principal" ou `NA`), que não têm nenhum polígono correspondente em `arq_tis`.
#' @param arq_tis Objeto sf com as TIs delimitadas (alvo `arq_tis`), contendo `undadm_codigo`,
#'   `fase_ti`, `terrai_codigo`, `terrai_nome`, `reestudo_ti`.
#' @param arq_ti_estudo Objeto sf com as TIs ainda sem delimitação geométrica (alvo
#'   `arq_ti_estudo`, camada `tis_pontos_portarias`), mesmas colunas de `arq_tis`.
#' @param arq_uorg_ug Data.frame de correspondência entre código SII (`cd_sii`) e código UORG
#'   (`cd_uorg`). Usado para traduzir `undadm_codigo` -> CR do SII (ver `adicionar_cd_sii()`).
#' @param dicionario_cr Dicionário de CRs (com `codigo_siorg`/`sigla` já atualizados a partir do
#'   SIORG vigente, ver `atualizar_nomes_dicionario_cr()`) usado tanto para derivar `cd_sii`
#'   quanto como fonte canônica da sigla exibida na tabela.
#' @return Tibble com uma linha por CR (+ linha "Sem vinculação (SII)" e linha "Brasil") e uma
#'   coluna por fase + Total.
tabela_ti_fase_cr <- function(arq_tis, arq_ti_estudo, arq_uorg_ug, dicionario_cr) {

  ordem_fases <- c("Em Estudo", "Delimitada", "Declarada", "Homologada", "Regularizada", "Encaminhada RI")
  sigla_sem_vinculacao <- "Sem vinculação (SII)"

  # Sigla de CR resolvida pela vinculação no SII (undadm_codigo -> cd_sii -> cd_uorg -> sigla
  # vigente), o mesmo caminho já usado em criar_mapa_divergencia_ti_sii()/
  # calcular_jurisdicao_atual_bruta() -- NUNCA o texto solto em undadm_sigla, coluna
  # denormalizada trazida pelo WFS da Funai que traz siglas de CRs já extintas/renomeadas pela
  # reestruturação e variações de digitação (ex.: "CR-LISE", "CR- JPR", "CR-Mpur"), produzindo
  # siglas "fantasma" sem CR correspondente na estrutura vigente.
  arq_uorg_ug_sii <- adicionar_cd_sii(arq_uorg_ug, dicionario_cr)
  lookup_sigla_sii <- arq_uorg_ug_sii |>
    dplyr::transmute(cd_sii, cd_uorg = as.character(cd_uorg)) |>
    dplyr::filter(!is.na(cd_sii)) |>
    dplyr::left_join(
      dicionario_cr |> dplyr::transmute(cd_uorg = as.character(codigo_siorg), sigla),
      by = "cd_uorg"
    ) |>
    dplyr::distinct(cd_sii, sigla)

  resolver_sigla <- function(dados) {
    dados |>
      dplyr::mutate(undadm_codigo = as.character(undadm_codigo)) |>
      dplyr::left_join(lookup_sigla_sii, by = c("undadm_codigo" = "cd_sii")) |>
      dplyr::mutate(sigla = dplyr::coalesce(sigla, sigla_sem_vinculacao))
  }

  base_poligonos <- arq_tis |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(fase_ti)) |>
    resolver_sigla() |>
    dplyr::select(sigla, fase_ti)

  # Só os estudos de primeira vez (sem polígono em arq_tis) -- exclui reestudos de TIs já
  # delimitadas para não contar a mesma TI duas vezes (ver descrição da função).
  base_pontos <- arq_ti_estudo |>
    sf::st_drop_geometry() |>
    dplyr::filter(is.na(reestudo_ti) | tolower(trimws(reestudo_ti)) != "reestudo") |>
    dplyr::mutate(fase_ti = "Em Estudo") |>
    resolver_sigla() |>
    dplyr::select(sigla, fase_ti)

  base <- dplyr::bind_rows(base_poligonos, base_pontos) |>
    dplyr::mutate(fase_ti = factor(fase_ti, levels = ordem_fases))

  tabela <- base |>
    dplyr::count(sigla, fase_ti, name = "n", .drop = FALSE) |>
    tidyr::pivot_wider(names_from = fase_ti, values_from = n, values_fill = 0) |>
    dplyr::mutate(Total = rowSums(dplyr::across(dplyr::all_of(ordem_fases)))) |>
    dplyr::filter(Total > 0) |>
    # "Sem vinculação (SII)" sempre por último entre as linhas de CR (imediatamente antes da
    # linha "Brasil"), por ser um resíduo e não uma unidade regional comparável às demais.
    dplyr::arrange(sigla == sigla_sem_vinculacao, dplyr::desc(Total)) |>
    dplyr::rename(Sigla = sigla)

  total <- tabela |>
    dplyr::summarise(
      Sigla = "Brasil",
      dplyr::across(dplyr::all_of(ordem_fases), sum),
      Total = sum(Total)
    )

  dplyr::bind_rows(tabela, total) |>
    dplyr::select(Sigla, dplyr::all_of(ordem_fases), Total)
}

#' @title Extrai a população indígena por município a partir da tabela bruta do SIDRA
#' @description Converte a tabela bruta do SIDRA (alvo `tb_pop_pi`) num tibble de população
#' indígena por município, tratando o sentinela "-" do SIDRA (município sem população
#' indígena registrada) como zero.
#' @param tb_pop_mun Tabela bruta do SIDRA de população indígena por município (alvo `tb_pop_pi`).
#' @return Tibble com `cd_mun` (character) e `pop_pi` (numeric).
obter_pop_indigena_municipio <- function(tb_pop_mun) {
  tb_pop_mun |>
    dplyr::slice(-1) |>
    dplyr::transmute(
      cd_mun = as.character(D1C),
      # SIDRA usa "-" para município sem população indígena registrada (equivalente a 0)
      pop_pi = as.numeric(ifelse(V == "-", "0", V))
    )
}

#' @title Tabela-resumo da precisão da geocodificação (apenas Coordenações Regionais)
#' @description Resume, por categoria de precisão do `geocodebr`, o número, o percentual e o
#' desvio médio (em metros) das Coordenações Regionais (excluídas as Unidades Técnicas Locais)
#' cujo endereço do SIORG foi geocodificado com aquele nível de precisão.
#' @param sf_pontos_filtrados Objeto sf de pontos geocodificados já filtrado para `sigla`
#'   iniciada em "CR-" (alvo `sf_pontos_filtrados`), contendo `precisao` e `desvio_metros`.
#' @return Tibble com uma linha por categoria de precisão, ordenada da mais para a menos precisa.
tabela_precisao_geocode_cr <- function(sf_pontos_filtrados) {

  ordem_precisao <- c("numero", "numero_aproximado", "logradouro", "localidade", "cep", "municipio")

  rotulos_precisao <- c(
    numero             = "Número exato",
    numero_aproximado  = "Número aproximado",
    logradouro         = "Logradouro",
    localidade         = "Localidade/Bairro",
    cep                = "CEP",
    municipio          = "Município"
  )

  sf_pontos_filtrados |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(precisao)) |>
    dplyr::mutate(precisao = factor(precisao, levels = ordem_precisao)) |>
    dplyr::group_by(precisao) |>
    dplyr::summarise(
      n_crs = dplyr::n(),
      desvio_medio_m = mean(desvio_metros, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct = 100 * n_crs / sum(n_crs),
      `Categoria de Precisão` = rotulos_precisao[as.character(precisao)]
    ) |>
    dplyr::arrange(precisao) |>
    dplyr::select(
      `Categoria de Precisão`,
      `Nº de CRs` = n_crs,
      `% de CRs` = pct,
      `Desvio Médio (m)` = desvio_medio_m
    )
}

#' @title Tabela-síntese de setores censitários por critério de associação à CR
#' @description Quantifica a proporção nacional de setores censitários, área e população indígena
#' atribuída a cada critério de associação usado para montar a malha básica por setor censitário --
#' a mesma coluna de controle `criterio` citada como garantia de rastreabilidade na seção "Ajustes e
#' Imputação Manual", e a mesma base de dados da Figura 3 (Critérios de Associação). Criada para
#' substituir a leitura puramente qualitativa do mapa por um número: a imputação manual (CGGE + GT
#' CR-MSOL) não é um ajuste marginal sobre um resultado majoritariamente algorítmico, e a coluna de
#' controle já permite quantificar essa proporção -- o texto apenas não o fazia.
#' @param arq_setores Caminho do gpkg de setores censitários com a coluna `criterio` (alvo
#'   `arquivo_gpkg_suavizado_v2_msol`).
#' @param df_pi Data.frame com população indígena por setor censitário (alvo `censo_agregado_sc_pi`).
#' @return Tibble com uma linha por critério de associação, mais uma linha de total: número de
#'   setores, área (km²) e população indígena (Censo 2022), cada um também como percentual do total
#'   nacional. Os percentuais combinados de imputação manual (CGGE + GT CR-MSOL) ficam nos atributos
#'   `pct_setores_manual`/`pct_area_manual`/`pct_pop_manual`.
#' @details A malha vetorial de setores do IBGE traz, além dos 468.097 setores censitários, duas
#'   áreas operacionais (Lagoa Mirim, `430000100000000`, e Lagoa dos Patos, `430000200000000`) que
#'   não são setores. Elas são mantidas como estão na malha -- contadas como duas unidades a mais
#'   no total (468.099) e com sua área no total de área --, o que a nota técnica explica no texto e
#'   na fonte da Tabela 5.
montar_tabela_criterios_associacao <- function(arq_setores, df_pi) {

  df <- sf::st_read(
    arq_setores,
    query = glue::glue("SELECT cd_setor, criterio, area_km2 FROM {sf::st_layers(arq_setores)$name}"),
    geometry_column = NA
  )

  # limpar_sigilo_ibge(): "X" (célula suprimida pelo sigilo estatístico do IBGE) -> 0, mesma
  # convenção usada em todo o resto do pipeline (ver exportar_indigenas_duckdb() em
  # R/define_divisoes_cr.R). Consequência quantificada e explicada em nt_jurisdicoes.qmd, seção
  # "Limitações da Proposta por Setores Censitários".
  df_pi_limpo <- df_pi |>
    sf::st_drop_geometry() |>
    dplyr::mutate(total_pi = limpar_sigilo_ibge(v01690)) |>
    dplyr::select(cd_setor, total_pi)

  base <- df |> dplyr::left_join(df_pi_limpo, by = "cd_setor")

  total_setores <- nrow(base)
  total_area <- sum(base$area_km2, na.rm = TRUE)
  total_pop <- sum(base$total_pi, na.rm = TRUE)

  tabela <- base |>
    dplyr::group_by(`Critério de Associação` = criterio) |>
    dplyr::summarise(
      `Nº de Setores` = dplyr::n(),
      `Área (km²)` = sum(area_km2, na.rm = TRUE),
      `População Indígena (Censo 2022)` = sum(total_pi, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `% de Setores` = 100 * `Nº de Setores` / total_setores,
      `% da Área` = 100 * `Área (km²)` / total_area,
      `% da População Indígena` = 100 * `População Indígena (Censo 2022)` / total_pop
    ) |>
    dplyr::arrange(dplyr::desc(`Nº de Setores`))

  linha_total <- tibble::tibble(
    `Critério de Associação` = "Total",
    `Nº de Setores` = total_setores,
    `Área (km²)` = total_area,
    `População Indígena (Censo 2022)` = total_pop,
    `% de Setores` = 100,
    `% da Área` = 100,
    `% da População Indígena` = 100
  )

  tabela_final <- dplyr::bind_rows(tabela, linha_total)

  manual <- c("Imputação Manual (CGGE)", "Imputação Manual (GT CR-MSOL)")
  linhas_manuais <- tabela |> dplyr::filter(`Critério de Associação` %in% manual)

  attr(tabela_final, "pct_setores_manual") <- round(100 * sum(linhas_manuais$`Nº de Setores`) / total_setores, 1)
  attr(tabela_final, "pct_area_manual") <- round(100 * sum(linhas_manuais$`Área (km²)`) / total_area, 1)
  attr(tabela_final, "pct_pop_manual") <- round(100 * sum(linhas_manuais$`População Indígena (Censo 2022)`) / total_pop, 1)

  tabela_final
}

#' @title Município-CR conforme a declaração de atendimento (processo SEI)
#' @description Lê a declaração de atendimento por CR (processo SEI n° 08620.010286/2021-89,
#' ver `ARQ_ATENDIDOS_DECLARADOS` em `R/config.R`) e resolve, para cada município, a CR
#' declarada, desempatando pelo maior número de SEI quando há mais de uma declaração para o
#' mesmo município (mesmo critério de `imputar_atendimento_declarado()`).
#' @param arq_tabela_declarados Caminho do xlsx da declaração (alvo `atendidos_declarados`),
#'   com a aba `UORG_MUN` (`cd_mun`, `codigoUnidade`, `unidade`, `sei`).
#' @param arq_siorg Data.frame do SIORG (alvo `arq_siorg`), usado para obter a sigla da CR.
#' @return Tibble com `cd_mun`, `codigoUnidade`, `nome_cr`, `sigla`.
obter_municipios_declarados_cr <- function(arq_tabela_declarados, arq_siorg) {

  df <- readxl::read_excel(arq_tabela_declarados, sheet = "UORG_MUN")

  df |>
    dplyr::mutate(sei = as.numeric(sei), cd_mun = as.character(cd_mun)) |>
    dplyr::group_by(cd_mun) |>
    dplyr::slice_max(order_by = sei, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      cd_mun,
      codigoUnidade = as.numeric(codigoUnidade),
      nome_cr = unidade
    ) |>
    dplyr::left_join(
      arq_siorg |>
        dplyr::transmute(codigoUnidade = as.numeric(codigoUnidade), sigla) |>
        dplyr::distinct(),
      by = "codigoUnidade"
    )
}

#' @title Mapa de concentração indígena e carência de atendimento declarado por CR
#' @description Mapa coroplético único de população indígena por município (Censo 2022),
#' destacando com contorno os municípios com presença indígena que nenhuma Coordenação Regional
#' declarou atender (processo SEI n° 08620.010286/2021-89) -- a informação central do mapa, já
#' que evidencia onde a carência de atendimento formal se sobrepõe à concentração populacional
#' real -- e sobrepondo os pontos de localidades indígenas do IBGE como referência de presença no
#' território. Substitui os antigos `criar_mapa_pi_municipio_regic()` (concentração + fluxos
#' REGIC) e `criar_mapa_pi_municipio_declarado()` (concentração nos municípios já declarados),
#' cuja base visual (coroplético de PI por município) era redundante entre si.
#' @param sf_municipios Objeto sf dos municípios brasileiros (alvo `arq_municipios`).
#' @param tb_pop_mun Tabela bruta do SIDRA de população indígena por município (alvo `tb_pop_pi`).
#' @param df_declarados Município-CR declarados (alvo `municipios_declarados_cr`, ver
#'   `obter_municipios_declarados_cr()`).
#' @param sf_localidades Objeto sf de pontos de localidades indígenas do IBGE (alvo
#'   `arq_localidades_indigenas`).
#' @param limiar_concentracao População indígena mínima para um município com PI e sem CR
#'   declarada ser destacado como carência (padrão: 100). Sem esse corte, o contorno cobriria a
#'   maior parte do mapa -- quase todo município do país tem ao menos 1 pessoa autodeclarada
#'   indígena no Censo -- e deixaria de evidenciar onde a carência realmente concentra população.
#' @param cor Cor principal da escala de preenchimento.
#' @param cor_carencia Cor de destaque (contorno) dos municípios com PI e sem CR declarada.
#' @param arquivo Nome do arquivo de exportação (sem extensão). Se `NULL`, não exporta.
#' @return Objeto ggplot.
criar_mapa_pi_carencia_atendimento <- function(
    sf_municipios,
    tb_pop_mun,
    df_declarados,
    sf_localidades,
    limiar_concentracao = 100,
    cor = CORES_FUNAI[["principal"]],
    cor_carencia = CORES_FUNAI[["critico"]],
    arquivo = NULL
) {

  crs_calc <- CRS_ALBERS_BR
  pop_mun <- obter_pop_indigena_municipio(tb_pop_mun)

  sf_mun_pi <- sf_municipios |>
    sf::st_transform(crs_calc) |>
    dplyr::mutate(cd_mun = as.character(cd_mun)) |>
    dplyr::left_join(pop_mun, by = "cd_mun") |>
    dplyr::mutate(
      pop_pi = dplyr::na_if(pop_pi, 0),
      declarado = cd_mun %in% df_declarados$cd_mun
    )

  # Restrito a municípios com concentração relevante (>= limiar_concentracao): praticamente todo
  # município do país tem ao menos 1 pessoa autodeclarada indígena no Censo, então sem esse corte
  # o contorno de carência cobriria a maior parte do mapa e deixaria de destacar o que importa --
  # onde a concentração indígena real coincide com a ausência de CR declarada.
  sf_mun_carencia <- sf_mun_pi |>
    dplyr::filter(!is.na(pop_pi), pop_pi >= limiar_concentracao, !declarado)

  sf_pontos_localidades <- sf_localidades |>
    sf::st_transform(crs_calc)

  n_carencia <- nrow(sf_mun_carencia)
  pi_carencia <- sum(sf_mun_carencia$pop_pi, na.rm = TRUE)
  fmt_numero <- scales::label_number(big.mark = ".", decimal.mark = ",")

  # Rótulos das duas camadas de referência (pontos de localidade + contorno de carência), unidos
  # numa única legenda categórica de "color" -- antes só apareciam explicados no subtítulo (texto
  # pequeno, fácil de não notar) sem nenhuma chave visual no próprio mapa. override.aes desenha o
  # ícone certo por chave (ponto para localidades, linha para o contorno de carência) já que as
  # duas camadas dividem a mesma escala de cor.
  rotulo_localidades <- "Localidades indígenas (IBGE)"
  rotulo_carencia <- paste0("Município sem CR declarada (≥ ", limiar_concentracao, " indígenas)")

  g <- ggplot2::ggplot(sf_mun_pi) +
    ggplot2::geom_sf(fill = "grey96", color = "white", linewidth = 0.05) +
    ggplot2::geom_sf(ggplot2::aes(fill = pop_pi), color = "white", linewidth = 0.05) +
    ggplot2::geom_sf(
      data = sf_pontos_localidades,
      ggplot2::aes(color = rotulo_localidades),
      size = 0.3, alpha = 0.75, shape = 16
    ) +
    ggplot2::geom_sf(
      data = sf_mun_carencia,
      ggplot2::aes(color = rotulo_carencia),
      fill = NA, linewidth = 0.45
    ) +
    ggplot2::scale_fill_gradient(
      trans = "sqrt",
      low = extremo_claro_hcl(cor),
      high = cor,
      na.value = "grey96",
      labels = fmt_numero,
      name = "População Indígena\n"
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      breaks = c(rotulo_localidades, rotulo_carencia),
      values = stats::setNames(c("#E9A227", cor_carencia), c(rotulo_localidades, rotulo_carencia)),
      guide = ggplot2::guide_legend(
        override.aes = list(
          shape     = c(16, NA),
          linetype  = c(0, 1),
          size      = c(2, NA),
          linewidth = c(NA, 0.8),
          alpha     = c(1, 1)
        )
      )
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        barwidth = grid::unit(8, "cm"),
        barheight = grid::unit(0.5, "cm"),
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    ggplot2::labs(
      title = "Concentração Indígena e Carência de Atendimento Declarado por CR",
      subtitle = paste0(
        "População indígena por município (Censo 2022)\n",
        n_carencia, " municípios em carência de atendimento (", fmt_numero(pi_carencia), " pessoas) -- ver legenda"
      ),
      caption = paste0(
        "Fonte: IBGE (Censo Demográfico 2022; Localidades Indígenas 2022); ",
        "declaração de atendimento por CR (processo SEI n° 08620.010286/2021-89)"
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    obter_tema(tipo = "mapa", cor = cor) +
    ggplot2::theme(legend.box = "vertical")

  if (!is.null(arquivo)) {
    exporta_grafico(g, arquivo)
  }

  g
}

#' @title Monta a tabela de encontros virtuais regionais (Tabela 9 da nota técnica)
#' @description Para cada agrupamento de CRs definido em `GRUPOS_ENCONTROS_REGIONAIS`
#' (`R/config.R`), calcula (a) a Região predominante do encontro, a partir de onde cada CR do
#' agrupamento tem a maior parte de seu território (sobreposição de área com a malha de
#' Grandes Regiões do IBGE -- não a UF do escritório da CR, que pode não refletir onde está a
#' maior parte do território atendido), e (b) as CRs de "participação optativa": CRs que não
#' integram o agrupamento mas fazem fronteira com ao menos uma CR que integra, calculado via
#' grafo de adjacência territorial (`sf::st_touches()`) sobre o polígono final da malha básica
#' já agrupado por CR.
#' @details `arq_poligonos_cr` já tem um polígono por `cd_uorg` (ver `unir_poligonos_final()`
#' em `R/define_divisoes_cr.R`), não precisa de dissolve adicional aqui.
#' @param arq_poligonos_cr Caminho do GeoPackage com um polígono por CR (ex.:
#' `arquivo_final_sc_mun`), colunas `cd_uorg`/`nome_uorg`/`sigla`.
#' @param arq_regioes Objeto sf da malha de Grandes Regiões do IBGE (alvo `arq_regioes` em
#' `_targets.R` -- `format` padrão, já é o sf retornado por `baixar_shapefile_zip()`, não um
#' caminho de arquivo), coluna `nm_regiao`.
#' @param grupos Lista com um elemento por encontro, na ordem da tabela (padrão:
#' `GRUPOS_ENCONTROS_REGIONAIS` em `R/config.R`). Cada elemento é uma lista com `obrigatoria`
#' (vetor de siglas) e, opcionalmente, `regiao` e `opcional` -- quando informados, prevalecem
#' sobre o cálculo (a) e (b) acima. Um vetor de siglas simples equivale a `list(obrigatoria = ...)`.
#' @param crs_area CRS métrico para o cálculo de área de sobreposição (padrão: `CRS_ALBERS_BR`).
#' @return Data.frame com uma linha por encontro: `Ordem` (posição do encontro em `grupos`),
#' `Região`, `CRs de Participação Obrigatória`, `Participação Opcional`.
montar_tabela_encontros_regionais <- function(
    arq_poligonos_cr,
    arq_regioes,
    grupos = GRUPOS_ENCONTROS_REGIONAIS,
    crs_area = CRS_ALBERS_BR
) {

  cr <- sf::st_read(arq_poligonos_cr, quiet = TRUE) |>
    dplyr::select(cd_uorg, nome_uorg, sigla)
  cr <- sf::st_transform(cr, crs_area) |> sf::st_make_valid()

  grupos <- lapply(grupos, function(g) if (is.list(g)) g else list(obrigatoria = g))

  siglas_config <- unique(unlist(lapply(grupos, function(g) c(g$obrigatoria, g$opcional))))
  faltando <- setdiff(siglas_config, c(cr$sigla, CRS_SEM_AREA_PROPRIA))
  if (length(faltando) > 0) {
    stop("GRUPOS_ENCONTROS_REGIONAIS contém siglas de CR ausentes em arq_poligonos_cr: ",
         paste(faltando, collapse = ", "))
  }

  # arq_regioes (alvo `format` padrão "qs", não "file") já chega como objeto sf --
  # baixar_shapefile_zip() retorna o sf lido, não o caminho do gpkg gravado.
  regioes <- arq_regioes |>
    dplyr::select(nm_regiao)
  regioes <- sf::st_transform(regioes, crs_area) |> sf::st_make_valid()

  # Região de cada CR: maior área de sobreposição territorial com a malha de Grandes Regiões.
  inter <- sf::st_intersection(cr, regioes)
  inter$area <- as.numeric(sf::st_area(inter))
  regiao_por_cr <- inter |>
    sf::st_drop_geometry() |>
    dplyr::group_by(sigla) |>
    dplyr::slice_max(area, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(sigla, nm_regiao)
  regiao_map <- setNames(regiao_por_cr$nm_regiao, regiao_por_cr$sigla)

  # Grafo de adjacência territorial: dois CRs são vizinhos se seus polígonos compartilham
  # fronteira (sf::st_touches()) na malha final.
  vizinhos <- sf::st_touches(cr)
  lista_adjacencia <- setNames(lapply(vizinhos, function(idx) cr$sigla[idx]), cr$sigla)

  linhas <- lapply(seq_along(grupos), function(i) {
    g <- grupos[[i]]$obrigatoria
    regiao_grupo <- if (is.null(grupos[[i]]$regiao)) {
      names(sort(table(regiao_map[g]), decreasing = TRUE))[1]
    } else {
      grupos[[i]]$regiao
    }
    optativos <- if (is.null(grupos[[i]]$opcional)) {
      sort(setdiff(unique(unlist(lista_adjacencia[g])), g))
    } else {
      grupos[[i]]$opcional
    }
    data.frame(
      Ordem = as.character(i),
      Regiao = regiao_grupo,
      Obrigatoria = paste(g, collapse = ", "),
      Opcional = paste(optativos, collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  resultado <- do.call(rbind, linhas)
  names(resultado) <- c("Ordem", "Região", "CRs de Participação Obrigatória", "Participação Opcional")
  resultado
}

#' @title Mapa da jurisdição atual (declarada/oficial) por CR
#' @description Mapa coroplético categórico da jurisdição atual (vinculação institucional
#' vigente -- SII para Terra Indígena, declaração de atendimento SEI/site institucional para
#' Município -- ver `calcular_jurisdicao_atual_bruta()` em `R/geoprocessamento.R`), uma cor por
#' CR, com a sigla rotulada em cada polígono via `ggrepel` (mesmo estilo "chip" de
#' `criar_mapa_coropletico_cr()`).
#' @details Paleta qualitativa gerada por `grDevices::hcl.colors(n, "Dynamic")` (matizes
#' igualmente espaçados em HCL, sem depender do pacote `colorspace`): com ~40 CRs, nenhuma
#' paleta categórica consegue separar todos os pares de forma segura para daltonismo -- o
#' próprio conjunto de referência do skill `dataviz` (`references/palette.md`) só garante isso
#' para até 3-4 cores em cenário "all-pairs" (choropleth). A identificação principal do mapa é
#' o rótulo da sigla sobre cada polígono, não a memorização da cor. "Sem vinculação declarada"
#' (território sem CR atual) fica em cinza neutro fixo, fora da rotação qualitativa -- é uma
#' ausência de dado, não uma CR real.
#' @param arq_poligonos_cr Objeto sf ou caminho de gpkg com um polígono por CR (alvo
#'   `jurisdicao_atual_dissolvida`), colunas `cd_uorg`/`nome_uorg`/`sigla`.
#' @param arquivo Nome do arquivo de saída (sem extensão). Se `NULL`, não exporta.
#' @return Objeto ggplot.
criar_mapa_jurisdicao_atual <- function(arq_poligonos_cr, arquivo = "mapa_jurisdicao_atual") {

  cr <- if (inherits(arq_poligonos_cr, "sf")) arq_poligonos_cr else sf::st_read(arq_poligonos_cr, quiet = TRUE)
  cr <- cr |> sf::st_transform(4326) |> sf::st_make_valid()

  idx_sem_vinc <- which(cr$sigla == "Sem vinculação declarada")

  # Cor por adjacência territorial (coloração de grafo, tipo "mapa político"), não por ordem
  # arbitrária dos dados: duas CRs que fazem fronteira (inclusive uma CR "ilha" cercada por
  # outra, já que sf::st_touches() enxerga o multipolígono inteiro de cada CR, não só a peça
  # principal) nunca recebem cores parecidas -- sem isso, uma CR pequena/fragmentada podia
  # sair com uma cor perto da vizinha só por coincidência da ordem das linhas.
  # Cada CR tem uma cor PREFERIDA espalhada pela paleta (posição dela entre as CRs com
  # vinculação, não a ordem de processamento) para manter boa variedade visual no mapa inteiro
  # -- ao contrário deste mapa (com grandes vazios cinza de "sem vinculação" entre as CRs, não
  # uma malha 100% contígua como a proposta), a adjacência real é esparsa: a maioria das CRs
  # não faz fronteira com quase nenhuma outra, então uma heurística gulosa pura tipo
  # Welsh-Powell ("primeira cor livre") faz quase todo mundo colidir na cor 1, por falta de
  # conflito para forçar variação. Só desvia da cor preferida quando ela realmente colide com
  # uma vizinha (por adjacência) já colorida, processando as CRs mais conectadas primeiro.
  paleta <- grDevices::hcl.colors(14, "Dynamic")
  idx_com_vinc <- setdiff(seq_len(nrow(cr)), idx_sem_vinc)
  sub <- cr[idx_com_vinc, ]
  vizinhos <- sf::st_touches(sub)
  preferida <- ((seq_len(nrow(sub)) - 1) %% length(paleta)) + 1
  ordem_sub <- order(-lengths(vizinhos))
  cor_idx_sub <- rep(NA_integer_, nrow(sub))
  for (j in ordem_sub) {
    usadas <- cor_idx_sub[vizinhos[[j]]]
    usadas <- usadas[!is.na(usadas)]
    candidatas <- c(preferida[j], setdiff(seq_along(paleta), preferida[j]))
    escolhida <- candidatas[!candidatas %in% usadas][1]
    cor_idx_sub[j] <- if (!is.na(escolhida)) escolhida else sample(seq_along(paleta), 1)
  }
  cor_idx <- rep(NA_integer_, nrow(cr))
  cor_idx[idx_com_vinc] <- cor_idx_sub
  cores <- paleta[cor_idx]
  if (length(idx_sem_vinc) > 0) cores[idx_sem_vinc] <- "#B0B0B0"
  cores <- stats::setNames(cores, cr$sigla)

  # Simplifica só para exibição -- depois da coloração por adjacência acima, que precisa da
  # geometria original em st_touches() (ver simplificar_para_mapa()).
  cr <- simplificar_para_mapa(cr)

  centroides <- cr |>
    sf::st_point_on_surface() |>
    dplyr::mutate(rotulo = dplyr::if_else(sigla == "Sem vinculação declarada", NA_character_, gsub("^CR-", "", sigla)))

  # aes(geometry = geometry) abaixo espera a coluna com esse nome -- cr pode chegar com a
  # geometria nomeada "geom" (armadilha de nome de coluna documentada em CLAUDE.md), então
  # renomeia explicitamente (mesmo padrão de calcular_malha_basica_bruta()).
  sf_col <- attr(centroides, "sf_column")
  names(centroides)[names(centroides) == sf_col] <- "geometry"
  sf::st_geometry(centroides) <- "geometry"

  n_sem_vinc <- length(idx_sem_vinc)

  g <- ggplot2::ggplot(cr) +
    ggplot2::geom_sf(ggplot2::aes(fill = sigla), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(values = cores, guide = "none") +
    ggrepel::geom_label_repel(
      data = centroides,
      ggplot2::aes(geometry = geometry, label = rotulo),
      stat = "sf_coordinates",
      size = 1.6,
      color = "#1A1A1A",
      fontface = "bold",
      label.size = 0,
      label.r = grid::unit(0.2, "lines"),
      label.padding = grid::unit(0.12, "lines"),
      fill = scales::alpha("white", 0.85),
      seed = 42,
      max.overlaps = Inf,
      min.segment.length = 0,
      segment.size = 0.2,
      segment.color = scales::alpha("#1A1A1A", 0.5),
      box.padding = 0.15,
      point.padding = 0.05,
      force = 2,
      max.iter = 20000,
      max.time = 2
    ) +
    ggplot2::coord_sf(crs = sf::st_crs(4326), expand = FALSE) +
    ggplot2::labs(
      title = "Jurisdição Atual por Coordenação Regional",
      subtitle = paste0(
        "Vinculação institucional vigente: Terras Indígenas pelo SII; municípios pela declaração de atendimento (processo SEI/site institucional)",
        "\n(", n_sem_vinc, " CR", ifelse(n_sem_vinc == 1, "", "s"), " sem território atualmente vinculado)"
      ),
      caption = "Fonte: SII (FUNAI); declaração de atendimento por CR (processo SEI n° 08620.010286/2021-89) e site institucional da Funai"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    obter_tema(tipo = "mapa", cor = CORES_FUNAI[["principal"]]) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 10, face = "bold", color = CORES_FUNAI[["principal"]]),
      plot.subtitle = ggplot2::element_text(size = 8, color = "grey35"),
      plot.caption  = ggplot2::element_text(size = 7, color = "grey50", hjust = 1),
      plot.margin   = ggplot2::margin(6, 6, 6, 6)
    )

  if (!is.null(arquivo)) exporta_grafico(g, arquivo)
  g
}
