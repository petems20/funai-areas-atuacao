load_cr_geocode <- function(
    arq_siorg,
    arq_municipios
    ){
  
  
  # arq_siorg <- targets::tar_read(arq_siorg)
  # arq_municipios <- targets::tar_read(arq_municipios)
  
  output_dir <- DIR_PROCESSADOS

  # Geocodificação congelada (insumo da release de dados): evita baixar a base do CNEFE usada pelo
  # geocodebr e garante os mesmos pontos de origem das CRs no roteamento. É copiada para
  # dados/processados/, onde o projeto QGIS a lê.
  if (!ATUALIZAR_SIORG && file.exists(ARQ_CR_GEOCODE)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    destino <- file.path(output_dir, "cr_geocode.gpkg")
    file.copy(ARQ_CR_GEOCODE, destino, overwrite = TRUE)
    return(destino)
  }

  # --- 1. Leitura dos dados de entrada ---
  # Como os targets anteriores retornam caminhos de arquivos, precisamos lê-los:
  siorg <- arq_siorg
  sf_mun <- arq_municipios
  
  
  # --- 2. Filtrar apenas CRs e UTLs e selecionar campos relevantes ---
  siorg_cr <- siorg |>
    dplyr::filter(grepl('^CR-', trimws(sigla)) | grepl('^UTL-', trimws(sigla))) |>
    dplyr::distinct(codigoUnidade, .keep_all = TRUE) |>
    dplyr::select(codigoUnidadePai, codigoUnidade, nome, sigla, logradouro, numero, complemento, cep,
           bairro, municipio, uf)
  
  # --- 3. Acrescentar nome do município usando dados geográficos já carregados ---
  siorg_cr <- siorg_cr |>
    dplyr::left_join(
      sf_mun |>
        dplyr::mutate(cd_mun = as.integer(cd_mun)) |>
        sf::st_drop_geometry() |>
        dplyr::select(cd_mun, nm_mun, sigla_uf),
      by = dplyr::join_by(municipio == cd_mun)
    )
  
  # --- 4. Definir correspondência de campos para geocodificação ---
  campos <- geocodebr::definir_campos(
    logradouro = "logradouro",
    numero = "numero",
    cep = "cep",
    localidade = "bairro",
    municipio = "nm_mun",
    estado = "sigla_uf"
  )
  
  # --- 5. Geocodificação ---
  cr_geocode <- geocodebr::geocode(
    enderecos = siorg_cr,
    padronizar_enderecos = TRUE,
    campos_endereco = campos,
    resolver_empates = TRUE,
    resultado_sf = TRUE,
    verboso = TRUE
  )
  
  # --- 6. Salvar resultado em GeoPackage ---
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  caminho_final <- file.path(output_dir, "cr_geocode.gpkg")
  sf::st_write(cr_geocode, caminho_final, delete_dsn = TRUE, quiet = TRUE)
  return(caminho_final)
}

#' @title Constrói as linhas de subordinação (Unidade Técnica Local -> CR responsável)
#' @description Para cada Unidade Técnica Local (`codigoUnidadePai != 173`), constrói uma linha
#' ligando seu ponto geocodificado ao ponto geocodificado da CR à qual está subordinada
#' (`codigoUnidade == codigoUnidadePai`), reproduzindo a camada "Subordinação" do projeto QGIS de
#' Áreas de Atuação (antes mantida como um gpkg estático, fora deste pipeline).
#' @param arq_cr_geocode Caminho do gpkg de pontos com a geocodificação das CRs/Unidades Técnicas
#'   Locais (alvo `geocodificacao_cr`), contendo `codigoUnidade`, `codigoUnidadePai`, `nome`, `sigla`.
#' @param dir_saida Diretório de saída (padrão: `DIR_PROCESSADOS`).
#' @return Caminho do gpkg gerado (`subordinacao.gpkg`).
construir_linhas_subordinacao <- function(arq_cr_geocode, dir_saida = DIR_PROCESSADOS) {

  pontos <- sf::st_read(arq_cr_geocode, quiet = TRUE) |>
    sf::st_transform(CRS_GEOPROCESSAMENTO)

  coords <- sf::st_coordinates(pontos)
  pontos_df <- pontos |>
    sf::st_drop_geometry() |>
    dplyr::mutate(x = coords[, "X"], y = coords[, "Y"])

  filhos <- pontos_df |> dplyr::filter(codigoUnidadePai != 173)
  pais <- pontos_df |> dplyr::select(codigoUnidade, x_pai = x, y_pai = y)

  subordinacao <- filhos |>
    dplyr::left_join(pais, by = dplyr::join_by(codigoUnidadePai == codigoUnidade)) |>
    dplyr::filter(!is.na(x_pai)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      geom = list(sf::st_linestring(matrix(c(x, y, x_pai, y_pai), ncol = 2, byrow = TRUE)))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-x, -y, -x_pai, -y_pai) |>
    sf::st_as_sf(sf_column_name = "geom", crs = CRS_GEOPROCESSAMENTO)

  dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)
  caminho_saida <- file.path(dir_saida, "subordinacao.gpkg")
  sf::st_write(subordinacao, caminho_saida, delete_dsn = TRUE, quiet = TRUE)

  return(caminho_saida)
}
