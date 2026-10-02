#' @title Gera o gpkg de critérios de associação (dissolvido por critério)
#' @description Extrai a única parte de um antigo `criar_mapa_final()` que efetivamente alimentava
#' algo a jusante no pipeline: a camada "Critérios" do projeto QGIS AACR
#' (qgis/ProjetoAACR_FUNAI.qgz), fonte da Figura 3 da nota técnica ("Critérios de
#' Associação da Malha Básica por Setores Censitários"). A função antiga também construía um mapa
#' `leaflet` completo e exportava um GeoJSON -- nenhum dos dois nunca foi consumido por nenhuma
#' etapa seguinte do pipeline nem pela nota técnica, só acrescentavam tempo de processamento
#' (`htmlwidgets::saveWidget()` é a etapa mais cara). Removidos junto com `criar_mapa_uorgs()`
#' (outra função de mapa `leaflet` já sem nenhum chamador).
#' @param arq_sc_final Caminho do gpkg de setores censitários com a coluna `criterio`.
#' @param caminho_saida Caminho do gpkg de saída.
#' @return Caminho do gpkg gerado.
criar_gpkg_criterios <- function(arq_sc_final, caminho_saida) {

  sf_criterio <- sf::st_read(arq_sc_final, quiet = TRUE) |>
    dplyr::select("criterio")

  sf_criterio <- aggregate(
    x = dplyr::select(sf_criterio),
    by = list(criterio = sf_criterio$criterio),
    FUN = head
  )

  if (!dir.exists(dirname(caminho_saida))) {
    dir.create(dirname(caminho_saida), recursive = TRUE)
  }
  sf::st_write(sf_criterio, caminho_saida, delete_dsn = TRUE, quiet = TRUE)

  caminho_saida
}

#' @title Exporta layouts de impressão do projeto QGIS de Áreas de Atuação (AACR) para PNG/PDF
#' @description Abre o projeto QGIS em modo headless (via `python-qgis.bat`, ambiente PyQGIS do
#' OSGeo4W) e exporta os layouts de impressão pedidos (por nome) para PNG e PDF em `dir_saida`.
#' Layouts com mais de uma página geram múltiplos PNG (`<nome>.png`, `<nome>_2.png`, ...) mas um
#' único PDF. A função apenas lê o projeto (`QgsProject.read`, ver `R/exportar_layouts_qgis.py`)
#' -- nunca o altera nem o salva, então os layouts em si nunca são modificados por esta função.
#' O `.qgz` (`arq_projeto_qgis`) é um arquivo único versionado no repositório: estilos, layouts e
#' caminhos de camada relativos aos `.gpkg` gerados pelo pipeline foram configurados uma única vez,
#' manualmente, na interface do QGIS -- não há mais cópia/reconexão automatizada.
#' @param arq_projeto_qgis Caminho do arquivo .qgz do projeto QGIS (ver `ARQ_PROJETO_QGIS_AACR`
#'   em `R/config.R`).
#' @param layouts Vetor com os nomes exatos dos layouts de impressão a exportar (ex.:
#'   `c("Demonstrativo Áreas de Atuação Proposta Município", "Critérios")`).
#' @param dir_saida Diretório onde os PNG/PDF serão salvos (ver `DIR_FIGURAS` em `R/config.R`).
#' @param python_qgis_bat Caminho do executável `python-qgis.bat` do ambiente headless do QGIS
#'   (ver `PYTHON_QGIS_BAT` em `R/config.R`).
#' @param ... Alvos dos `.gpkg` referenciados (por caminho relativo) pelas camadas do `.qgz`, não
#'   usados no corpo da função -- servem só para estabelecer a dependência no grafo do `{targets}`,
#'   já que o `.qgz` em si não muda de hash quando esses arquivos são regenerados (ver chamada do
#'   alvo `layouts_qgis_aacr` em `_targets.R`, parâmetros `dep_*`).
#' @return Vetor de caminhos dos arquivos PNG/PDF exportados.
exportar_layouts_qgis <- function(arq_projeto_qgis, layouts, dir_saida, python_qgis_bat, ...) {

  if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

  script_py <- file.path("R", "exportar_layouts_qgis.py")

  resultado <- system2(
    python_qgis_bat,
    args = shQuote(c(script_py, arq_projeto_qgis, dir_saida, layouts)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(resultado, "status")

  message(paste(resultado, collapse = "\n"))

  if (!is.null(status) && status != 0) {
    stop("Falha ao exportar layouts do projeto QGIS (ver mensagens acima).")
  }

  arquivos <- sub("^ARQUIVO_EXPORTADO: ", "", grep("^ARQUIVO_EXPORTADO: ", resultado, value = TRUE))

  if (length(arquivos) == 0) {
    stop("exportar_layouts_qgis(): nenhum arquivo exportado (ver mensagens acima).")
  }

  arquivos
}

# copiar_e_reconectar_projeto_qgis() e editar_layouts_qgis() foram removidas: o projeto QGIS AACR
# (qgis/ProjetoAACR_FUNAI.qgz) passou a ser um arquivo único versionado no repositório, com
# estilos, layouts e caminhos de camada relativos aos .gpkg gerados por este pipeline já
# configurados manualmente na interface do QGIS. Reconexão/edição de layout via script deixaram de
# ser necessárias -- exportar_layouts_qgis() (acima) continua sendo a única etapa automatizada,
# pois só lê o .qgz para gerar as figuras do relatório.