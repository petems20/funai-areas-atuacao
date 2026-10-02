library(sf)
library(sfarrow)

# --- Função segura para baixar WFS ---
baixar_wfs <- function(url, nome, crs_alvo = 4326) {
  # 1. Garante que a pasta existe
  dir.create(DIR_BRUTOS, recursive = TRUE, showWarnings = FALSE)

  # 2. Constrói o caminho final do ficheiro
  nome_arquivo <- paste0("arquivos_", nome, ".gpkg")
  caminho <- file.path(DIR_BRUTOS, nome_arquivo)

  # 3. Lógica de Cache (Download vs Leitura Local)
  if (file.exists(caminho)) {
    # Já existe em cache: lê do disco e evita o fetch de rede
    dados <- sf::st_read(caminho, quiet = TRUE)
    return(dados)
  }

  dados <- sf::st_read(url, quiet = TRUE)
  # CORREÇÃO GEOMÉTRICA
  dados <- sf::st_make_valid(dados)

  if (is.na(sf::st_crs(dados)$epsg) || sf::st_crs(dados)$epsg != crs_alvo) {
    dados <- sf::st_transform(dados, crs = crs_alvo)
  }

  # Salva em cache para as próximas execuções
  sf::st_write(dados, caminho, delete_dsn = TRUE, quiet = TRUE)

  # 4. Retorna o objeto sf carregado na memória para o targets guardar como "qs"
  return(dados)
}

baixar_hidrovias_en <- function(
    url = 'https://www.gov.br/antaq/pt-br/central-de-conteudos/SHP_VEN2022completo.zip'){
  
  dest_dir = DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  temp <- tempfile(fileext = ".zip")
  download.file(url, temp, mode = "wb")
  
  dest <- tempfile()
  dir.create(dest)
  
  unzip(temp, exdir = dest)
  
  sf <- st_read(
    file.path(dest, "SHP_VEN-2022-completo", "VEN-2022-completo.shp"),
    quiet = TRUE
  )
  
  unlink(temp)
  
  colnames(sf) <- tolower(names(sf))
  
  # salvar CSV
  output_path <- file.path(dest_dir, 'hidrovias_antaq.gpkg')
  
  # Salvar tudo em um único arquivo GeoPackage
  sf::st_write(sf, output_path, delete_dsn = TRUE, quiet = TRUE)
  return(sf)
  
}

#' @title Baixa e processa dados de portos da ANTAQ
#' @description Faz o download do ZIP, extrai usando ferramenta à prova de falhas de encoding 
#' (pacote zip ou OS nativo), lê o shapefile, padroniza as colunas, reprojeta e salva.
#' @param url URL do arquivo ZIP da ANTAQ.
#' @param dest_dir Diretório de destino para salvar o GPKG.
#' @return Objeto sf com os dados processados.
baixar_portos_en <- function(
    url = "https://www.gov.br/antaq/pt-br/central-de-conteudos/Instalaesporturias06052025.zip"
) {
  
  dest_dir = DIR_BRUTOS

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. Download do arquivo ZIP para área temporária
  temp_zip <- tempfile(fileext = '.zip')
  download.file(url, temp_zip, mode = "wb", method = "libcurl", quiet = TRUE)
  
  # 1.1 Trava de segurança
  if (!file.exists(temp_zip) || file.info(temp_zip)$size < 1000) {
    unlink(temp_zip)
    rlang::abort("Falha no download da ANTAQ. O arquivo retornado está vazio ou corrompido.")
  }
  
  # 2. Criar uma pasta temporária exclusiva para a extração
  tmp_exdir <- tempfile()
  dir.create(tmp_exdir)
  
  # 3. EXTRAÇÃO BLINDADA (Corrige o bug 'string multibyte inválida' do Windows)
  # Tenta usar o pacote {zip} que lida perfeitamente com UTF-8/Acentos
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::unzip(temp_zip, exdir = tmp_exdir)
  } else {
    # Plano B: Usa o extrator do próprio sistema operacional (Windows nativo não engasga com acentos)
    if (.Platform$OS.type == "windows") {
      system2("tar", args = c("-xf", shQuote(temp_zip), "-C", shQuote(tmp_exdir)), stdout = FALSE, stderr = FALSE)
    } else {
      utils::unzip(temp_zip, exdir = tmp_exdir)
    }
  }
  
  # 4. Encontrar o caminho absoluto do .shp 
  shp_interno <- list.files(
    tmp_exdir,
    pattern = "\\.shp$",
    recursive = TRUE,
    full.names = TRUE
  )[1]
  
  if (is.na(shp_interno) || length(shp_interno) == 0) {
    unlink(temp_zip)
    unlink(tmp_exdir, recursive = TRUE)
    rlang::abort("Nenhum arquivo .shp encontrado dentro do ZIP extraído.")
  }
  
  # 5. Ler o shapefile
  sf_obj <- sf::st_read(shp_interno, quiet = TRUE, options = "ENCODING=WINDOWS-1252")
  
  
  names(sf_obj) <- tolower(names(sf_obj))
  
  out <- file.path(dest_dir, "portos_antaq.gpkg")
  sf::st_write(sf_obj, out, delete_dsn = TRUE, quiet = TRUE)
  
  # 9. Limpeza final
  unlink(temp_zip)
  unlink(tmp_exdir, recursive = TRUE)
  
  return(sf_obj)
}

#' @title Importa a estrutura organizacional da Funai (SIORG/indigenR)
#' @description Usa `indigenR::funai_estrutura_completa()` -- wrapper específico para a Funai
#' (código de unidade 173 fixo) de `indigenR::siorg_estrutura_completa()` -- em vez de passar o
#' código manualmente. Mantém o mesmo padrão de cache-then-fetch: se já existe cache local, lê
#' dele; senão chama a API e grava um novo cache.
#' @return tibble com uma linha por unidade organizacional (codigoUnidade, sigla, nome, endereço, ...).
baixar_siorg <- function() {

  caminho_arquivo <- ARQ_SIORG
  output_dir <- dirname(caminho_arquivo)

  # 1. Lógica de Cache: se já existe em disco, lê e evita a chamada à API
  if (file.exists(caminho_arquivo)) {
    siorg <- readr::read_csv(caminho_arquivo, show_col_types = FALSE)
  } else {
    # 2. Coleta os dados da API (indigenR)
    siorg <- funai_estrutura_completa()

    # 3. Garante que o diretório existe
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    # 4. O "WRITE": Salva em CSV
    # Usamos readr::write_csv por ser mais rápido e lidar melhor com caracteres especiais
    readr::write_csv(siorg, caminho_arquivo)
  }

  # `nome`/`sigla` às vezes vêm do SIORG com espaço em branco no início/fim (ex.: " CR-ALSE",
  # inconsistência de cadastro na fonte, não deste pipeline) -- filtros e joins por igualdade
  # exata de sigla adiante (ex.: sf_pontos_filtrados, calcular_malha_basica_bruta()) quebram
  # silenciosamente com isso. Elimina na fonte, tanto para o CSV recém-baixado quanto para
  # qualquer cache já gravado em disco antes desta correção.
  siorg |>
    dplyr::mutate(
      nome = trimws(nome),
      sigla = trimws(sigla)
    )
}

#' @title Importa o relatório dinâmico de estrutura organizacional da Funai (SIORG/indigenR)
#' @description Substitui a exportação manual do "Relatório Dinâmico - Estrutura Viva" do SIORG
#' (CSV baixado à mão pela interface web e salvo em `dados/brutos/relatorio-dinamico-estrutura-viva.csv`)
#' pela importação direta via `indigenR::funai_relatorio_estrutura()`. A função do pacote já
#' geocodifica os endereços das unidades e enriquece os cargos com o catálogo de valores do
#' CCE/FCE (`indigenR::siorg_catalogo_cargos_comissionados()`), mas essa geometria/enriquecimento
#' não é usado por este pipeline (que já tem sua própria geocodificação em `load_cr_geocode()` e
#' seu próprio catálogo de cargos em `arq_cargos`) -- por isso a geometria é descartada aqui,
#' mantendo o formato tabular plano equivalente ao CSV manual que esta função substitui. Usa o
#' mesmo padrão de cache-then-fetch de `baixar_siorg()`.
#' @return tibble com uma linha por (unidade, cargo), incluindo `Código Unidade`, `Tipo do Cargo`,
#'   `Categoria`, `Nível` e `Quantidade` -- mesmas colunas usadas pelo CSV manual que substitui.
importar_siorg_estrutura <- function() {

  caminho_arquivo <- ARQ_SIORG_ESTRUTURA
  output_dir <- dirname(caminho_arquivo)

  if (file.exists(caminho_arquivo)) {
    return(readr::read_csv(caminho_arquivo, show_col_types = FALSE))
  }

  dados <- funai_relatorio_estrutura() |> sf::st_drop_geometry()

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(dados, caminho_arquivo)

  dados
}

#' @title Atualiza nome/sigla do dicionário de CRs com o SIORG vigente
#' @description O dicionário estático de CRs (`dados/imputacoes/dicionario_cr.rds`, ver
#' `ARQ_DICIONARIO_CR`) traz identificadores estáveis (`codigo_siape`, `codigo_siorg`,
#' `codigo_ug_siafi`) que não mudam, mas seu `nome`/`sigla` podem ficar desatualizados após
#' reestruturações (ex.: CR-NE-I/CR-NE-II passaram a se chamar CR-ALSE/CR-CEPIRN). Substitui
#' `nome`/`sigla` pelos valores vigentes no SIORG para a unidade correspondente a
#' `codigo_siorg`, mantendo os valores estáticos como fallback para códigos sem correspondência
#' na consulta atual (ex.: unidades desativadas). Não sobrescreve o `.rds` em disco -- apenas o
#' valor do alvo `dicionario_cr` reflete o SIORG vigente a cada execução do pipeline.
#' @param dicionario_cr_estatico Data.frame lido de `dados/imputacoes/dicionario_cr.rds`.
#' @param arq_siorg Data.frame do SIORG (alvo `arq_siorg`), contendo `codigoUnidade`, `nome`, `sigla`.
#' @return `dicionario_cr_estatico` com `nome`/`sigla` atualizados a partir do SIORG.
atualizar_nomes_dicionario_cr <- function(dicionario_cr_estatico, arq_siorg) {

  siorg_nomes <- arq_siorg |>
    dplyr::transmute(
      codigo_siorg = as.character(codigoUnidade),
      nome_siorg = nome,
      sigla_siorg = trimws(sigla)
    ) |>
    dplyr::distinct()

  dicionario_cr_estatico |>
    dplyr::mutate(.codigo_siorg_chr = as.character(codigo_siorg)) |>
    dplyr::left_join(siorg_nomes, by = c(".codigo_siorg_chr" = "codigo_siorg")) |>
    dplyr::mutate(
      nome = dplyr::coalesce(nome_siorg, nome),
      sigla = dplyr::coalesce(sigla_siorg, sigla)
    ) |>
    dplyr::select(-.codigo_siorg_chr, -nome_siorg, -sigla_siorg)
}

#' @title Confere os insumos congelados contra o manifesto
#' @description Os insumos que mudam com o tempo (geoserver da Funai e do IBGE, SIORG, CadÚnico,
#'   SIOP, SIASI, BDQueimadas, geocodificação das CRs e respostas do OSRM) são distribuídos na
#'   release de dados do repositório e listados em `ARQ_MANIFESTO_INSUMOS` com seu SHA-256. Sem eles,
#'   as funções de importação baixariam versões atuais das fontes e o resultado deixaria de
#'   reproduzir o publicado sem nenhum aviso; por isso o pipeline para se algum faltar ou divergir.
#'   Defina `JURISDICOES_INSUMOS=livre` no `.Renviron` para pular a verificação e baixar das fontes.
#' @param manifesto Caminho do manifesto (CSV com `arquivo`, `bytes`, `sha256`, `uso`).
#' @param osrm_modo Modo do OSRM (`OSRM_MODO`); em `"api"` as respostas congeladas não são exigidas.
#' @param atualizar_siorg Se `TRUE` (`JURISDICOES_ATUALIZAR=siorg`), SIORG e geocodificação não são exigidos.
#' @return `TRUE`, invisível, se todos os insumos existem e conferem.
verificar_insumos_congelados <- function(manifesto = ARQ_MANIFESTO_INSUMOS, osrm_modo = OSRM_MODO,
                                         atualizar_siorg = ATUALIZAR_SIORG) {
  if (identical(Sys.getenv("JURISDICOES_INSUMOS"), "livre")) return(invisible(TRUE))

  insumos <- utils::read.csv(manifesto, colClasses = "character", encoding = "UTF-8")
  # Insumos que o modo de atualização (JURISDICOES_ATUALIZAR) refaz não são exigidos
  if (identical(osrm_modo, "api")) insumos <- insumos[insumos$uso != "osrm", ]
  if (isTRUE(atualizar_siorg)) insumos <- insumos[insumos$uso != "siorg", ]

  ausentes <- insumos$arquivo[!file.exists(insumos$arquivo)]
  if (length(ausentes) > 0) {
    stop(
      "Faltam ", length(ausentes), " insumos congelados em dados/brutos/ (", paste(utils::head(ausentes, 5), collapse = ", "),
      if (length(ausentes) > 5) ", ..." else "", "). Baixe a release de dados do repositório e ",
      "descompacte-a na raiz do projeto (ver README), ou defina JURISDICOES_INSUMOS=livre no .Renviron ",
      "para baixar as versões atuais das fontes (o resultado pode diferir do publicado).",
      call. = FALSE
    )
  }

  hashes <- vapply(insumos$arquivo, function(a) digest::digest(file = a, algo = "sha256"), character(1))
  divergentes <- insumos$arquivo[hashes != insumos$sha256]
  if (length(divergentes) > 0) {
    stop(
      "Insumos com conteúdo diferente do manifesto (", ARQ_MANIFESTO_INSUMOS, "): ",
      paste(divergentes, collapse = ", "), ". Restaure-os a partir da release de dados.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @title Baixa a tabela de valores dos cargos comissionados (CCE e FCE) da API do SIORG
#' @description Consulta a API pública do SIORG para as siglas CCE e FCE e devolve uma linha por
#'   cargo, com o rótulo no mesmo formato usado na estrutura organizacional ("CCE 1 05": sigla,
#'   categoria e nível com dois dígitos), o valor da remuneração e o valor em unidades de CCE. Usa o
#'   padrão cache-then-fetch: se `arquivo` já existe, lê dele; senão consulta a API e o grava.
#' @param arquivo Caminho do cache em CSV (padrão: `ARQ_CARGOS`).
#' @param max_tentativas Número máximo de tentativas por sigla antes de abortar.
#' @return data.table com `cargo`, `valor` e `cce_unitario`.
baixar_cargos_siorg <- function(arquivo = ARQ_CARGOS, max_tentativas = 5) {
  if (file.exists(arquivo)) {
    return(data.table::fread(arquivo, sep = ";", dec = ","))
  }

  consultar <- function(sigla) {
    url <- paste0(URL_SIORG_CARGOS, sigla)
    tryCatch(
      httr2::request(url) |>
        httr2::req_retry(max_tries = max_tentativas, backoff = function(i) 5 * i) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE),
      error = function(e) {
        stop("Falha ao consultar os cargos ", sigla, " na API do SIORG (", url, "): ", conditionMessage(e))
      }
    )
  }

  cargos <- lapply(c("CCE", "FCE"), function(sigla) {
    resposta <- consultar(sigla)
    message("SIORG: ", nrow(resposta), " cargos ", sigla, " recebidos")
    data.table::data.table(
      cargo = paste(resposta$sigla, resposta$categoria, sprintf("%02d", as.integer(resposta$nivel))),
      valor = resposta$valor,
      cce_unitario = resposta$cceUnitario
    )
  }) |>
    data.table::rbindlist()

  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(cargos, arquivo, sep = ";", dec = ",")
  cargos
}

#' @title Importa pessoas e famílias indígenas no Cadastro Único por município (VisData/indigenR)
#' @description Substitui a exportação manual do VisData (CSV baixado à mão, nome do arquivo
#' atualizado manualmente em `ARQ_CADU_NOME`) pela importação direta via
#' `indigenR::visdata_importa_racor_indigena()`, que já traz pessoas e famílias (o CSV manual só
#' tinha pessoas). Usa o mesmo padrão de cache-then-fetch de `baixar_siorg()`: se já existe um
#' cache local, lê dele; senão chama a API e grava um novo cache.
#' @param forcar_atualizacao Lógico. Se `TRUE`, ignora o cache local e busca dados novos na API.
#' @return tibble com `codigo_ibge` (código IBGE do município, 6 dígitos com ponto decorativo, ex.
#'   `"120.001"`), `municipio`, `uf`, `referencia` (período, ex. `"08/2026"`),
#'   `n_fam_indigena_cad`, `n_pess_indigena_cad`.
baixar_cadunico_racor <- function(forcar_atualizacao = FALSE) {

  caminho_arquivo <- file.path(DIR_BRUTOS, "cadunico_visdata_racor.csv")

  if (file.exists(caminho_arquivo) && !forcar_atualizacao) {
    # codigo_ibge vem da API como texto com ponto decorativo ("120.001"); sem fixar o tipo, o
    # read_csv o converteria em número (120.001) e a releitura divergiria da primeira execução.
    return(readr::read_csv(
      caminho_arquivo,
      col_types = readr::cols(codigo_ibge = readr::col_character()),
      show_col_types = FALSE
    ))
  }

  dados <- indigenR::visdata_importa_racor_indigena(max_tries = 5, verbose = TRUE)

  dir.create(DIR_BRUTOS, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(dados, caminho_arquivo)

  dados
}

# Download e união agregados
baixar_e_unir_agregados_censo <- function(
    url_pessoas,
    url_domicilios,
    join
) {
  
  # url_pessoas = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Municipio_csv/Agregados_por_municipios_pessoas_indigenas_BR.zip"
  # url_domicilios = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Municipio_csv/Agregados_por_municipios_domicilios_indigenas_BR.zip"
  # join = 'cd_mun'
  
  pessoas <- download_agregados_censo(
    url = url_pessoas
  )
  
  domicilios <- download_agregados_censo(
    url = url_domicilios
  )
  
  # Join via data.table (in-place, sem cópias intermediárias): as tabelas
  # tem ~1000 colunas cada (todas as variáveis v0* do Censo são usadas a
  # jusante em unir_poligonos()/tabela_dados_final), então dplyr::left_join
  # duplicava a tabela inteira na memória em cada etapa do pipe.
  nm_var <- gsub('cd_', 'nm_', join)
  if (nm_var %in% names(domicilios)) {
    domicilios[, (nm_var) := NULL]
  }

  resultado <- merge(pessoas, domicilios, by = join, all.x = TRUE, sort = FALSE)
  rm(pessoas, domicilios)
  gc()

  resultado
  
}


download_malha_municipal <- function(
    url = "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2025/Brasil/BR_Municipios_2025.zip"
) {
  
  dest_dir = DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  zip_path <- file.path(dest_dir, basename(url))
  unzip_dir <- file.path(dest_dir, "tmp_malha_mun")
  
  tryCatch({
    # download
    download.file(url, zip_path, mode = "wb")
    
    # unzip
    unzip(zip_path, exdir = unzip_dir)
    
    # localizar arquivo específico
    target_file <- list.files(
      unzip_dir,
      pattern = "BR_Municipios_[0-9]{4}\\.shp$",
      full.names = TRUE,
      recursive = TRUE
    )
    
    
    if (length(target_file) == 0) {
      stop("Arquivo de municípios não encontrado.")
    }
    
    # Ler arquivo
    municipios <- st_read(target_file)
    colnames(municipios) <- tolower(names(municipios))
    
    # salvar CSV
    output_path <- file.path(dest_dir, 'municipios.gpkg')
    # Salvar tudo em um único arquivo GeoPackage
    sf::st_write(municipios, output_path, delete_dsn = TRUE, quiet = TRUE)
  },
  finally = {
    # limpeza (remove zip e pasta extraída)
    unlink(zip_path)
    unlink(unzip_dir, recursive = TRUE)
  }
  )
  
  #return(output_path)
  return(municipios)
}

baixar_shapefile_zip <- function(url, 
                                 arquivo_saida, 
                                 padrao_busca = "\\.shp$", 
                                 dest_dir = DIR_BRUTOS) {
  
  # 1. Garante que o diretório de destino existe
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 2. Define caminhos dinâmicos baseados nos argumentos
  zip_path <- file.path(dest_dir, basename(url))
  
  # Cria um nome de pasta temporária única baseada no nome do arquivo de saída
  nome_base <- tools::file_path_sans_ext(arquivo_saida)
  unzip_dir <- file.path(dest_dir, paste0("tmp_", nome_base))
  
  output_path <- file.path(dest_dir, arquivo_saida)
  
  # 3. Executa o processo com tratamento de erros (tryCatch)
  tryCatch({
    # Download do arquivo
    download.file(url, zip_path, mode = "wb", quiet = TRUE)
    
    # Extração
    unzip(zip_path, exdir = unzip_dir)
    
    # Localizar o arquivo .shp específico usando o padrão de busca
    target_file <- list.files(
      unzip_dir,
      pattern = padrao_busca,
      full.names = TRUE,
      recursive = TRUE
    )
    
    # Verifica se o arquivo foi encontrado
    if (length(target_file) == 0) {
      stop(sprintf("Arquivo com o padrão '%s' não encontrado no zip.", padrao_busca))
    }
    
    # 4. Leitura e Padronização
    # st_read lê o arquivo espacial. Usamos [1] caso a busca retorne mais de um arquivo.
    dados <- sf::st_read(target_file[1], quiet = TRUE)
    
    # Padroniza os nomes das colunas para minúsculas
    colnames(dados) <- tolower(names(dados))
    
    # 5. Salvar como GeoPackage
    sf::st_write(dados, output_path, delete_dsn = TRUE, quiet = TRUE)
    
  },
  finally = {
    # 6. Limpeza: Sempre executado, mesmo se der erro no meio do processo
    if (file.exists(zip_path)) unlink(zip_path)
    if (dir.exists(unzip_dir)) unlink(unzip_dir, recursive = TRUE)
  })
  
  # Retorna o caminho para o pacote targets rastrear
  #return(output_path)
  return(dados)
}



download_process_dtb <- function(
    url = "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/2025/DTB_2025.zip"
) {
  dest_dir = DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(dest_dir, basename(url))
  unzip_dir <- file.path(dest_dir, "tmp_dtb")
  
  # download
  download.file(url, zip_path, mode = "wb")
  
  # unzip
  unzip(zip_path, exdir = unzip_dir)
  
  # localizar arquivo específico
  target_file <- list.files(
    unzip_dir,
    pattern = "RELATORIO_DTB_BRASIL_2025_MUNICIPIOS\\.xls$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(target_file) == 0) {
    stop("Arquivo de municípios não encontrado.")
  }
  
  # ler pulando 6 linhas
  df <- readxl::read_excel(target_file, skip = 6)

  if (ncol(df) != 9) {
    stop(sprintf(
      "Esperado 9 colunas em '%s', mas foram encontradas %d. Verifique se o layout da planilha mudou.",
      basename(target_file), ncol(df)
    ))
  }

  colnames(df) <- c('uf', 'nm_uf', 'cd_rgint', 'nm_rgint', 'cd_rgi', 'nm_rgi', 'id_mun', 'cd_mun', 'nm_mun')
  
  # salvar CSV
  output_csv <- file.path(dest_dir, "dtb_municipios_2025.csv")
  readr::write_csv(df, output_csv)
  
  # limpeza (remove zip e pasta extraída)
  unlink(zip_path)
  unlink(unzip_dir, recursive = TRUE)
  
  #return(output_csv)
  return(df)
}


baixar_geopackage_norte <- function(url) {
  dest_dir <- DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(dest_dir, basename(url))
  unzip_dir <- file.path(dest_dir, "tmp_osrm")
  
  # Download seguro
  httr::GET(
    url, 
    httr::user_agent("MeuProjetoR_AnaliseEspacial/1.0"),
    httr::write_disk(zip_path, overwrite = TRUE),
    httr::config(ssl_verifypeer = FALSE)
  )
  
  unzip(zip_path, exdir = unzip_dir)
  
  # Encontra o .gpkg extraído
  gpkg_temp <- list.files(unzip_dir, pattern = "\\.gpkg$", full.names = TRUE)[1]
  
  # Move o arquivo para a pasta principal e dá um nome definitivo
  gpkg_final <- file.path(dest_dir, "norte_geofabrik.gpkg")
  file.rename(gpkg_temp, gpkg_final)
  
  # Limpeza do ZIP e da pasta temporária
  unlink(zip_path)
  unlink(unzip_dir, recursive = TRUE)
  
  # IMPORTANTE: A função retorna o caminho como um caractere
  return(gpkg_final)
}



download_localidades_indigenas <- function(
    url = "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/localidades/localidades_indigenas_2022/Arquivos_vetoriais/LI/shp/BR/BR_LIs_CD2022_20250919.zip"
) {
  dest_dir = DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(dest_dir, basename(url))
  unzip_dir <- file.path(dest_dir, "tmp_localidades")
  
  tryCatch({
    # download
    download.file(url, zip_path, mode = "wb")

    # unzip
    unzip(zip_path, exdir = unzip_dir)
    
    # localizar arquivo específico
    target_file <- list.files(
      unzip_dir,
      pattern = ".shp$",
      full.names = TRUE,
      recursive = TRUE
    )
    
    if (length(target_file) == 0) {
      stop("Arquivo de localidades indígenas não encontrado.")
    }
    
    # lendo arquivo
    df <- sf::st_read(target_file, quiet = TRUE)
    colnames(df) <- tolower(names(df))
  
    # salvar shapefile
    output_sf <- file.path(dest_dir, paste0(tools::file_path_sans_ext(basename(url)),'.gpkg'))
    sf::st_write(df, output_sf, delete_dsn = TRUE, quiet = TRUE)
    
    # return(output_sf)
    return(df)
  }, finally = {
    unlink(zip_path)
    unlink(unzip_dir, recursive = TRUE)
  })
  
}

download_regic <- function(
    url = "https://geoftp.ibge.gov.br/organizacao_do_territorio/divisao_regional/regioes_de_influencia_das_cidades/Regioes_de_influencia_das_cidades_2018_Resultados_definitivos/base_vetorial/REGIC2018_Rotas2021.zip"
) {
  dest_dir = DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  zip_path <- file.path(dest_dir, basename(url))
  unzip_dir <- file.path(dest_dir, "tmp_regic")

  # Arquivo pesado (~700 MB): o timeout padrão de download.file() (options("timeout"),
  # 60s) estoura antes da conclusão em conexões comuns.
  timeout_original <- getOption("timeout")
  options(timeout = max(timeout_original, 1200))
  on.exit(options(timeout = timeout_original), add = TRUE)

  tryCatch({
    # download
    download.file(url, zip_path, mode = "wb")
    
    # unzip
    unzip(zip_path, exdir = unzip_dir)
    
    # localizar arquivo específico
    target_file <- list.files(
      unzip_dir,
      pattern = ".shp$",
      full.names = TRUE,
      recursive = TRUE
    )
    
    if (length(target_file) == 0) {
      stop("Arquivo do REGIC não encontrado.")
    }
    
    # lendo arquivo
    df <- sf::st_read(target_file, quiet = TRUE)
    colnames(df) <- tolower(names(df))

    # salvar shapefile
    output_sf <- file.path(dest_dir, paste0(tools::file_path_sans_ext(basename(url)),'.gpkg'))
    sf::st_write(df, output_sf, delete_dsn = TRUE, quiet = TRUE)

    #return(output_sf)
    return(df)
  }, finally = {
    unlink(zip_path)
    unlink(unzip_dir, recursive = TRUE)
  })

}

#' @title Série histórica de população indígena em Terras Indígenas (SIASI)
#' @description Lê os extratos anuais (um arquivo .xlsx por ano) do Sistema de Informação da
#' Atenção à Saúde Indígena (SIASI), em `dir_siasi`, e soma a população indígena por ano.
#' Cada arquivo já se refere exclusivamente a aldeias dentro de Terras Indígenas (colunas
#' `TERRA INDÍGENA`/`ALDEIA` sempre preenchidas), então o total anual reflete a população
#' indígena em TIs segundo o SIASI, não a população total do país.
#' @param dir_siasi Diretório com os extratos anuais do SIASI (ver `DIR_SIASI` em `R/config.R`).
#' @return Tibble com colunas `ano` e `populacao_indigena`, uma linha por ano.
importar_siasi_serie_historica <- function(dir_siasi = DIR_SIASI) {

  arquivos <- list.files(dir_siasi, pattern = "\\.xlsx$", full.names = TRUE)

  if (length(arquivos) == 0) {
    stop("Nenhum extrato do SIASI encontrado em ", dir_siasi)
  }

  serie <- purrr::map_dfr(arquivos, function(arquivo) {
    readxl::read_excel(arquivo, col_types = "text") |>
      dplyr::transmute(
        ano = as.integer(ANO),
        populacao_indigena = as.numeric(`POPULAÇÃO INDÍGENA`)
      )
  })

  serie |>
    dplyr::group_by(ano) |>
    dplyr::summarise(populacao_indigena = sum(populacao_indigena, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(ano)
}

download_agregados_censo <- function(
    url = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/Agregados_por_setores_pessoas_indigenas_BR.zip",
    pattern_csv = NULL
) {
  
  dest_dir <- DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(dest_dir, basename(url))
  unzip_dir <- tempfile()
  dir.create(unzip_dir)

  download.file(url, zip_path, mode = "wb")
  unzip(zip_path, exdir = unzip_dir)
  
  arquivos <- list.files(
    unzip_dir,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(arquivos) == 0) {
    stop("Nenhum arquivo CSV encontrado no ZIP.")
  }
  
  if (!is.null(pattern_csv)) {
    
    arquivos <- arquivos[
      grepl(pattern_csv, basename(arquivos), ignore.case = TRUE)
    ]
    
    if (length(arquivos) == 0) {
      stop(
        "Nenhum arquivo corresponde ao padrão informado: ",
        pattern_csv
      )
    }
  }
  
  if (length(arquivos) > 1) {
    
    stop(
      paste0(
        "Foram encontrados múltiplos arquivos CSV.\n",
        "Informe 'pattern_csv' para selecionar um deles.\n\n",
        "Arquivos encontrados:\n",
        paste0(" - ", basename(arquivos), collapse = "\n")
      )
    )
  }
  
  target_file <- arquivos[[1]]
  
  message("Lendo arquivo: ", basename(target_file))
  
  df <- data.table::fread(
    target_file,
    sep = ";",
    encoding = "Latin-1"
  )
  
  names(df) <- tolower(names(df))
  
  unlink(zip_path)
  unlink(unzip_dir, recursive = TRUE)
  
  df
}

download_censo_agregado_mun_pi <- function(
    url = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Municipio_csv/Agregados_por_municipios_pessoas_indigenas_BR.zip"
) {
  
  dest_dir = DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(dest_dir, basename(url))
  unzip_dir <- tempfile()
  dir.create(unzip_dir)

  # download
  download.file(url, zip_path, mode = "wb")
  
  # unzip
  unzip(zip_path, exdir = unzip_dir)
  
  # localizar arquivo específico
  target_file <- list.files(
    unzip_dir,
    pattern = "Agregados_por_municipios_pessoas_indigenas_BR.csv$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(target_file) == 0) {
    stop("Arquivo de dados agregados por setor censitário não encontrado.")
  }
  
  # ler pulando 6 linhas
  df <- data.table::fread(target_file, sep = ';', encoding = 'Latin-1')
  
  colnames(df) <- tolower(names(df))
  
  
  # salvar CSV
  output_csv <- file.path(dest_dir, "agg_sc_mun_pi_censo22.csv")
  readr::write_csv(df, output_csv)
  
  # limpeza (remove zip e pasta extraída)
  unlink(zip_path)
  unlink(unzip_dir, recursive = TRUE)
  
  #return(output_csv)
  return(df)
}

download_censo_agregado_sc_pi <- function(
    url = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/malha_com_atributos/setores_Indigenas/GPKG/BR_indigenas_setores_CD2022.gpkg"
) {
  
  dest_dir = DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(dest_dir, basename(url))
  
  # download
  df <- st_read(url, quiet = TRUE)
  
  colnames(df) <- tolower(names(df))
  
  # correções
  df <- sf::st_make_valid(df)
  df <- sf::st_transform(df, 4326)

  # salvar shapefile
  output_sf <- file.path(dest_dir, basename(url))
  sf::st_write(df, output_sf, delete_dsn = TRUE, quiet = TRUE)
  
  #return(output_sf)
  return(df)
}

download_malha_distritos <- function(
    url = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/malha_com_atributos/distritos/gpkg/BR/BR_distritos_CD2022.gpkg"
) {
  
  dest_dir = DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(dest_dir, basename(url))
  
  # download
  df <- st_read(url, quiet = TRUE)
  
  colnames(df) <- tolower(names(df))
  
  # salvar shapefile
  output_sf <- file.path(dest_dir, basename(url))
  sf::st_write(df, output_sf, delete_dsn = TRUE, quiet = TRUE)
  
  #return(output_sf)
  return(df)
}

download_agregados_distritos_pi <- function(
    url = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Distrito_csv/Agregados_por_distritos_pessoas_indigenas_BR.zip"
) {
  
  dest_dir = DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(dest_dir, basename(url))
  unzip_dir <- file.path(dest_dir, "tmp_distritos")
  
  tryCatch({
    # download
    download.file(url, zip_path, mode = "wb")
    
    # unzip
    unzip(zip_path, exdir = unzip_dir)
    
    # localizar arquivo específico
    target_file <- list.files(
      unzip_dir,
      pattern = "Agregados_por_distritos_pessoas_indigenas_BR.csv",
      full.names = TRUE,
      recursive = TRUE
    )
    
    
    if (length(target_file) == 0) {
      stop("Arquivo de agregados por distrito (PI) não encontrado.")
    }
    
    # Ler arquivo
    distritos <- data.table::fread(target_file, encoding = 'Latin-1')
    colnames(distritos) <- tolower(names(distritos))
    
    # salvar CSV
    output_path <- file.path(dest_dir, 'distritos.csv')
    # Salvar tudo em um único arquivo GeoPackage
    data.table::fwrite(distritos, output_path)
  },
  finally = {
    # limpeza (remove zip e pasta extraída)
    unlink(zip_path)
    unlink(unzip_dir, recursive = TRUE)
  }
  )
  
  #return(output_path)
  return(distritos)
}


download_malha_setores_censitarios <- function(
    url = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/malha_com_atributos/setores/gpkg/BR/BR_setores_CD2022.gpkg"
) {
  
  dest_dir = DIR_BRUTOS
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(dest_dir, basename(url))
  
  # download
  df <- st_read(url, quiet = TRUE)
  
  colnames(df) <- tolower(names(df))
  
  # correções
  df <- sf::st_make_valid(df)
  df <- sf::st_transform(df, 4326)
  
  
  # salvar shapefile
  output_sf <- file.path(dest_dir, basename(url))
  sf::st_write(df, output_sf, delete_dsn = TRUE, quiet = TRUE)
  
  #return(output_sf)
  return(df)
}


get_sidra_api <- function(url){
  
  resp <- request(url) |> 
    req_perform()
  
  txt <- resp_body_string(resp)
  
  df <- jsonlite::fromJSON(txt)
  
  return(df)
}


# TODO: unused, and returns nothing — needs return value if ever wired in
trata_dados_siape <- function(pasta_siape){

  arquivos_siape <- list.files(pasta_siape, full.names = T)

  arq_afastamentos <- arquivos_siape[grepl('Afastamentos', arquivos_siape)]
  arq_cadastro <- arquivos_siape[grepl('Cadastro', arquivos_siape)]
  arq_remuneracao <- arquivos_siape[grepl('Remuneracao', arquivos_siape)]
  arq_uorgs <- arquivos_siape[grepl('DIM_UORG', arquivos_siape)]

  # 30202 = código do órgão FUNAI no SIAPE
  cadastro <- data.table::fread(arq_cadastro, encoding = 'Latin-1')
  cadastro <- cadastro[COD_ORG_EXERCICIO == 30202, ]
  uorgs <- data.table::fread(arq_uorgs, encoding = 'Latin-1')
  uorgs <- uorgs[uorg_orgao_co == 30202, ]

  remuneracao <- data.table::fread(arq_remuneracao, encoding = 'Latin-1')

}
