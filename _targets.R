library(targets)
library(tarchetypes)
library(tibble)
library(dplyr)
library(crew)

# Setup targets ----
tar_option_set(
  packages = c("tibble", "sf", "indigenR", "dplyr", "geocodebr", "ggplot2", "osrm", "qs2", "leaflet",
               "readxl", "data.table", "tictoc", "igraph", "httr2", "jsonlite", "openxlsx", "arrow", "kableExtra", "janitor",
               "quarto", "gt", "ggtext", "duckdb", "duckspatial", "DBI", "rmapshaper"), # Packages that your targets need for their tasks.
  format = "qs",
  memory = 'transient',
  controller = crew_controller_local(
    workers = 1,
    tasks_max = 1,
    # Alguns alvos pesados (ex.: df_imputacao_norte) derrubam o worker com falha de
    # segmentação nativa (GEOS/sf) de forma intermitente/probabilística nesta máquina;
    # aumenta o teto de tentativas automáticas além do padrão (5) antes de desistir.
    crashes_max = 20
  ),
  garbage_collection = TRUE
)

# Configuração e funções ----
tar_source('R/config.R') # constantes (CRS, diretórios, parâmetros de API) — fonteado primeiro
tar_source('R/importando_dados.R')
tar_source('R/siorg_geocode.R')
tar_source('R/gera_graficos_relatorio.R')
tar_source('R/define_divisoes_cr.R')
tar_source('R/visualizar_resultados.R')
tar_source('R/geoprocessamento.R')
tar_source('R/funcoes_imputacao.R')
tar_source('R/relatorio_cr.R')
# Versão híbrida DuckDB+spatial de calcular_imputacoes_norte(), usada no alvo
# df_imputacao_norte abaixo. A função original permanece intocada em
# R/geoprocessamento.R (calcular_imputacoes_norte()) para referência/rollback.
tar_source('R/imputacao_norte_duckdb.R')

# Insumos congelados (release de dados): para antes de qualquer alvo se algum faltar ou divergir
# do manifesto -- ver verificar_insumos_congelados() em R/importando_dados.R.
verificar_insumos_congelados()

# Lista de URLs dos Geoservers ---- (definida em R/config.R: URLS_WFS)

# Início do Pipeline ----
list(
  # ETAPA 1 - Importando dados brutos ----
  
  # 1.1. Dados: Declaração do atendimento por CR ----
  # Os dados foram coletados do processo SEI n° 08620.010286/2021-89.
  tar_target(
    atendidos_declarados,
    ARQ_ATENDIDOS_DECLARADOS,
    format = "file"
  ),
  
  
  # 1.2. Dados: Download dos dados dos geoservers ----
  tar_map(
    values = URLS_WFS,
    names = nome,
    
    tar_target(
      arq, # prefixo
      baixar_wfs(link, nome),
      format = "qs" 
    )
  ),
  
  # 1.3. Dados: Shapefile do Brasil IBGE (2025) ----
  tar_target(
    arq_limite_fisico,
    baixar_shapefile_zip(
      url = "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2025/Brasil/BR_Pais_2025.zip",
      arquivo_saida = "uf.gpkg",
      padrao_busca = "BR_Pais_[0-9]{4}\\.shp$"
    )
  ),
  
  # 1.3b. Dados: Malha de UF IBGE (2025) ----
  tar_target(
    arq_uf,
    baixar_shapefile_zip(
      url = "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2025/Brasil/BR_UF_2025.zip",
      arquivo_saida = "malha_uf.gpkg",
      padrao_busca = "BR_UF_[0-9]{4}\\.shp$"
    )
  ),

  # 1.3c. Dados: Malha de Grandes Regiões IBGE (2025) ----
  tar_target(
    arq_regioes,
    baixar_shapefile_zip(
      url = "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2025/Brasil/BR_Regioes_2025.zip",
      arquivo_saida = "malha_regioes.gpkg",
      padrao_busca = "BR_Regioes_[0-9]{4}\\.shp$"
    )
  ),

  # 1.4. Dados: Divisão Territorial Brasileira (DTB) (2025) ----
  tar_target(
    arq_dtb,
    download_process_dtb()
  ),
  
  # 1.5. Dados: Malha municipal IBGE (2025) ----
  tar_target(
    arq_municipios,
    download_malha_municipal()
  ),
  
  # 1.6. Dados: Malha distrital IBGE (2025) ----
  tar_target(
    arq_distritos,
    download_malha_distritos()
  ),
  
  # 1.9. Dados: Agregados por SC de PIs (CENSO 2022) ----
  tar_target(
    arq_agregados_distritos,
    download_agregados_distritos_pi()
  ),
  
  # 1.10. Dados: Setores censitários indígenas (CENSO 2022) ----
  tar_target(
    censo_agregado_sc_pi,
    download_censo_agregado_sc_pi()
  ),
  
  # Malha completa dos setores censitários (CENSO 2022) ----
  tar_target(
    arq_setores_censitarios,
    download_malha_setores_censitarios()
  ),
  
  tar_target(
    censo_agregado_sc_pi_dados,
    baixar_e_unir_agregados_censo(
      url_pessoas = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/Agregados_por_setores_pessoas_indigenas_BR.zip",
      url_domicilios = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/Agregados_por_setores_domicilios_indigenas_BR.zip",
      join = 'cd_setor'
    )
  ),
  
  # Dados: Agregados por município PIs IBGE (CENSO 2022) ----
  tar_target(
    censo_agregado_mun_pi_dados,
    baixar_e_unir_agregados_censo(
      url_pessoas = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Municipio_csv/Agregados_por_municipios_pessoas_indigenas_BR.zip",
      url_domicilios = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Municipio_csv/Agregados_por_municipios_domicilios_indigenas_BR.zip",
      join = 'cd_mun'
    )
  ),
  
  # Dados: Agregados por distrito PIs IBGE (CENSO 2022) ----
  tar_target(
    censo_agregado_dist_pi_dados,
    baixar_e_unir_agregados_censo(
      url_pessoas = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Distrito_csv/Agregados_por_distritos_pessoas_indigenas_BR.zip",
      url_domicilios = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Distrito_csv/Agregados_por_distritos_domicilios_indigenas_BR.zip",
      join = 'cd_dist'
    )
  ),
  
  # Dados: Agregados por subdistrito PIs IBGE (CENSO 2022) ----
  tar_target(
    censo_agregado_subdist_pi_dados,
    baixar_e_unir_agregados_censo(
      url_pessoas = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_SubDistrito_csv/Agregados_por_subdistritos_pessoas_indigenas_BR.zip",
      url_domicilios = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_SubDistrito_csv/Agregados_por_subdistritos_domicilios_indigenas_BR.zip",
      join = 'cd_subdist'
    )
  ),
  
  # Dados: Agregados por bairro PIs IBGE (CENSO 2022) ----
  tar_target(
    censo_agregado_bairro_pi_dados,
    baixar_e_unir_agregados_censo(
      url_pessoas = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Bairro_csv/Agregados_por_bairros_pessoas_indigenas_BR.zip",
      url_domicilios = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Bairro_csv/Agregados_por_bairros_domicilios_indigenas_BR.zip",
      join = 'cd_bairro'
    )
  ),
  
  
  # 1.12. Dados: Localidades Indígenas IBGE (CENSO 2022) ----
  tar_target(
    arq_localidades_indigenas,
    download_localidades_indigenas()
  ),
  
  # 1.13. Dados: Regiões de influência dos centros urbanos (REGIC) (IBGE) ----
  tar_target(
    arq_regic,
    download_regic()
  ),
  
  # 1.14. Dados: População por Terra Indígena e Município IBGE (CENSO 2022) ----
  
  tar_target(
    tb_pop_ti,
    get_sidra_api(
      url = "https://apisidra.ibge.gov.br/values/t/9719/v/576,1432/n1125/all/h/n"
    )
  ),
  
  tar_target(
    tb_pop_pi,
    get_sidra_api(
      url = "https://apisidra.ibge.gov.br/values/t/8175/n6/all/v/allxp/p/last%201/c287/100362/c2/6794/c2661/32776"
    )
  ),

  # População indígena por município, EXCLUINDO quem mora em Terra Indígena (c2661/12870 = "Fora
  # de terras indígenas"). Não confundir com tb_pop_pi (c2661/32776 = Total, TI incluída, usado
  # como denominador de pct_cadunico) -- este alvo existe especificamente para compor total_pi por
  # CR em calcular_populacao_indigena_cr_sidra() sem dupla contagem com tb_pop_ti.
  tar_target(
    tb_pop_mun_fora_ti,
    get_sidra_api(
      url = "https://apisidra.ibge.gov.br/values/t/8175/n6/all/v/allxp/p/last%201/c287/100362/c2/6794/c2661/12870"
    )
  ),

  # 1.16. Dados: Importando dados do Siorg ----
  tar_target(
    arq_siorg,
    baixar_siorg()
  ),

  # 1.17. Dados: Importando dados adicionais de estrutura do SIORG (relatório dinâmico) ----
  tar_target(
    arq_siorg_estrutura,
    importar_siorg_estrutura()
  ),
  
  tar_target(
    arq_cargos,
    baixar_cargos_siorg()
  ),
  
  tar_target(
    arq_uorg_ug,
    read.xlsx(ARQ_UORG_UG)
  ),

  # Dicionário de CRs (siape/siorg/ug_siafi) usado para derivar o código SII das TIs. Nome/sigla
  # são atualizados a partir do SIORG vigente (ver atualizar_nomes_dicionario_cr()); os demais
  # campos (codigo_siape, codigo_siorg, codigo_ug_siafi, uf, situacao, ...) vêm do .rds estático.
  tar_target(
    dicionario_cr,
    atualizar_nomes_dicionario_cr(
      dicionario_cr_estatico = readRDS(file.path(DIR_IMPUTACOES, ARQ_DICIONARIO_CR)),
      arq_siorg = arq_siorg
    )
  ),

  # 1.18: Dados: Dados orçamentários do SIOP ----
  tar_target(
    arq_siop,
    data.table::fread(ARQ_SIOP) |>
      dplyr::rename(
        ano = "Ano Exercício",
        ug_resp = "Unidade Gestora Resp.", 
        nm_ug_resp = "Unidade Gestora Resp. (desc.)",
        programa = "Programa",
        nm_programa = "Programa (desc.)", 
        cd_acao = "Ação",
        nm_acao = "Ação (desc.)",
        autorizado = "Autorizado", 
        empenhado = "Empenhado",
        emp_liquidado = "Empenhado Liquidado",
        emp_liquidar = "Empenhado a/em Liquidar", 
        pago = "Pago",
        disponivel = "Disponível*"
      )
  ),
  
  # 1.19. Dados: Etnias ----
  tar_target(
    arq_etnias,
    {
      dados <- read.xlsx(r'(https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Etnias_e_Linguas_Indigenas_principais_caracteristicas_sociodemograficas_Resultados_do_universo/Tabelas_complementares/xlsx/Tabela_complementar_25.xlsx)',
                         startRow = 8)
      colnames(dados) <- c('cd_uf', 'nm_uf', 'cd_mun', 'nm_mun', 'cd_etnia', 'nm_etnia', 'qtd')
      dados
    }
  ),
  
  # 1.20. Dados: Línguas ----
  tar_target(
    arq_linguas,
    {
      dados <- read.xlsx(r'(https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Etnias_e_Linguas_Indigenas_principais_caracteristicas_sociodemograficas_Resultados_do_universo/Tabelas_complementares/xlsx/Tabela_complementar_26.xlsx)',
                startRow = 8)
      colnames(dados) <- c('cd_uf', 'nm_uf', 'cd_mun', 'nm_mun', 'cd_lingua', 'nm_lingua', 'qtd')
      dados
    }
  ),
  
  # 1.21. Dados: Cadastro Único (VisData, via indigenR) ----
  tar_target(
    arq_cadu,
    baixar_cadunico_racor()
  ),

  # 1.22. Dados: Geopackage do osrm ----
  tar_target(
    name = arquivo_gpkg_norte,
    command = baixar_geopackage_norte("https://download.geofabrik.de/south-america/brazil/norte-latest-free.gpkg.zip"),
    format = "file" 
  ),
  
  # Camada de Rodovias (depende do arquivo do Target 2)
  tar_target(
    name = mapa_vias,
    command = sf::st_read(arquivo_gpkg_norte, "gis_osm_roads_free")
  ),
  
  # Camada de Hidrovias (depende do arquivo do Target 2)
  tar_target(
    name = mapa_hidrovias,
    command = sf::st_read(arquivo_gpkg_norte, "gis_osm_waterways_free")
  ),
  
  # 1.23. Dados: Queimadas (BDQUEIMADAS IPE)
  tar_target(
    arq_bdqueimadas,
    sf::st_read(ARQ_BDQUEIMADAS)
  ),
  
  # Target para as Hidrovias da ANTAQ
  tar_target(
    hidrovias_antaq,
    baixar_hidrovias_en()
  ),
  
  # Target para os Portos da ANTAQ
  tar_target(
    portos_antaq,
    baixar_portos_en()
  ),
  
  # ETAPA 2 - Processamento ----
  # VERSÃO 1 ----
  
  # 2.1. Geocodificando os endereços do SIORG ----
  # Usa a geocodificação congelada (dados/brutos/cr_geocode.gpkg); com JURISDICOES_ATUALIZAR=siorg,
  # geocodifica de novo os endereços do SIORG atual com o pacote geocodebr (IPEA).
  tar_target(
      geocodificacao_cr,
      load_cr_geocode(
        arq_siorg,
        arq_municipios),
      format = "file"
    ),

  # 2.1b. Linhas de subordinação (Unidade Técnica Local -> CR responsável) ----
  tar_target(
    linhas_subordinacao,
    construir_linhas_subordinacao(geocodificacao_cr),
    format = "file"
  ),

  # 2.2. Aplicando filtragens e tratando dados ----
  # A CR-RR II foi excluída pois atualmente funciona no mesmo endereço da CR-RR I
  tar_target(
    sf_pontos_filtrados,
    {
      sf::st_read(geocodificacao_cr, quiet = TRUE) |>
        dplyr::filter(grepl('^CR-', trimws(sigla))) |>
        dplyr::filter(codigoUnidade != 509592) |>
        sf::st_make_valid() |>
        sf::st_transform(4326) |>
        # arrange() por as.numeric(codigoUnidade), não pela coluna crua (character): a matriz de
        # durações cacheada pela API do OSRM (resultados_api_brutos, cue = 'never') foi
        # construída com as origens em ordem NUMÉRICA de codigoUnidade -- resgatar_resultados_completo()
        # (R/define_divisoes_cr.R) rotula cada coluna da matriz na ordem de sf_pontos$codigoUnidade
        # aqui, então um arrange() em ordem de texto (ex.: "108028" antes de "87633") desalinha
        # essa correspondência posicional e rotula toda a malha com o cd_uorg errado.
        dplyr::arrange(as.numeric(codigoUnidade))
    }
  ),
  
  tar_target(
    sf_setores_filtrados,
    {
      arq_setores_censitarios |>
        dplyr::arrange(as.numeric(cd_setor)) |>
        sf::st_make_valid() |> 
        sf::st_transform(4326) |>
        dplyr::group_by(cd_setor) |>
        dplyr::summarise(
          dplyr::across(-geom, dplyr::first),
          geom = sf::st_union(geom),
          .groups = "drop"
        ) |>
        sf::st_as_sf()
    }
  ),
  
  # Processa topologia para construir grafo
  tar_target(
    matriz_topologia_setores,
    sf::st_touches(sf_setores_filtrados) # Calcula uma única vez e guarda em cache
  ),
  
  # 2.3. Gerando polígonos a partir dos setores censitários ----
  # Cria a lista de lotes usando os SETORES e PONTOS JÁ FILTRADOS
  # Essa etapa é necessária por causa do limite de requisições da API do OSRM
  # O branching dinâmico do targets ajuda em casos de falha na requisição, aproveitando os dados já baixados.
  
  # 2.5. OSRM: Importando dados da API ----
  tar_target(
    dados_origens,
    {
      coords <- sf::st_coordinates(sf_pontos_filtrados)

      data.frame(
        id_origem = seq_len(nrow(sf_pontos_filtrados)),
        cd_uorg = sf_pontos_filtrados$codigoUnidade,
        lon = as.numeric(coords[,1]),
        lat = as.numeric(coords[,2])
      ) |>
        # Mesmo motivo de sf_pontos_filtrados: ordem numérica de cd_uorg, não lexicográfica, para
        # bater com a ordem das origens já submetidas à API do OSRM (resultados_api_brutos).
        dplyr::arrange(as.numeric(cd_uorg))
    }
  ),
  
  # 2.4. OSRM: Criando lotes ----
  tar_target(
    lotes_destinos_lista_sc,
    prepara_lotes_osrm(
      dados_origens = dados_origens,
      sf_poligonos = sf_setores_filtrados,
      limite_elementos_matriz = 10000
    ),
    iteration = "list"
  ),
  
  
  # 2.5. OSRM: respostas da API ----
  # OSRM_MODO (R/config.R, variável de ambiente JURISDICOES_OSRM) decide a origem das respostas:
  # "congelado" lê as respostas guardadas na release de dados (reprodução exata); "api" refaz as
  # requisições ao servidor público do OSRM, lote a lote, e grava as respostas novas.
  if (identical(OSRM_MODO, "api")) {
    list(
      tar_target(
        resultados_api_brutos,
        requisitar_api_osrm(
          lotes_destinos_lista_sc,
          dados_origens = dados_origens
        ),
        pattern = map(lotes_destinos_lista_sc),
        # Um ramo por lote: se a execução cair no meio, os lotes já respondidos são aproveitados.
        # Para refazer requisições já concluídas, use targets::tar_invalidate(resultados_api_brutos).
        deployment = "main"
      ),
      tar_target(
        arq_respostas_osrm,
        extrair_respostas_osrm(resultados_api_brutos, sf_pontos_filtrados, ARQ_RESPOSTAS_OSRM_API),
        format = "file"
      )
    )
  } else {
    tar_target(arq_respostas_osrm, ARQ_RESPOSTAS_OSRM, format = "file")
  },

  # 2.6. OSRM: consolidando as respostas (linha reta, critério e ordem das CRs por setor) ----
  targets::tar_target(
    tabela_osrm_consolidada,
    consolidar_resultados_osrm(
      arq_respostas = arq_respostas_osrm,
      sf_pontos = sf_pontos_filtrados,
      sf_setores = sf_setores_filtrados,
      dir_output = 'resultados_api.parquet'
    ),
    format = "file"
  ),
  
  targets::tar_target(
    resultados_processados,
    processar_calculos_osrm_v2(
      tabela_osrm_consolidada,
      malha_setores = arq_setores_censitarios
      )
  ),
  
  targets::tar_target(
    arquivo_gpkg_resultado,
    salva_geopackage_final_osrm(
      sf_poligonos = sf_setores_filtrados,
      df_resultados_osrm = resultados_processados,
      sf_pontos = sf_pontos_filtrados,
      nome_arquivo = "setores_cr_todos.gpkg"
    ),
    format = "file"
  ),
  
  # # APLICA A IMPUTAÇÃO DECLARADA (REMOVIDO)
  # tar_target(
  #   arquivo_gpkg_imputado,
  #   imputar_atendimento_declarado(
  #     arq_gpkg_final = arquivo_gpkg_resultado,
  #     arq_tabela_declarados = atendidos_declarados,
  #     arq_siorg = arq_siorg
  #   ),
  #   format = "file"
  # ),

  # 2.9. Gera e Aplica imputações do Amazonas ----
  tar_target(
    df_imputacao_norte,
    {
      # 1. Filtro dos dados de entrada para a Amazônia Legal. O filtro por UF (11, 12, 13, 14,
      # 15, 16, 17, 21, 51) é só um pré-filtro barato (a Amazônia Legal está inteiramente contida
      # nesses 9 estados, então nunca exclui um setor que devesse entrar) -- a decisão real usa o
      # polígono oficial do IBGE (alvo `arq_amazonia_legal`, camada AmazoniaLegalLimites),
      # verificando se o centroide do setor cai dentro dele. Isso evita incluir setores de
      # municípios de MA/MT que ficam fora da delimitação oficial da Amazônia Legal, mas que
      # antes entravam só por pertencerem a uma dessas UFs.
      setores_norte_uf <- sf_setores_filtrados |>
        dplyr::filter(cd_uf %in% c(11, 12, 13, 14, 15, 16, 17, 21, 51))

      # O polígono oficial da Amazônia Legal e a malha de setores censitários são produtos
      # distintos do IBGE, generalizados/simplificados de forma independente -- o contorno de um
      # não bate exatamente com o do outro, mesmo onde ambos deveriam coincidir (ex.: limite de
      # UF). Testado com dados reais: municípios claramente amazônicos (Macapá, Oiapoque,
      # Pacaraima, São Gabriel da Cachoeira) têm centroides de setor a até ~2 km de distância do
      # polígono oficial só por causa dessa generalização, enquanto municípios do Maranhão
      # genuinamente fora da Amazônia Legal (ex.: Barreirinhas, Araioses) ficam a 40+ km dele --
      # uma tolerância de 5 km de buffer absorve o ruído de generalização sem incluir nenhum
      # município realmente fora da delimitação oficial.
      centroides_norte_uf <- sf::st_centroid(sf::st_geometry(setores_norte_uf))
      poligono_amazonia_legal <- sf::st_transform(arq_amazonia_legal, sf::st_crs(setores_norte_uf)) |>
        sf::st_buffer(5000)
      dentro_amazonia_legal <- lengths(sf::st_intersects(centroides_norte_uf, poligono_amazonia_legal)) > 0

      setores_norte <- setores_norte_uf[dentro_amazonia_legal, ]

      # 2. Execução da função multimodal (versão DuckDB+spatial -- ver
      # R/imputacao_norte_duckdb.R; a original calcular_imputacoes_norte() permanece
      # em R/geoprocessamento.R para referência/rollback)
      calcular_imputacoes_norte_duckdb(
        sf_setores   = setores_norte,
        sf_crs       = sf_pontos_filtrados,
        sf_rodovias  = mapa_vias |> dplyr::filter(fclass %in% c('motorway', 'trunk', 'primary', 'secondary')),
        sf_hidrovias = mapa_hidrovias |> dplyr::filter(fclass %in% c('river')), #hidrovias_antaq |> dplyr::filter(cla_icacao == "Navegável"),
        sf_portos    = portos_antaq,
        vel_rod = 60.0,
        vel_hid = 20.0,
        vel_walk = 4.5,
        max_walk_km = 30.0,
        max_ratio = 3.0
      )
    }
  ),
  
  
  tar_target(
    df_imputacao,
    {
      imputs_am <- df_imputacao_norte$imputacao_principal |> dplyr::filter(cd_uf == 13)
      # Retorna o dataframe direto pra memória!
      imputs <- imputs_am |> dplyr::distinct(cd_setor, .keep_all = TRUE)
      imputs
    }
  ),
  
  # 2.10. Salva arquivo final do resultado puro ---- 
  tar_target(
    arquivo_gpkg_imputado_final,
    imputar_atendimento(
      arq_gpkg = arquivo_gpkg_resultado,
      df_imputacao = df_imputacao |> select(cd_setor, codigoUnidadeImputado),
      sf_siorg = sf_pontos_filtrados,
      out_dir = 'setores_cr_imputados_final.gpkg',
      n_crit = 'Algoritmo de Roteamento (Norte)',
      n_etapa = 1,
      arq_resultados_osrm = tabela_osrm_consolidada
    ),
    format = "file"
  ),
  
  # 2.11. Aplica correções topológicas ----
  tar_target(
    arquivo_gpkg_suavizado,
    corrigindo_topologia(
      arq_entrada = arquivo_gpkg_imputado_final,
      sf_limite = arq_limite_fisico,
      lista_adjacencia_global = matriz_topologia_setores,
      caminho_saida = file.path(DIR_PROCESSADOS, "setores_cr_suavizado.gpkg"),
      coluna_alvo =  "cd_uorg",
      arq_resultados_osrm = tabela_osrm_consolidada
    ),
    deployment = "main",
    format = "file"
  ),
  
  # 2.11b. Exporta dados de população indígena normalizados (DuckDB) ----
  # Alvo compartilhado pelas versões 1/2/2+MSOL de unir_poligonos_tabular_cr(). Usa
  # exportar_indigenas_duckdb_sql() (R/define_divisoes_cr.R), não exportar_indigenas_duckdb():
  # a versão em R (loop de data.table::set() sobre as ~1029 colunas "v0*" de
  # censo_agregado_sc_pi_dados) mede pico de ~9,94GB nesta máquina -- ainda estoura o
  # orçamento de 6-7GB da máquina alvo mesmo isolada em worker próprio. A versão SQL baixa os
  # 2 CSVs do IBGE direto (mesmas URLs de censo_agregado_sc_pi_dados acima) e faz a
  # união+normalização inteiramente em DuckDB, sem materializar a tabela larga em R: mede
  # ~3,07GB de pico (mesma ordem do dissolve, que já era a etapa mais leve) e ~55s, resultado
  # idêntico. Por isso este alvo NÃO depende de censo_agregado_sc_pi_dados (que continua
  # existindo só para os outros consumidores em R/gera_graficos_relatorio.R) -- baixa sua
  # própria cópia dos CSVs, custo extra de ~35s de download.
  tar_target(
    arq_pi_duckdb_dados,
    exportar_indigenas_duckdb_sql(
      url_pessoas = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/Agregados_por_setores_pessoas_indigenas_BR.zip",
      url_domicilios = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/Agregados_por_setores_domicilios_indigenas_BR.zip",
      join_var = 'cd_setor',
      caminho_saida = file.path(DIR_PROCESSADOS, "censo_indigenas_normalizado.duckdb")
    ),
    format = "file"
  ),

  # 2.12. Unindo polígonos ----
  # Dividido em 3 alvos (tabular / geometria / join final) -- cada um roda no seu próprio
  # worker do crew (memory = 'transient' em tar_option_set()), o que é necessário para que a
  # memória de cada etapa pesada seja devolvida ao sistema operacional antes da próxima (ver
  # notas de memória em unir_poligonos_tabular_cr()/unir_poligonos_geometria() em
  # R/define_divisoes_cr.R -- rm()+gc() dentro do mesmo processo não bastava nesta máquina).
  tar_target(
    df_tabular_cr_sc,
    unir_poligonos_tabular_cr(
      arq_gpkg = arquivo_gpkg_suavizado,
      arq_pi_duckdb = arq_pi_duckdb_dados,
      join_var = 'cd_setor'
    )
  ),
  tar_target(
    sf_geometria_cr_sc,
    unir_poligonos_geometria(arq_gpkg = arquivo_gpkg_suavizado)
  ),
  tar_target(
    arquivo_final_sc,
    unir_poligonos_final(
      sf_geometria = sf_geometria_cr_sc,
      df_tabular = df_tabular_cr_sc,
      dir_output = 'poligonos_cr_final_sc.gpkg',
      CRS_EXTRACAO = CRS_EXTRACAO
    ),
    format = "file"
  ),

  #'##################################################################################################
  ############################################ VERSÃO 2 ##############################################
  #'##################################################################################################
  #'
  #' A versão 2 resulta da adaptação dos resultados obtidos a partir da rotina de atribuição dos setores censitários
  #' considerando critérios não incorporados na metodologia, que foram mapeados pela CGGE.
  #' Em geral, os critérios são:
  #' 
  #' 1. Existência de questões interétnicas que exigiram a atribuição manual de polígonos;
  #' 2. Indicativos das decisões da alta gestão;
  #' 3. Adstrição do território de terras indígenas a apenas uma CR sempre que possível
  #' 4. Ajustes pontuais para melhoria dos resultados
  #' 
  #' Como consequência de algumas imputações, alguns setores censitários passaram por uma realocação que desconsidera
  #' a primeira melhor opção anteriormente obtida pela rotina.
  # 2.14. Identifica setores que irão passar por realocação ----
  # Lógica extraída para identificar_setores_realocados_v2() em R/funcoes_imputacao.R
  # (mesmo comportamento, apenas nomeada para reduzir o tamanho do bloco inline).
  tar_target(
    sf_setores_realocados_v2,
    identificar_setores_realocados_v2(arquivo_gpkg_imputado_final)
  ),

  # 2.15. Processa os resultados da realocação ----
  tar_target(
    arquivo_gpkg_resultado_v2,
    processa_realocacoes(
      arq_resultados_osrm = tabela_osrm_consolidada,
      sf_poligonos = st_read(arquivo_gpkg_suavizado),
      sf_setores_atual = sf_setores_realocados_v2,
      nome_arquivo = "setores_cr_realocados_v2.gpkg"
    ),
    deployment = "main"
  ),
  
  # 2.17. Mapeamento dos setores censitários em TIs ----
  #' Para a realização do censo demográfico de 2022, o IBGE considerou para fins de planejamento
  #' da operação censitária as terras indígenas declaradas, homologadas ou regularizadas e as
  #' reservas indígenas encaminhadas até 31/07/2022. Por isso, buscou-se estimar a população
  #' indígena em terras delimitadas atualmente ou declaradas em data posterior ao recorte do instituto
  #' mediante a identificação dos setores censitários que intersectam ou estão contidos no polígono da TI
  tar_target(
    df_setores_em_ti,
    mapear_setores_ti(
      sf_setores = arq_setores_censitarios,
      sf_tis = arq_tis,
      # threshold = 50: regra de maioria de área -- setor só é atribuído à TI quando ao menos
      # metade da sua própria área está contida nela (usado por tabela_dados_malha_basica() para
      # atribuir os indicadores do Censo por CR na seção "Potencialidades" da nota técnica).
      threshold = 50
    )
  ),
  
  targets::tar_target(
    mapeamento_setores_hidrovia,
    mapear_setores_linhas(
      sf_setores = arq_setores_censitarios,
      sf_linhas = sf::st_read(file.path(DIR_BRUTOS, "arquivos_hidroviario.gpkg"), quiet = TRUE),
      id_setor = "cd_setor",
      id_linha = "id"
    )
  ),
  
  # 2.18. Regras de imputação (dados/imputacoes/, ver README da pasta) ----
  tar_target(
    arq_regras_imputacao,
    file.path(DIR_IMPUTACOES, ARQ_REGRAS_IMPUTACAO),
    format = "file"
  ),
  tar_target(
    arq_imputacoes_setor,
    file.path(DIR_IMPUTACOES, ARQ_IMPUTACOES_SETOR),
    format = "file"
  ),
  tar_target(
    regras_imputacao,
    ler_regras_imputacao(arq_regras_imputacao, dicionario_cr)
  ),

  # Imputações por TI da V2, com as variáveis da TI (usadas no anexo da nota técnica)
  tar_target(
    imputs_ti_v2,
    imputar_por_ti(
      regras_imputacao |> dplyr::filter(etapa == "v2", tipo == "ti"),
      df_setores_em_ti
    )
  ),

  # 2.19. Imputações da versão 2 ----
  tar_target(
    df_imputacao_v2,
    {
      setores_v2 <- sf::st_read(arquivo_gpkg_resultado_v2, quiet = TRUE) |> sf::st_drop_geometry()

      aplicar_regras_imputacao(
        regras = regras_imputacao |> dplyr::filter(etapa == "v2"),
        setores = setores_v2,
        df_setores_em_ti = df_setores_em_ti,
        mapeamento_hidrovia = mapeamento_setores_hidrovia,
        imputacoes_setor = ler_imputacoes_setor(arq_imputacoes_setor),
        calculados = list(
          piaui_melhor_rota = gerar_imputs_melhor_rota_uf(
            tabela_osrm_consolidada, uf = "Piauí", crs_candidatas = c("118218", "87987")
          ),
          manter_cr_atual = setores_v2 |> dplyr::transmute(cd_setor, codigoUnidadeImputado = cd_uorg)
        )
      )
    }
  ),
  
  # 2.19 Cria arquivo final pós imputações (V2) ----
  tar_target(
    arquivo_gpkg_imputado_final_v2,
    imputar_atendimento(
      arq_gpkg = arquivo_gpkg_resultado_v2, 
      df_imputacao = df_imputacao_v2,
      sf_siorg = sf_pontos_filtrados,
      out_dir = 'setores_cr_imputados_final_v2.gpkg',
      n_crit = 'Imputação Manual (CGGE)',
      n_etapa = 2,
      arq_resultados_osrm = tabela_osrm_consolidada
      
    ),
    format = "file"
  ),
  
  # 2.20. Aplicando correções topológicas (V2) ----
  tar_target(
    arquivo_gpkg_suavizado_v2,
    corrigindo_topologia(
      arq_entrada = arquivo_gpkg_imputado_final_v2,
      sf_limite = arq_limite_fisico,
      lista_adjacencia_global = matriz_topologia_setores,
      caminho_saida = file.path(DIR_PROCESSADOS, "setores_cr_suavizado_v2.gpkg"),
      coluna_alvo =  "cd_uorg",
      arq_resultados_osrm = tabela_osrm_consolidada
    ),
    deployment = "main",
    format = "file"
  ),
  
  # 2.21. Unindo polígonos (V2) ---- (dividido em 3 alvos -- ver nota em "2.12." acima)
  tar_target(
    df_tabular_cr_sc_v2,
    unir_poligonos_tabular_cr(
      arq_gpkg = arquivo_gpkg_suavizado_v2,
      arq_pi_duckdb = arq_pi_duckdb_dados,
      join_var = 'cd_setor'
    )
  ),
  tar_target(
    sf_geometria_cr_sc_v2,
    unir_poligonos_geometria(arq_gpkg = arquivo_gpkg_suavizado_v2)
  ),
  tar_target(
    arquivo_final_sc_v2,
    unir_poligonos_final(
      sf_geometria = sf_geometria_cr_sc_v2,
      df_tabular = df_tabular_cr_sc_v2,
      dir_output = 'poligonos_cr_final_sc_v2.gpkg',
      CRS_EXTRACAO = CRS_EXTRACAO
    ),
    format = "file"
  ),

  
  # Aprimoramento da proposta - Contribuições das demais unidades da Funai (GT CR-MSOL) ----
  tar_target(
    df_imputacao_msol,
    {
      setores_v2 <- sf::st_read(arquivo_gpkg_suavizado_v2, quiet = TRUE) |> sf::st_drop_geometry()

      aplicar_regras_imputacao(
        regras = regras_imputacao |> dplyr::filter(etapa == "msol"),
        setores = setores_v2,
        df_setores_em_ti = df_setores_em_ti,
        calculados = list(
          realocacao_msol = gerar_imputs_realocacao_cr(
            setores_v2, df_imputacao_norte$combinacoes, cd_uorg_origem = 509751
          )
        )
      )
    }
  ),

  # 2.19 Cria arquivo das imputacoes após proposta CR-MSOL ----
  tar_target(
    arquivo_gpkg_imputado_final_v2_msol,
    imputar_atendimento(
      arq_gpkg = arquivo_gpkg_suavizado_v2,
      df_imputacao = df_imputacao_msol,
      sf_siorg = sf_pontos_filtrados,
      out_dir = 'setores_cr_imputados_final_v2_msol.gpkg',
      n_crit = 'Imputação Manual (GT CR-MSOL)',
      n_etapa = 3,
      arq_resultados_osrm = tabela_osrm_consolidada
      
    ),
    format = "file"
  ),
  
  # 2.20. Aplicando correções topológicas (V2 + GT MSOL) ----
  tar_target(
    arquivo_gpkg_suavizado_v2_msol,
    corrigindo_topologia(
      arq_entrada = arquivo_gpkg_imputado_final_v2_msol,
      sf_limite = arq_limite_fisico,
      lista_adjacencia_global = matriz_topologia_setores,
      caminho_saida = file.path(DIR_PROCESSADOS, "setores_cr_suavizado_v2_msol.gpkg"),
      coluna_alvo =  "cd_uorg",
      arq_resultados_osrm = tabela_osrm_consolidada
    ),
    deployment = "main",
    format = "file"
  ),
  
  # 2.21. Unindo polígonos (V2 MSOL) ---- (dividido em 3 alvos -- ver nota em "2.12." acima)
  tar_target(
    df_tabular_cr_sc_v2_msol,
    unir_poligonos_tabular_cr(
      arq_gpkg = arquivo_gpkg_suavizado_v2_msol,
      arq_pi_duckdb = arq_pi_duckdb_dados,
      join_var = 'cd_setor'
    )
  ),
  tar_target(
    sf_geometria_cr_sc_v2_msol,
    unir_poligonos_geometria(arq_gpkg = arquivo_gpkg_suavizado_v2_msol)
  ),
  tar_target(
    arquivo_final_sc_v2_msol,
    unir_poligonos_final(
      sf_geometria = sf_geometria_cr_sc_v2_msol,
      df_tabular = df_tabular_cr_sc_v2_msol,
      dir_output = 'poligonos_cr_final_sc_v2_msol.gpkg',
      CRS_EXTRACAO = CRS_EXTRACAO
    ),
    format = "file"
  ),
  
  # 2.22b. Gerando gpkg de critérios de associação (V2 + GT MSOL) ----
  # Alimenta a camada "Critérios" do projeto QGIS AACR (qgis/ProjetoAACR_FUNAI.qgz, ver
  # exportar_layouts_qgis()) -- passado como dependência dep_arq_criterios_v2_msol do alvo
  # layouts_qgis_aacr para disparar a reexportação das figuras quando este gpkg mudar.
  # Usa criar_gpkg_criterios() (R/visualizar_resultados.R), que só grava esse gpkg -- a versão
  # anterior (criar_mapa_final()) também construía um mapa leaflet HTML e um GeoJSON que nenhuma
  # etapa seguinte do pipeline nem a nota técnica consumiam, só custava tempo de processamento.
  tar_target(
    criando_mapa_final_sc_v2_msol,
    criar_gpkg_criterios(
      arq_sc_final = arquivo_gpkg_suavizado_v2_msol,
      caminho_saida = file.path(DIR_PROCESSADOS, "criterios_mapa_final_v2_msol.gpkg")
    ),
    format = "file"
  ),

  #'##################################################################################################
  ############################################ VERSÃO 3 ##############################################
  #'##################################################################################################
  #' A versão 3 consiste na adequação dos resultados obtidos a nível de setor censitário para os polígonos de cada CR
  #' à divisão territorial político-administrativa dos municípios brasileiros, considerando as eventuais vantagens
  #' dessa adstrição, como por exemplo o aproveitamento de dados de outras fontes publicados a nível de desagregação
  #' municipal e a simplificação das relações das unidades regionais com as autoridades locais, que envolve atualmente
  #' um trabalho intenso de articulação.
  #' Após reunião com a Diretoria Colegiada no dia 28/07/2026, ficou estabelecido que as áreas de atuação serão definidas considerando
  #' municípios e terras indígenas.
  #' A partir dessa definição, as imputações passaram a considerar os polígonos dos limites municipais e das terras indígenas,
  #' independentemente de correspondência com os setores censitários.

  # 2.23. Atribuição a nível de município partindo dos setores censitários ----
  # Lógica extraída para gerar_df_imputacao_mun() em R/funcoes_imputacao.R
  # (mesmo comportamento, apenas nomeada para reduzir o tamanho do bloco inline).
  tar_target(
    df_imputacao_mun,
    gerar_df_imputacao_mun(
      arquivo_gpkg_suavizado_v2_msol = arquivo_gpkg_suavizado_v2_msol,
      censo_agregado_sc_pi = censo_agregado_sc_pi
    )
  ),
  
  # 2.24 Cria arquivo final pós imputações (V3) ----
  tar_target(
    arquivo_gpkg_imputado_final_mun,
    imputar_atendimento(
      arq_gpkg = arquivo_gpkg_suavizado_v2_msol, 
      df_imputacao = df_imputacao_mun,
      sf_siorg = sf_pontos_filtrados,
      out_dir = 'setores_cr_imputados_final_mun.gpkg',
      n_crit = 'Imputação Manual (V3)',
      n_etapa = 3,
      arq_resultados_osrm = tabela_osrm_consolidada
      ),
    format = "file"
  ),
  
  # 2.25. Corrigindo enclaves administrativos a nível de MUNICÍPIO (V3) ----
  # A partir da V3, o município passa a ser a unidade atômica de atribuição (decisão da Diretoria
  # Colegiada de 28/07/2026). Por isso a correção topológica deixa de rodar a nível de setor
  # censitário aqui (como acontecia nas V1/V2, ver `corrigindo_topologia()` mais acima): testes com
  # dados reais mostraram que alguns municípios têm setores internamente desconectados entre si
  # (ex.: Arauá/SE, cd_mun=2800407, tem 6 de seus 32 setores formando 4 fragmentos que não tocam o
  # restante do próprio município nem entre si) -- a correção topológica de setor tratava esses
  # fragmentos como "enclaves" e os devolvia para a CR vizinha, quebrando a exclusividade municipal
  # que a etapa de imputação (2.24) tinha acabado de garantir. Rodar a correção a nível de MUNICÍPIO
  # (ver corrigir_enclaves_municipais() em R/geoprocessamento.R) resolve o problema real que ainda
  # importa nessa granularidade -- um município inteiro ficar territorialmente isolado do bloco
  # principal de sua CR -- sem correr o risco de fragmentar um único município entre duas CRs.
  tar_target(
    arquivo_gpkg_suavizado_mun,
    corrigir_enclaves_municipais(
      arq_entrada = arquivo_gpkg_imputado_final_mun,
      sf_municipios = arq_municipios,
      sf_limite = arq_limite_fisico
    ),
    format = "file"
  ),

  # 2.25b. Reforço final de exclusividade municipal, rede de segurança para casos residuais (V3) ----
  # Não deve alterar nada na prática (corrigir_enclaves_municipais() já garante exclusividade por
  # construção), mas funciona como defesa em profundidade antes da verificação abaixo.
  tar_target(
    arquivo_gpkg_suavizado_mun_final,
    reforcar_exclusividade_municipal(arq_entrada = arquivo_gpkg_suavizado_mun),
    format = "file"
  ),

  # 2.25c. Verificação: cada município deve ter exatamente uma CR (interrompe o pipeline se falhar) ----
  tar_target(
    verificacao_exclusividade_mun,
    verificar_exclusividade_municipal(arquivo_gpkg_suavizado_mun_final)
  ),

  # 2.25d. Exporta dados de população indígena normalizados (DuckDB) -- V3 (dados de
  # censo_agregado_sc_pi, distinto de censo_agregado_sc_pi_dados usado nas versões 1/2/2+MSOL)
  tar_target(
    arq_pi_duckdb_mun,
    exportar_indigenas_duckdb(
      arq_pi = censo_agregado_sc_pi,
      join_var = 'cd_setor',
      caminho_saida = file.path(DIR_PROCESSADOS, "censo_indigenas_mun_normalizado.duckdb")
    ),
    format = "file"
  ),

  # 2.26. Unindo polígonos (V3) ---- (dividido em 3 alvos -- ver nota em "2.12." acima)
  tar_target(
    df_tabular_cr_sc_mun,
    unir_poligonos_tabular_cr(
      arq_gpkg = arquivo_gpkg_suavizado_mun_final,
      arq_pi_duckdb = arq_pi_duckdb_mun,
      join_var = 'cd_setor'
    )
  ),
  tar_target(
    sf_geometria_cr_sc_mun,
    unir_poligonos_geometria(arq_gpkg = arquivo_gpkg_suavizado_mun_final)
  ),
  tar_target(
    arquivo_final_sc_mun,
    unir_poligonos_final(
      sf_geometria = sf_geometria_cr_sc_mun,
      df_tabular = df_tabular_cr_sc_mun,
      dir_output = 'poligonos_cr_final_sc_mun.gpkg',
      CRS_EXTRACAO = CRS_EXTRACAO
    ),
    format = "file"
  ),

  # 2.26b. Malha básica (jurisdição por TI e por Município, sem ajustes manuais) ----
  # Consumida pelo painel_funai (processa_malha()), que aplica só a etapa de ajustes manuais
  # (config/malha_basica_overrides.csv) sobre esta base, em vez de recalcular a interseção espacial.
  tar_target(
    malha_basica_bruta,
    calcular_malha_basica_bruta(
      arq_poligonos_cr = arquivo_final_sc_mun,
      arq_area_atuacao = arquivo_gpkg_suavizado_mun_final,
      arq_tis = arq_tis,
      arq_municipios = arq_municipios,
      arq_uorg_ug = arq_uorg_ug,
      dicionario_cr = dicionario_cr,
      regras_imputacao = regras_imputacao
    ),
    format = "file"
  ),

  # 2.26c. Malha básica dissolvida por CR (camada "Malha Básica" do QGIS AACR) ----
  tar_target(
    malha_basica_dissolvida,
    dissolver_malha_basica_por_cr(malha_basica_bruta),
    format = "file"
  ),

  # 2.26d. Jurisdição ATUAL (declarada/oficial) por TI e por Município ----
  # Réplica de malha_basica_bruta/malha_basica_dissolvida, mas com a CR vinda da vinculação já
  # institucionalizada (SII para TI; declaração SEI/site institucional, já desempatada por
  # maior número SEI, para Município) -- não do algoritmo de roteamento. Ver
  # calcular_jurisdicao_atual_bruta() em R/geoprocessamento.R.
  tar_target(
    jurisdicao_atual_bruta,
    calcular_jurisdicao_atual_bruta(
      arq_tis = arq_tis,
      arq_uorg_ug = arq_uorg_ug,
      dicionario_cr = dicionario_cr,
      arq_municipios = arq_municipios,
      df_declarados = municipios_declarados_cr,
      arq_siorg = arq_siorg
    ),
    format = "file"
  ),

  tar_target(
    jurisdicao_atual_dissolvida,
    dissolver_malha_basica_por_cr(
      jurisdicao_atual_bruta,
      nome_arquivo = "jurisdicao_atual_dissolvida.gpkg"
    ),
    format = "file"
  ),

  # Mapa da jurisdição atual por CR (ggplot2) -- não usado mais em nt_jurisdicoes.qmd (a Figura 5
  # passou a vir do layout QGIS "Demonstrativo Jurisdição Atual", ver alvo layouts_qgis_aacr), mas
  # ainda consumido pela apresentação interna (apresentacao_crs.qmd, fora do repositório público).
  tar_target(
    mapa_jurisdicao_atual,
    criar_mapa_jurisdicao_atual(
      arq_poligonos_cr = jurisdicao_atual_dissolvida,
      arquivo = "mapa_jurisdicao_atual"
    )
  ),

  # Anexo em xlsx da proposta atualizada (TI, Município, Distrito, UF) ----
  tar_target(
    anexos_proposta_xlsx,
    gerar_anexos_proposta_xlsx(
      arq_malha_basica_bruta = malha_basica_bruta,
      arq_municipios = arq_municipios,
      arq_distritos = arq_distritos,
      arq_jurisdicao_atual_bruta = jurisdicao_atual_bruta
    ),
    format = "file"
  ),

  # Anexo em xlsx dos municípios com presença indígena sem nenhuma CR declarada (nem SEI, nem
  # site institucional) ----
  tar_target(
    anexo_municipios_sem_atendimento_xlsx,
    gerar_anexo_municipios_sem_atendimento_xlsx(
      arq_municipios = arq_municipios,
      tb_pop_mun = tb_pop_pi,
      df_declarados = municipios_declarados_cr
    ),
    format = "file"
  ),

  #

  # 2.27. Gerando mapa (V3) ----
  # tar_target(
  #   criando_mapa_final_sc_mun,
  #   criar_mapa_final(
  #     arq_sc_final = arquivo_gpkg_suavizado_mun,
  #     arq_poligonos = arquivo_final_sc_mun,
  #     sf_pontos = sf_pontos_filtrados,
  #     caminho_saida = file.path(DIR_SAIDAS, "mapas", "mapa_final_mun.html")
  #   ),
  #   format = "file"
  # ),
  
  # 3. Análise exploratória ----

  # 3.1. Identifica TIs ausentes na base do IBGE ----
  tar_target(
    tis_para_estimativa,
    arq_tis |> 
      sf::st_drop_geometry() |> 
      dplyr::anti_join(
        arq_tis_ibge |> 
          sf::st_drop_geometry() |> 
          dplyr::mutate(cd_funai = as.integer(cd_funai)),
        by = c("terrai_codigo" = "cd_funai")
      ) |> 
      dplyr::pull(terrai_codigo)
  ),
  
  # 3.2. Mapeamento espacial apenas para as TIs ausentes ----
  tar_target(
    mapeamento_setores_estimativa,
    mapear_setores_ti(
      sf_setores = arq_setores_censitarios,
      sf_tis = arq_tis,
      cd_tis = tis_para_estimativa,
      # threshold = 90: regra de quase-totalidade -- setor só entra na estimativa de população da
      # TI quando ao menos 90% da sua própria área está contida nela (usado por
      # calcular_populacao_indigena_cr_sidra()/grafico_situacao_atual_sii() para estimar população
      # indígena em TIs sem código oficial do IBGE).
      threshold = 90
    )
  ),
  
  # Tabela de dados por CR usando a malha básica (situação atual, sem ajustes manuais) ----
  # Substitui a antiga tabela_dados_final_sc (tabela_dados_sc(), removida por não ter mais nenhum
  # consumidor) como fonte dos mapas coropléticos abaixo: malha_basica_bruta é a jurisdição
  # institucionalmente mais recente (TI + Município, decisão da Diretoria Colegiada de
  # 28/07/2026) -- arquivo_final_sc_v2_msol é um estágio intermediário anterior a ela.
  tar_target(
    tabela_dados_final_malha_basica,
    tabela_dados_malha_basica(
      arq_malha_basica_dissolvida = malha_basica_dissolvida,
      arq_malha_basica_bruta = malha_basica_bruta,
      arq_setores_censitarios = arq_setores_censitarios,
      df_setores_em_ti = df_setores_em_ti,
      agg_sc = censo_agregado_sc_pi_dados,
      arq_siorg = arq_siorg,
      arq_siorg_estrutura = arq_siorg_estrutura,
      arq_siop = arq_siop,
      arq_cargos = arq_cargos,
      arq_ug = arq_uorg_ug,
      arq_bdqueimadas = arq_bdqueimadas,
      arq_tis = arq_tis,
      tb_pop_ti_sidra = tb_pop_ti,
      tb_pop_mun_fora_ti_sidra = tb_pop_mun_fora_ti,
      df_tis_ibge = arq_tis_ibge |> sf::st_drop_geometry(),
      df_mapeamento_setores = mapeamento_setores_estimativa
    )
  ),
  
  # Relatório PDF padronizado por CR: dados ----
  # SIASI detalhado (TI/aldeia/sexo/faixa etária) -- não substitui serie_historica_pi_siasi
  # (mantida para a série nacional já usada na nota técnica); ver R/relatorio_cr.R.
  tar_target(
    df_siasi_detalhado,
    importar_siasi_detalhado(dir_siasi = DIR_SIASI)
  ),

  # Uma linha por CR com os indicadores do relatório padronizado (Potencialidades + SIASI +
  # aldeias + localidades indígenas + etnias/línguas, já rankeados entre as CRs). Consumida pelo
  # template `relatorio_cr.qmd`, um render por CR (ver alvo `relatorios_cr` mais abaixo).
  tar_target(
    dados_relatorio_cr,
    montar_dados_relatorio_cr(
      malha_basica = malha_basica_bruta,
      tabela_potencialidades = tabela_dados_final_malha_basica,
      df_siasi_detalhado = df_siasi_detalhado,
      arq_aldeias = arq_aldeias,
      arq_localidades_indigenas = arq_localidades_indigenas,
      arq_etnias = arq_etnias,
      arq_linguas = arq_linguas
    )
  ),

  # Um PDF por CR (branching dinâmico, uma linha de `dados_relatorio_cr` por render) ----
  tarchetypes::tar_quarto_rep(
    relatorios_cr,
    path = "documentos/relatorio_cr.qmd",
    execute_params = {
      if (!dir.exists(DIR_RELATORIOS_CR)) dir.create(DIR_RELATORIOS_CR, recursive = TRUE)
      dados_relatorio_cr |>
        dplyr::transmute(
          cd_uorg = cd_uorg,
          output_file = file.path(DIR_RELATORIOS_CR, paste0("relatorio_cr_", sigla, ".pdf"))
        )
    }
  ),

  # Cria tabela final com dados consolidados ----
  tar_target(
    tabela_dados_final,
    tabela_dados_municipio(
      arq_final = arquivo_final_sc_mun,
      arq_final_sc = arquivo_gpkg_suavizado_mun,
      agg_mun = tb_pop_pi,
      arq_siorg = arq_siorg,
      arq_siorg_estrutura = arq_siorg_estrutura,
      arq_siop = arq_siop,
      arq_cargos = arq_cargos,
      arq_ug = arq_uorg_ug,
      arq_bdqueimadas = arq_bdqueimadas,
      arq_tis = arq_tis,
      arq_cadu = arq_cadu,
      arq_etnias = arq_etnias,
      arq_linguas = arq_linguas,
      arq_malha_basica_bruta = malha_basica_bruta,
      tb_pop_ti_sidra = tb_pop_ti,
      tb_pop_mun_fora_ti_sidra = tb_pop_mun_fora_ti,
      df_tis_ibge = arq_tis_ibge |> sf::st_drop_geometry(),
      df_sc_agregado = censo_agregado_sc_pi_dados,
      df_mapeamento_setores = mapeamento_setores_estimativa
    )
  ),

  # Tabela dos encontros virtuais regionais de validação (Tabela 9 da nota técnica) ----
  # Região predominante + CRs de participação optativa (fronteira, fora do agrupamento) por
  # encontro -- ver montar_tabela_encontros_regionais() em R/gera_graficos_relatorio.R.
  tar_target(
    tabela_encontros_regionais,
    montar_tabela_encontros_regionais(
      arq_poligonos_cr = arquivo_final_sc_mun,
      arq_regioes = arq_regioes,
      grupos = GRUPOS_ENCONTROS_REGIONAIS
    )
  ),

  # QA não-bloqueante: compara total_pi por setor (antigo) vs. método híbrido SIDRA (novo) ----
  tar_target(
    verificacao_populacao_sidra,
    verificar_populacao_indigena_cr_sidra(
      arq_final_sc_mun = arquivo_final_sc_mun,
      arq_malha_basica_bruta = malha_basica_bruta,
      tb_pop_ti_sidra = tb_pop_ti,
      tb_pop_mun_fora_ti_sidra = tb_pop_mun_fora_ti,
      df_tis_ibge = arq_tis_ibge |> sf::st_drop_geometry(),
      df_sc_agregado = censo_agregado_sc_pi_dados,
      df_mapeamento_setores = mapeamento_setores_estimativa
    )
  ),
  
  # 4. Gráficos e Tabelas ----
  
  # Tabela: População Indígena por Região (SIDRA) ----
  tar_target(
    pop_indigena_regiao,
    obter_pop_indigena_regiao(
      "https://apisidra.ibge.gov.br/values/t/9970/n2/all/v/allxp/p/all/c287/100362/c2661/all/c1/6795",
      arquivo_saida = file.path(DIR_TABELAS, "populacao_indigena_grande_regiao.xlsx")
    )
  ),
  
  # Tabela: População Indígena Fora de Terra Indígena por Situação do Domicílio e Região (SIDRA) ----
  tar_target(
    pop_indigena_fora_ti_situacao,
    obter_pop_indigena_regiao_situacao(
      url = "https://apisidra.ibge.gov.br/values/t/9970/n2/all/v/allxp/p/all/c287/100362/c2661/12870/c1/all",
      arquivo_saida = file.path(DIR_TABELAS, "populacao_indigena_situacao_domicilio_grande_regiao.xlsx")
    )
  ),
  
  # Tabela: Resultado Bruto ----
  tar_target(
    resultado_bruto,
    tabela_resultado_bruto(
      arq_resultado_bruto = tabela_osrm_consolidada
    )
  ),
  
  # Tabela: Demonstrativo das Imputações ----
  tar_target(
    anexos_imputacoes,
    gera_anexos_imputacoes(
      df_imputacoes = df_imputacao_v2,
      imputs_ti = imputs_ti_v2,
      sf_pontos_filtrados = sf_pontos_filtrados,
      arq_final = sf::st_read(
        arquivo_gpkg_suavizado,
        quiet = TRUE
      )
    )
  ),
  
  # Gráfico: População em TI (SII) ----
  tar_target(
    grafico_populacao_sii,
    grafico_situacao_atual_sii(
      df_sc_agregado = censo_agregado_sc_pi_dados, 
      sf_tis = arq_tis,
      df_tis_ibge = arq_tis_ibge |> sf::st_drop_geometry(),
      tb_pop_ti_sidra = tb_pop_ti,
      df_mapeamento_setores = mapeamento_setores_estimativa
    )
  ),
  
  # Gráfico: Comparativo da proporção da população indígena em área rural e total ----
  tar_target(
    grafico_pct_pi_comparativo,
      # 2. Passa os dados calculados para a função genérica
      criar_mapa_coropletico_evolucao_cr(
        df_plot = tabela_dados_final_malha_basica |>
          # 1. Calcula as proporções dinamicamente criando colunas com os nomes desejados para as facetas
          dplyr::mutate(
            soma_nacional_total = sum(total_pi, na.rm = TRUE),
            soma_nacional_rural = sum(total_pi_rural, na.rm = TRUE),
            pct_rural = dplyr::if_else(soma_nacional_rural > 0, (total_pi_rural / soma_nacional_rural) * 100, 0),
            pct_total = dplyr::if_else(soma_nacional_total > 0, (total_pi / soma_nacional_total) * 100, 0)
            
          ),
        variaveis = c(
          "População em Área Rural" = 'pct_rural',
          "População Total" = 'pct_total'
        ),
        titulo = "Distribuição Proporcional da População Indígena por CR",
        subtitulo = "Comparativo entre a concentração do público total e do público residente em áreas rurais",
        caption = "Valores em %\nFonte: IBGE (Censo Demográfico 2022)",
        titulo_legenda = "Proporção Nacional\n",
        cor = "#005a5a",
        prefixo = "",
        sufixo = "%",
        mostrar_valor = TRUE,
        tamanho_valor = 2.5,
        accuracy = 0.1,
        tamanho_sigla_cr = 2,
        nrow = 1,
        arquivo = "mapa_pct_populacao_indigena_comparativo"
      )
  ),
  
  # Gráfico: Orçamento por pessoa indígena ----
  tar_target(
    grafico_orcamento_pi_cr,
    criar_mapa_coropletico_cr(
      df_plot = tabela_dados_final_malha_basica,
      variavel = autorizado_por_pi_2025,
      titulo = "Orçamento Descentralizado em 2025 por Pessoa Indígena",
      subtitulo = "Orçamento discricionário descentralizado por indígena para cada CR",
      caption = "Valores em R$\nFonte: SIOP (RPs 2 a 9) e IBGE (2022)",
      titulo_legenda = "Orçamento por PI\n",
      cor = "#005a5a",
      mostrar_sigla = TRUE,
      mostrar_valor = TRUE,
      tamanho_sigla_cr = 2.5,
      tamanho_valor = 3,
      arquivo = "mapa_orcamento_descentralizado_por_pi"
    )
  ),
  
  # Gráfico: Focos de queimadas ----
  tar_target(
    grafico_focos_queimadas_cr,
      criar_mapa_coropletico_cr(
        df_plot = tabela_dados_final_malha_basica,
        variavel = n_focos,
        titulo = "Focos de Fogo por Coordenação Regional",
        subtitulo = "Contagem total de focos de calor identificados por CR (jan-mai 2026)",
        caption = "Fonte: Banco de Dados de Queimadas (INPE)",
        titulo_legenda = "Nº de Focos\n",
        cor = "#c47f2d",
        prefixo = "",
        arquivo = "mapa_focos_incendio_por_cr"
      )
  ),
  
  # Gráfico 4: Focos de queimadas em terras indígenas ----
  tar_target(
    grafico_focos_queimadas_ti,
    criar_mapa_coropletico_cr(
      df_plot = tabela_dados_final_malha_basica,
      variavel = n_focos_em_ti,
      titulo = "Focos de Fogo em Terras Indígenas por Coordenação Regional",
      subtitulo = "Contagem de focos de calor ocorridos dentro dos limites de TIs por CR (jan-mai 2026)",
      caption = "Fonte: Banco de Dados de Queimadas (INPE) e SII",
      titulo_legenda = "N° de Focos em TIs\n",
      cor = "#d94801",
      prefixo = NULL,
      arquivo = "mapa_focos_incendio_ti_por_cr",
      tamanho_sigla_cr = 2.5,
      tamanho_valor = 3
    )
  ),

  # Gráfico: Dispersão CCE vs Etnias ----
  tar_target(
    grafico_cce_etnias,
    criar_grafico_dispersao(
      df_plot = tabela_dados_final,
      var_x = qtd_etnias_15,
      var_y = total_pts_cce,
      var_label = sigla, # Utilizando a sigla das CRs como rótulo
      titulo = "Pontos de CCE vs Número de Etnias",
      subtitulo = "Distribuição de pontos CCE pelo número de etnias mapeadas pelo IBGE",
      caption = "Contabilizadas as etnias com no mínimo 15 pessoas por município\nFonte: SIORG e IBGE",
      xlab = "Número de Etnias",
      ylab = "Total de Pontos (CCE Unitário)",
      cor = "#005a5a",
      arquivo = "pts_cce_etnias_atendidas"
    )
  ),
  
  # Gráfico: Dispersão CCE vs Línguas ----
  tar_target(
    grafico_cce_linguas,
    criar_grafico_dispersao(
      df_plot = tabela_dados_final,
      var_x = qtd_linguas_15,
      var_y = total_pts_cce,
      var_label = sigla, 
      titulo = "Pontos de CCE vs Número de Línguas",
      subtitulo = "Distribuição de pontos CCE pelo número de línguas mapeadas pelo IBGE",
      caption = "Contabilizadas as línguas com no mínimo 15 falantes por município\nFonte: SIORG e IBGE",
      xlab = "Número de línguas",
      ylab = "Total de Pontos (CCE Unitário)",
      cor = "#005a5a",
      arquivo = "cce_linguas"
    )
  ),
  
  # Gráfico  Indígenas no Cadastro Único ----
  tar_target(
    grafico_cadunico_absoluto,
      criar_mapa_coropletico_cr(
        df_plot = tabela_dados_final,
        variavel = qtd_indigenas_cad,
        titulo = "Indígenas no Cadastro Único",
        # Período mais recente da extração do VisData (mesmo critério `referencia == max(referencia)`
        # já usado em tabela_dados_municipio() para os dados) -- nunca fica desatualizado sozinho.
        subtitulo = paste0(
          "População indígena no Cadastro Único para Programas Sociais por CR (",
          formatar_referencia_cadunico(max(arq_cadu$referencia)), ")"
        ),
        caption = "Proposta por municípios\nFonte: VisData (MDS)",
        titulo_legenda = "Indígenas no CadÚnico\n",
        cor = "#950606",
        prefixo = "",
        sufixo = "",
        arquivo = "mapa_pi_cadunico_absoluto",
        mostrar_sigla = TRUE,
        mostrar_valor = TRUE,
        accuracy = 1,
        tipo_malha = "mun"
      )
  ),
  
  # Gráfico: Registro de Nascimento por Coordenação Regional ----
  tar_target(
    grafico_registro_nascimento,
    criar_mapa_coropletico_cr(
      df_plot = tabela_dados_final_malha_basica |> mutate(
        # calcular_taxa_indicador() (R/gera_graficos_relatorio.R): setores com sigilo do IBGE
        # ("X") em qualquer uma das 3 variáveis já foram excluídos do numerador/denominador em
        # tabela_dados_malha_basica() (num_reg/den_reg); abaixo do piso de amostra
        # (LIMIAR_AMOSTRA_MINIMA_INDICADOR, R/config.R), a CR mostra "dado insuficiente" em vez
        # de uma taxa instável.
        pct_srn = calcular_taxa_indicador(num_reg, den_reg, n_setores_validos_reg)
      ),
      variavel = pct_srn,
      titulo = "Registro de Nascimento para pessoas indígenas de até 5 anos por Coordenação Regional",
      subtitulo = "Percentual de Pessoas indígenas de até 5 anos de idade que não tem ou não sabe se tem registro de nascimento",
      caption = "Fonte: IBGE (Censo 2022)",
      titulo_legenda = "% de Pessoas\n",
      cor = "#004CFF",
      prefixo = NULL,
      sufixo = "%",
      arquivo = "mapa_registro_civil",
      accuracy = .1,
      tamanho_sigla_cr = 2,
      tamanho_valor = 2.5
    )
  ),
  
  # Gráfico:  Analfabetismo por Coordenação Regional ----
  tar_target(
    grafico_alfabetizacao,
    criar_mapa_coropletico_cr(
      df_plot = tabela_dados_final_malha_basica |> mutate(
        # calcular_taxa_indicador() (R/gera_graficos_relatorio.R): setores com sigilo do IBGE
        # ("X") em v02101 ou v02137 já foram excluídos do numerador/denominador em
        # tabela_dados_malha_basica() (num_alf/den_alf) -- mesmo piso de amostra de pct_srn
        # acima. `num_alf`/`den_alf` são alfabetizados/total, então o percentual de
        # analfabetismo é o complemento: 100 - taxa de alfabetização.
        pct_analfab = 100 - calcular_taxa_indicador(num_alf, den_alf, n_setores_validos_alf)
      ),
      variavel = pct_analfab,
      titulo = "Taxa de Analfabetismo por Coordenação Regional",
      subtitulo = "Percentual de pessoas indígenas com 15 anos ou mais analfabetas",
      caption = "Fonte: IBGE (Censo 2022)",
      titulo_legenda = "% de Pessoas\n",
      cor = "#FF5100",
      prefixo = NULL,
      sufixo = "%",
      arquivo = "mapa_alfabetizacao",
      accuracy = .1,
      tamanho_sigla_cr = 2,
      tamanho_valor = 2.5
    )
  ),
  
  # Gráfico: Déficit de Saneamento por Coordenação Regional ----
  tar_target(
    grafico_deficit_saneamento,
    tabela_dados_final_malha_basica |>
      dplyr::mutate(
        # calcular_taxa_indicador() (R/gera_graficos_relatorio.R): setores com sigilo do IBGE
        # ("X") em qualquer uma das 8 variáveis de destino de esgoto já foram excluídos do
        # numerador/denominador em tabela_dados_malha_basica() (num_esg/den_esg) -- mesmo piso
        # de amostra de pct_srn.
        pct_esgoto_inadequado = calcular_taxa_indicador(num_esg, den_esg, n_setores_validos_esg)
      ) |>
      criar_mapa_coropletico_cr(
        variavel = pct_esgoto_inadequado,
        titulo = "Domicílios Indígenas com Esgotamento Sanitário Precário por Coordenação Regional",
        subtitulo = "Percentual de domicílios com esgoto destinado a fossas rudimentares, valas ou rios",
        caption = "Fonte: IBGE (Censo 2022)",
        titulo_legenda = "% de Domicílios\n",
        cor = "#8B4513", # Marrom (SaddleBrown) associado a saneamento e infraestrutura
        prefixo = "",
        sufixo = "%",
        mostrar_sigla = TRUE,
        mostrar_valor = TRUE,
        tamanho_sigla_cr = 2,
        tamanho_valor = 2.5,
        accuracy = 0.1,
        tipo_malha = "sc",
        arquivo = "mapa_deficit_saneamento_cr"
      )
  ),

  # Gráfico: Dependência de Rios e Lagos para Abastecimento de Água por Coordenação Regional ----
  tar_target(
    grafico_risco_hidrico,
    criar_mapa_coropletico_cr(
      df_plot = tabela_dados_final_malha_basica |>
        dplyr::mutate(
          # calcular_taxa_indicador() (R/gera_graficos_relatorio.R): setores com sigilo do IBGE
          # ("X") em qualquer uma das 8 variáveis de fonte de água já foram excluídos do
          # numerador/denominador em tabela_dados_malha_basica() (num_agua/den_agua) -- mesmo
          # piso de amostra de pct_srn.
          pct_agua_rio = calcular_taxa_indicador(num_agua, den_agua, n_setores_validos_agua)
        ),
      variavel = pct_agua_rio,
      titulo = "Dependência de Rios e Lagos para Abastecimento de Água por Coordenação Regional",
      subtitulo = "Percentual de domicílios indígenas abastecidos diretamente por rios, açudes, córregos ou igarapés",
      caption = "Fonte: IBGE (Censo 2022)",
      titulo_legenda = "% de Domicílios\n",
      cor = "#2C7BB6", # Azul profundo associado a recursos hídricos
      prefixo = "",
      sufixo = "%",
      mostrar_sigla = TRUE,
      mostrar_valor = TRUE,
      tamanho_sigla_cr = 2,
      tamanho_valor = 2.5,
      accuracy = 0.1,
      arquivo = "mapa_risco_hidrico_cr"
    )
  ),

  # Tabela: Relatório de TIs Inteiramente Contidas vs. Divididas ---
  tar_target(
    tabela_tis_por_cr,
    criar_tabela_tis_cr(
      arq_poligonos_cr = tabela_dados_final,
      arq_tis = arq_tis
    ),
    format = "file"
  ),
  
  # Tabela 3: Relatório de Concordância/Divergência entre a malha atual (TI + Município) e o SII
  tar_target(
    tabela_concordancia_sii,
    criar_tabela_concordancia_sii(
      arq_poligonos_cr = tabela_dados_final_malha_basica,
      arq_tis = arq_tis,
      arq_uorg_ug = arq_uorg_ug,
      dicionario_cr = dicionario_cr
    ),
    format = "file"
  ),

  tar_target(
    mapa_divergencia_ti_sii,
    criar_mapa_divergencia_ti_sii(
      arq_poligonos_cr = tabela_dados_final_malha_basica,
      arq_tis          = arq_tis,
      arq_uorg_ug      = arq_uorg_ug,
      dicionario_cr    = dicionario_cr,
      arquivo          = "mapa_divergencia_ti_sii"
    )
  ),

  # Tabela-síntese de divergência SII x Proposta, por TI (nº de TIs e população oficial por par
  # CR de origem/destino) -- mesma correspondência espacial do mapa acima (via
  # calcular_divergencia_ti_sii()), nunca divergindo dele.
  tar_target(
    tabela_divergencia_ti_sii,
    montar_tabela_divergencia_ti_sii(
      divergencia_ti = calcular_divergencia_ti_sii(
        arq_poligonos_cr = tabela_dados_final_malha_basica,
        arq_tis          = arq_tis,
        arq_uorg_ug      = arq_uorg_ug,
        dicionario_cr    = dicionario_cr
      ),
      tb_pop_ti_sidra = tb_pop_ti,
      df_tis_ibge = arq_tis_ibge
    )
  ),

  # Mapa: Convergência entre a malha atual (TI + Município) e os municípios declarados por CR ----
  tar_target(
    mapa_divergencia_municipio_declarado,
    criar_mapa_divergencia_municipio_declarado(
      arq_malha_basica_bruta = malha_basica_bruta,
      arq_municipios = arq_municipios,
      df_declarados = municipios_declarados_cr,
      arquivo = "mapa_divergencia_municipio_declarado"
    )
  ),

  # Tabela-síntese de divergência SEI x Proposta, por município (nº de municípios e população
  # indígena "fora de TI" por par CR declarante/destino) -- mesma correspondência do mapa acima
  # (via calcular_divergencia_municipio_declarado()), nunca divergindo dele.
  tar_target(
    tabela_divergencia_municipio_declarado,
    montar_tabela_divergencia_municipio_declarado(
      divergencia_mun = calcular_divergencia_municipio_declarado(
        arq_malha_basica_bruta = malha_basica_bruta,
        df_declarados = municipios_declarados_cr
      ),
      tb_pop_mun_fora_ti_sidra = tb_pop_mun_fora_ti,
      total_pop_fora_ti_nacional = pop_indigena_regiao |>
        dplyr::filter(`Região` == "Brasil") |>
        dplyr::pull(`Fora de terras indígenas`)
    )
  ),

  # Mapa: Municípios com declaração de atendimento sobreposta entre CRs (fonte bruta, antes do
  # desempate por maior número de documento SEI) ----
  tar_target(
    grafico_sobreposicao_municipios_declarados,
    criar_mapa_sobreposicao_municipios_declarados(
      arq_tabela_declarados = atendidos_declarados,
      arq_municipios = arq_municipios,
      arq_siorg = arq_siorg,
      arquivo = "mapa_sobreposicao_municipios_declarados"
    )
  ),

  tarchetypes::tar_quarto(
    nt_jurisdicoes,
    path = "documentos/nt_jurisdicoes.qmd"
  ),

  # Apresentação PowerPoint (resumo para as CRs), reaproveitando os mesmos alvos de mapas e
  # tabelas da nota técnica. O .qmd e o template .pptx são de uso interno e não acompanham o
  # repositório público (.gitignore): o alvo só existe quando o .qmd está presente.
  if (file.exists("documentos/apresentacao_crs.qmd")) {
    tarchetypes::tar_quarto(
      apresentacao_crs,
      path = "documentos/apresentacao_crs.qmd"
    )
  } else {
    list()
  },

  # Projeto QGIS de Áreas de Atuação (AACR): arquivo único versionado no repositório ----
  # O .qgz (qgis/ProjetoAACR_FUNAI.qgz) já é o resultado final -- estilos, layouts e caminhos de
  # camada relativos aos .gpkg gerados por este pipeline foram configurados uma única vez,
  # manualmente, na interface do QGIS (não há mais cópia/reconexão automatizada). format = "file"
  # só rastreia edições manuais nesse .qgz para disparar a reexportação das figuras.
  tar_target(
    arq_projeto_qgis_aacr,
    ARQ_PROJETO_QGIS_AACR,
    format = "file"
  ),

  # Alvos "format = 'file'" dedicados aos .gpkg que o projeto QGIS AACR lê diretamente do disco
  # (camadas UF, Terra Indígena, Rodovia, Hidrovia e Localidades Indígenas) -- os alvos originais
  # (arq_uf, arq_tis, arq_rodovias, arq_hidroviario, arq_localidades_indigenas) usam format = "qs"
  # de propósito, pois arq_tis sozinho é consumido como objeto sf em ~15 lugares deste arquivo; um
  # alvo qs só é invalidado quando o R detecta mudança no objeto/dependências, nunca verificando o
  # arquivo em si -- se o .gpkg em disco for reescrito por fora do {targets} (ou se o cache interno
  # de baixar_wfs()/baixar_shapefile_zip() disfarçar uma mudança real), o QGIS fica lendo um
  # arquivo desatualizado sem que layouts_qgis_aacr perceba. Estes alvos-satélite dependem do alvo
  # original (obrigando-o a rodar primeiro) e devolvem o caminho do .gpkg já gravado por ele,
  # dessa vez com format = "file" verificando o arquivo de fato -- sem alterar nenhum dos
  # consumidores existentes dos alvos originais.
  tar_target(arq_uf_arquivo, { arq_uf; file.path(DIR_BRUTOS, "malha_uf.gpkg") }, format = "file"),
  tar_target(arq_regioes_arquivo, { arq_regioes; file.path(DIR_BRUTOS, "malha_regioes.gpkg") }, format = "file"),
  tar_target(arq_tis_arquivo, { arq_tis; file.path(DIR_BRUTOS, "arquivos_tis.gpkg") }, format = "file"),
  tar_target(arq_rodovias_arquivo, { arq_rodovias; file.path(DIR_BRUTOS, "arquivos_rodovias.gpkg") }, format = "file"),
  tar_target(arq_hidroviario_arquivo, { arq_hidroviario; file.path(DIR_BRUTOS, "arquivos_hidroviario.gpkg") }, format = "file"),
  tar_target(arq_localidades_indigenas_arquivo, { arq_localidades_indigenas; file.path(DIR_BRUTOS, "BR_LIs_CD2022_20250919.gpkg") }, format = "file"),

  # Exporta layouts de impressão do projeto QGIS (AACR) para dados/figuras ----
  tar_target(
    layouts_qgis_aacr,
    exportar_layouts_qgis(
      arq_projeto_qgis = arq_projeto_qgis_aacr,
      layouts = LAYOUTS_QGIS_AACR,
      dir_saida = DIR_FIGURAS,
      python_qgis_bat = PYTHON_QGIS_BAT,
      # Os parâmetros dep_* abaixo não são usados dentro de exportar_layouts_qgis() -- o .qgz
      # commitado resolve os caminhos relativos aos .gpkg por conta própria quando reaberto pelo
      # QGIS. Eles servem só para estabelecer a aresta de dependência no grafo do {targets}: sem
      # isso, o hash do .qgz nunca mudaria e as figuras nunca seriam reexportadas quando esses
      # .gpkg forem regenerados.
      dep_arquivo_final_sc_mun = arquivo_final_sc_mun,
      dep_linhas_subordinacao = linhas_subordinacao,
      dep_malha_basica_dissolvida = malha_basica_dissolvida,
      dep_arq_regioes = arq_regioes_arquivo,
      dep_arq_uf = arq_uf_arquivo,
      dep_arq_tis = arq_tis_arquivo,
      dep_arq_rodovias = arq_rodovias_arquivo,
      dep_arq_hidroviario = arq_hidroviario_arquivo,
      dep_arq_localidades_indigenas = arq_localidades_indigenas_arquivo,
      dep_geocodificacao_cr = geocodificacao_cr,
      dep_arq_criterios_v2_msol = criando_mapa_final_sc_v2_msol,
      dep_arquivo_final_sc = arquivo_final_sc,
      dep_jurisdicao_atual_dissolvida = jurisdicao_atual_dissolvida
    ),
    format = "file"
  ),

  # Dados: Série histórica de população indígena em TIs (SIASI) ----
  tar_target(
    serie_historica_pi_siasi,
    importar_siasi_serie_historica(dir_siasi = DIR_SIASI)
  ),

  # Gráfico: Série histórica de população indígena em TIs (SIASI) ----
  tar_target(
    grafico_pi_siasi,
    grafico_serie_historica_pi_siasi(serie_historica_pi_siasi)
  ),

  # Tabela: Terras Indígenas por CR (SII), desagregada por fase ----
  tar_target(
    tabela_ti_por_fase_cr,
    tabela_ti_fase_cr(
      arq_tis = arq_tis,
      arq_ti_estudo = arq_ti_estudo,
      arq_uorg_ug = arq_uorg_ug,
      dicionario_cr = dicionario_cr
    )
  ),

  # Tabela: Precisão da geocodificação (apenas CRs) ----
  tar_target(
    tabela_geocode_precisao_cr,
    tabela_precisao_geocode_cr(sf_pontos_filtrados)
  ),

  # Tabela: Setores censitários por critério de associação à CR (nº, área e população indígena,
  # nacional) -- mesma coluna `criterio` que a Figura 3 usa, quantificada em vez de só mapeada ----
  tar_target(
    tabela_criterios_associacao,
    montar_tabela_criterios_associacao(
      arq_setores = arquivo_gpkg_suavizado_v2_msol,
      df_pi = censo_agregado_sc_pi
    )
  ),

  # Dados: Município-CR conforme declaração de atendimento (processo SEI) ----
  tar_target(
    municipios_declarados_cr,
    obter_municipios_declarados_cr(
      arq_tabela_declarados = atendidos_declarados,
      arq_siorg = arq_siorg
    )
  ),

  # Gráfico: Concentração indígena e carência de atendimento declarado por CR ----
  # Substitui grafico_pi_municipio_regic + grafico_pi_municipio_declarado (bases visuais
  # redundantes entre si -- ver criar_mapa_pi_carencia_atendimento()).
  tar_target(
    grafico_pi_carencia_atendimento,
    criar_mapa_pi_carencia_atendimento(
      sf_municipios = arq_municipios,
      tb_pop_mun = tb_pop_pi,
      df_declarados = municipios_declarados_cr,
      sf_localidades = arq_localidades_indigenas,
      arquivo = "mapa_pi_carencia_atendimento"
    )
  )

  # tar_target(
  #   grafico_municipios_atendidos_atual,
  #   criar_grafico_municipios_atual(
  #     path_atendimento = atendidos_declarados,
  #     path_municipios = arq_municipios,
  #     path_siorg = arq_siorg
  #   ),
  #   format = "file"
  # )#,
  
  # # CRIA GRÁFICO DE MUNICÍPIOS ATENDIDOS POR COORDENAÇÃO REGIONAL
  # tar_target(
  #   grafico_cr_mun_etapa,
  #   criar_grafico_cr_mun_etapa(
  #     arq_cr_mun_final = arq_cr_mun_final,
  #   ),
  #   format = 'file'
  # ),
  
  ) # FIM
