# ==============================================================================
# CONFIGURAÇÃO CENTRAL DO PROJETO
# ==============================================================================
# Constantes compartilhadas entre os scripts do pipeline {targets}. Este
# arquivo é fonteado primeiro (ver _targets.R) para garantir que fique
# disponível para todos os demais arquivos de R/.

# --- Sistemas de referência de coordenadas ---
CRS_EXTRACAO <- 4674 # SIRGAS 2000 - usado na extração dos dados brutos
CRS_GEOPROCESSAMENTO <- 4326 # WGS 84 - usado nas operações de geoprocessamento
# EPSG:10857 (SIRGAS 2000 / Brasil Polyconic - Albers Equal Area), padrão do
# projeto para cálculos de área/distância em escala nacional.
CRS_ALBERS_BR <- 10857

# --- Diretórios de dados e saídas ---
# Caminhos relativos à raiz do projeto.
DIR_BRUTOS      <- "dados/brutos"
DIR_PROCESSADOS <- "dados/processados"
DIR_IMPUTACOES  <- "dados/imputacoes"

# Tudo o que o pipeline gera para leitura humana fica em saidas/ (não versionada). Os documentos
# renderizados pelo Quarto (nota técnica, apresentação) vão para saidas/documentos/ (_quarto.yml).
DIR_SAIDAS        <- "saidas"
DIR_FIGURAS       <- file.path(DIR_SAIDAS, "figuras")
DIR_TABELAS       <- file.path(DIR_SAIDAS, "tabelas")
DIR_RELATORIOS_CR <- file.path(DIR_SAIDAS, "relatorios_cr")
DIR_LOGS          <- file.path(DIR_SAIDAS, "logs")

# --- Parâmetros de chamadas à API OSRM ---
OSRM_MAX_TENTATIVAS  <- 5
OSRM_ESPERA_SEGUNDOS <- 10

# --- O que atualizar em relação aos insumos congelados (variável de ambiente JURISDICOES_ATUALIZAR) ---
# "nada" (padrão): usa só os insumos da release de dados e reproduz exatamente as malhas publicadas.
# "osrm": refaz as requisições ao servidor público do OSRM (muitas horas) com as mesmas CRs; o
#   resultado pode variar, porque a malha viária do servidor muda com o tempo.
# "siorg": baixa a estrutura atual do SIORG (indigenR), geocodifica de novo os endereços das CRs e
#   refaz as requisições ao OSRM -- incorpora CRs criadas depois da publicação e pode mudar bastante
#   o resultado. CRs novas não estão em dicionario_cr.rds nem nas regras de imputação.
# Os insumos atualizados são gravados em DIR_PROCESSADOS; os congelados de DIR_BRUTOS não mudam.
# Defina no .Renviron, por exemplo: JURISDICOES_ATUALIZAR=osrm
MODO_ATUALIZACAO <- Sys.getenv("JURISDICOES_ATUALIZAR", unset = "nada")
if (!MODO_ATUALIZACAO %in% c("nada", "osrm", "siorg")) {
  stop("JURISDICOES_ATUALIZAR deve ser \"nada\", \"osrm\" ou \"siorg\" (valor atual: \"", MODO_ATUALIZACAO, "\")")
}
ATUALIZAR_SIORG <- identical(MODO_ATUALIZACAO, "siorg")
OSRM_MODO <- if (MODO_ATUALIZACAO %in% c("osrm", "siorg")) "api" else "congelado"

# Respostas do OSRM (matriz de tempos e distâncias entre CRs e setores censitários)
ARQ_RESPOSTAS_OSRM <- file.path(DIR_BRUTOS, "osrm_respostas.parquet")
ARQ_RESPOSTAS_OSRM_API <- file.path(DIR_PROCESSADOS, "osrm_respostas_api.parquet")

# Piso mínimo de setores censitários sem sigilo do IBGE ("X") exigido para publicar a taxa de
# um indicador por CR (registro civil, alfabetização, saneamento, água -- ver
# tabela_dados_malha_basica() em R/gera_graficos_relatorio.R). Abaixo desse número de setores
# válidos, a CR mostra "dado insuficiente" no mapa em vez de uma porcentagem calculada sobre uma
# amostra pequena demais para ser estável -- CRs pequenas e remotas (ex.: Vale do Javari, Médio
# Juruá) concentram justamente os maiores percentuais de setores sigilosos, por terem setores de
# baixíssima densidade populacional, exatamente a condição que aciona o sigilo estatístico.
LIMIAR_AMOSTRA_MINIMA_INDICADOR <- 30

# Destino da tabela consolidada do OSRM (tabela_osrm_consolidada / resultados_api.parquet, gravada
# por consolidar_resultados_osrm()). "local" grava em DIR_PROCESSADOS, na própria pasta do projeto;
# "nuvem" grava no mesmo diretório usado como *store* do pipeline em _targets.yaml.
OSRM_CACHE_DESTINO <- "local" # "local" ou "nuvem"

DIR_CACHE_OSRM <- if (identical(OSRM_CACHE_DESTINO, "nuvem")) {
  file.path(targets::tar_config_get("store"), "cache_osrm")
} else {
  DIR_PROCESSADOS
}

# --- Endpoints de download (WFS/geoservers) ---
# Usado via tar_map(values = URLS_WFS, ...) em _targets.R.
URLS_WFS <- tibble::tribble(
  ~nome, ~link,
  "tis", "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai%3Atis_poligonais_portarias&outputFormat=application%2Fjson&maxFeatures=10000",
  "tis_ibge", "https://geoservicos.ibge.gov.br/geoserverIBGE/CGMAT/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CGMAT%3Aqg_2022_610_terraindigena__v02&outputFormat=application%2Fjson&maxFeatures=600000",
  "aldeias",  "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai%3Aaldeias_pontos&outputFormat=application%2Fjson&maxFeatures=10000",
  "aeroporto", "https://geoservicos.ibge.gov.br/geoserverCCAR/CCAR/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CCAR%3ABC250_2025_aer_complexo_aeroportuario_p&outputFormat=application%2Fjson&maxFeatures=600000",
  "pista_pouso", "https://geoservicos.ibge.gov.br/geoserverCCAR/CCAR/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CCAR%3Apista_ponto_pouso&outputFormat=application%2Fjson&maxFeatures=600000",
  "hidroviario", "https://geoservicos.ibge.gov.br/geoserverCCAR/CCAR/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CCAR%3Ahidrovias_escala&outputFormat=application%2Fjson&maxFeatures=600000",
  "cr", "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai%3Atis_cr&outputFormat=application%2Fjson&maxFeatures=10000",
  "ti_estudo", "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai%3Atis_pontos_portarias&outputFormat=application%2Fjson&maxFeatures=10000",
  "mun_rgint", 'https://geoservicos.ibge.gov.br/geoserverIBGE/CGMAT/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CGMAT%3Aqg_2022_120_reggeoginterm&outputFormat=application%2Fjson&maxFeatures=600000',
  "mun_rgi", 'https://geoservicos.ibge.gov.br/geoserverIBGE/CGMAT/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CGMAT%3Aqg_2024_110_reggeogimed&outputFormat=application%2Fjson&maxFeatures=600000',
  "drenagem", 'https://geoservicos.ibge.gov.br/geoserverCCAR/CCAR/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CCAR%3ABC250_2025_hid_trecho_drenagem_l&outputFormat=application%2Fjson&maxFeatures=600000',
  "rodovias", 'https://geoservicos.inde.gov.br/geoserver/MInfra/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=MInfra:Rodovias%20Federais&outputFormat=application/json',
  "amazonia_legal", 'https://geoservicos.ibge.gov.br/geoserver/CGEO/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CGEO:AmazoniaLegalLimites&maxFeatures=600000&outputFormat=application/json'
)

# --- Arquivos manuais ---
# ARQ_CADU_NOME (export manual do Visdata/CadÚnico) foi removida: o alvo arq_cadu agora usa
# baixar_cadunico_racor() / indigenR::visdata_importa_racor_indigena() (ver R/importando_dados.R).
# Declaração do atendimento por CR (processo SEI n° 08620.010286/2021-89), versionada.
ARQ_ATENDIDOS_DECLARADOS <- file.path(DIR_IMPUTACOES, "cr_municipio.xlsx")

# Extratos anuais (2020-2026) do Sistema de Informação da Atenção à Saúde Indígena (SIASI),
# um arquivo .xlsx por ano, usados para a série histórica de população indígena em TIs.
DIR_SIASI <- file.path(DIR_BRUTOS, "SIASI")

# Insumos sem API pública (ou cuja API devolve só a situação atual), distribuídos na release de
# dados junto com os demais insumos congelados (ver dados/brutos/MANIFESTO.csv).
ARQ_CARGOS       <- file.path(DIR_BRUTOS, "dados_cargos.csv")           # valor dos CCE/FCE (SIORG)
ARQ_UORG_UG      <- file.path(DIR_BRUTOS, "tb_uorg_ug.xlsx")            # unidade organizacional x UG
ARQ_SIOP         <- file.path(DIR_BRUTOS, "Extrator1787666469433.csv")  # execução orçamentária (SIOP)
ARQ_BDQUEIMADAS  <- file.path(DIR_BRUTOS, "bdqueimadas_2026-01-01_2026-06-02.geojson") # focos (INPE)
URL_SIORG_CARGOS <- "https://siorg.gov.br/siorg-cidadao-webapp/api/cargos-comissionados/cargos?sigla="
ARQ_CR_GEOCODE   <- file.path(DIR_BRUTOS, "cr_geocode.gpkg")            # endereços das CRs geocodificados

# Estrutura organizacional (SIORG, via indigenR): congelada em DIR_BRUTOS ou, com
# JURISDICOES_ATUALIZAR=siorg, baixada de novo para DIR_PROCESSADOS.
DIR_SIORG             <- if (ATUALIZAR_SIORG) DIR_PROCESSADOS else DIR_BRUTOS
ARQ_SIORG             <- file.path(DIR_SIORG, "siorg.csv")
ARQ_SIORG_ESTRUTURA   <- file.path(DIR_SIORG, "siorg_estrutura.csv")

# Manifesto dos insumos congelados (arquivo, tamanho e SHA-256), versionado no repositório.
ARQ_MANIFESTO_INSUMOS <- "dados/MANIFESTO_INSUMOS.csv"

# --- Arquivos de referência manuais versionados (ver dados/imputacoes/ no repositório) ---
# Dicionário de CRs (codigo_siape, codigo_siorg, codigo_ug_siafi, sigla, nome, ...), usado para
# reconstruir o código SII (`undadm_codigo` das TIs) a partir do código SIAPE de cada CR.
ARQ_DICIONARIO_CR <- "dicionario_cr.rds"

# Regras de imputação (decisões de vinculação território -> CR, uma por linha, com justificativa)
# e imputações manuais por setor censitário -- ver dados/imputacoes/README.md.
ARQ_REGRAS_IMPUTACAO <- "regras_imputacao.csv"
ARQ_IMPUTACOES_SETOR <- "imputacoes_setor.csv"

# Códigos SIAPE antigos de CRs recadastradas, usados como alias em adicionar_cd_sii()
# (R/gera_graficos_relatorio.R). Algumas Terras Indígenas ainda trazem, em undadm_codigo (SII), o
# código SIAPE anterior à mudança de cadastro da CR -- sem o alias, o join com o cd_sii atual
# (construído a partir do codigo_siape vigente em dicionario_cr) falha, e a TI aparece como "sem
# vinculação no SII" mesmo tendo uma CR responsável registrada (só que sob o código antigo).
# Descoberto em 09/2026: as 23 TIs sem vinculação eram todas de CR-JPA e CR-GPV, com undadm_nome
# preenchido corretamente ("COORDENACAO REGIONAL DE JOAO PESSOA"/"DE GUARAPUAVA"), mas
# undadm_codigo apontando para o SIAPE antigo.
ALIASES_SIAPE_CR <- tibble::tribble(
  ~codigo_siorg, ~codigo_siape_antigo,
  230272,        2287, # CR-JPA (João Pessoa)
  230279,        2408  # CR-GPV (Guarapuava)
)

# --- Projeto QGIS de Áreas de Atuação das CRs (AACR) ---
# Arquivo único versionado no repositório (estilos, layouts e caminhos de camada relativos aos
# .gpkg gerados pelo pipeline já configurados manualmente na interface do QGIS) -- ver
# exportar_layouts_qgis() em R/visualizar_resultados.R, única etapa que ainda o consome.
ARQ_PROJETO_QGIS_AACR <- "qgis/ProjetoAACR_FUNAI.qgz"

# Executável headless do QGIS usado por exportar_layouts_qgis(). O caminho varia por máquina:
# defina a variável de ambiente PYTHON_QGIS_BAT no seu .Renviron (não versionado) se o QGIS não
# estiver no local padrão de instalação do OSGeo4W para o usuário atual.
PYTHON_QGIS_BAT <- Sys.getenv(
  "PYTHON_QGIS_BAT",
  unset = file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "OSGeo4W", "bin", "python-qgis.bat")
)
# Nomes exatos (ver QGIS > Gerenciador de Layouts) dos layouts de impressão exportados para
# dados/figuras pelo alvo `layouts_qgis_aacr`. "Demonstrativo Áreas de Atuação" e "Demonstrativo
# Malha Básica SC" só têm a página geral (Brasil inteiro) usada no relatório -- as páginas
# regionais existem no layout mas não são embutidas no .qmd.
LAYOUTS_QGIS_AACR <- c(
  "Demonstrativo Áreas de Atuação",
  "Demonstrativo Malha Básica SC",
  "Demonstrativo Áreas de Atuação Proposta Atual",
  "Demonstrativo Áreas de Atuação Proposta Município",
  "Critérios",
  "Demonstrativo Jurisdição Atual"
)

# --- CRs sem polígono próprio na malha básica ---
# A CR-RR II foi desconsiderada no algoritmo por ter o mesmo endereço da CR-RR I (nota técnica,
# seção Dados e Metodologia): sua futura área está contida na da CR-RR I. Ela participa dos
# encontros regionais (GRUPOS_ENCONTROS_REGIONAIS) mesmo sem aparecer em arquivo_final_sc_mun.
CRS_SEM_AREA_PROPRIA <- c("CR-RR II")

# --- Agrupamentos de CRs para os encontros virtuais regionais de validação (Tabela 9 da nota
# técnica) --- Definição institucional (não geográfica pura): cada elemento é um encontro, na
# ordem em que aparece na tabela, com as CRs de participação obrigatória (`obrigatoria`) por
# sigla. `regiao` e `opcional` são opcionais: quando ausentes, montar_tabela_encontros_regionais()
# (R/gera_graficos_relatorio.R) calcula a Região predominante do encontro e as CRs de participação
# optativa (vizinhas por fronteira, fora do agrupamento); quando presentes, prevalecem sobre o
# cálculo. Aqui estão todos fixados conforme a versão da Tabela 9 assinada no SEI (Nota Técnica
# nº 1/2026/Coplan/CGGE/Dages-FUNAI), que foi definida manualmente pela CGGE.
GRUPOS_ENCONTROS_REGIONAIS <- list(
  list(
    regiao = "Sul/Sudeste",
    obrigatoria = c("CR-MGES", "CR-Lise", "CR-GPV", "CR-INTS", "CR-PFD", "CR-LIS"),
    opcional = c("CR-ATO", "CR-BSF", "CR-CGR", "CR-PP", "CR-DOU", "CR-SBA")
  ),
  list(
    regiao = "Centro-Oeste",
    obrigatoria = c("CR-RC", "CR-XIN", "CR-XAV", "CR-CGB", "CR-CGR", "CR-DOU", "CR-PP", "CR-ATO"),
    opcional = c(
      "CR-CAC", "CR-NOMT", "CR-NMT", "CR-GPV", "CR-Lise", "CR-MGES", "CR-BSF", "CR-MA",
      "CR-BTO", "CR-KSPA"
    )
  ),
  list(
    regiao = "Nordeste",
    obrigatoria = c("CR-MA", "CR-CEPIRN", "CR-JPA", "CR-PE", "CR-ALSE", "CR-BSF", "CR-SBA"),
    opcional = c("CR-ATO", "CR-BTO", "CR-MGES")
  ),
  list(
    regiao = "Norte",
    obrigatoria = c("CR-VJ", "CR-MJur", "CR-JUR", "CR-MPur", "CR-Apur", "CR-AS", "CR-MSol"),
    opcional = c("CR-MAD", "CR-MAO", "CR-RNG")
  ),
  list(
    regiao = "Norte",
    obrigatoria = c("CR-MAD", "CR-JPR", "CR-GJM", "CR-NOMT", "CR-CAC"),
    opcional = c("CR-Apur", "CR-CGB", "CR-MAO", "CR-MPur", "CR-NMT")
  ),
  list(
    regiao = "Norte",
    obrigatoria = c("CR-RR I", "CR-RR II", "CR-RNG", "CR-MAO", "CR-ANP"),
    opcional = c("CR-CLPA", "CR-JPR", "CR-MAD", "CR-MPur", "CR-MSol", "CR-NMT", "CR-NOMT", "CR-TPJ")
  ),
  list(
    regiao = "Norte",
    obrigatoria = c("CR-TPJ", "CR-CLPA", "CR-BTO", "CR-KSPA", "CR-NMT"),
    opcional = c(
      "CR-MA", "CR-MAO", "CR-ATO", "CR-RC", "CR-XAV", "CR-XIN", "CR-CGB", "CR-NOMT", "CR-ANP"
    )
  )
)
