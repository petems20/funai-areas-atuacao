# AGENTS.md

Este arquivo orienta agentes de código (Codex, Claude Code etc.) que trabalham neste repositório. O `CLAUDE.md` apenas importa este arquivo; edite somente aqui.

## Visão geral do projeto

Pipeline geoespacial da FUNAI (Fundação Nacional dos Povos Indígenas). O objetivo é territorializar jurisdições, mapeando o atendimento de Coordenações Regionais (CRs) a municípios e setores censitários (IBGE), usando roteamento logístico (API OSRM) e operações topológicas (teoria dos grafos). Orquestração 100% com o pacote `{targets}`.

## Comandos comuns

- Ambiente: R 4.5.0 gerenciado por `renv` (`renv::restore()` para instalar dependências; `.Rprofile` já carrega `renv/activate.R` automaticamente ao abrir o projeto).
- O pacote `indigenR` (usado em `R/importando_dados.R`, hospedado em `petems20/indigenR` no GitHub) pode falhar ao ser instalado pelo `renv::restore()`, pois o mecanismo interno do `renv` para pacotes GitHub exige autenticação (`GITHUB_PAT`/`gitcreds`) nem sempre disponível. Se isso ocorrer, instale-o manualmente com `remotes::install_github("petems20/indigenR")` e rode `renv::restore()` novamente para os demais pacotes.
- Arquivos `.R` fora do pipeline `{targets}` e o diretório local `_targets/` ficam listados em `.renvignore`, para que `renv::snapshot()` não registre no `renv.lock` dependências que não são usadas pelo pipeline.
- Rodar/atualizar o pipeline inteiro: `targets::tar_make()`.
- Rodar/depurar um único alvo: `targets::tar_make(names = "nome_do_alvo")`.
- Inspecionar o grafo de dependências: `targets::tar_visnetwork()`; listar alvos: `targets::tar_manifest()`.
- Ler o resultado de um alvo já computado: `targets::tar_read(nome_do_alvo)`.
- Invalidar/forçar recomputo: `targets::tar_invalidate(nome_do_alvo)`; `targets::tar_destroy()` para limpar tudo.
- O *store* dos alvos (cache do `{targets}`) pode ficar fora do repositório, num diretório configurado em `_targets.yaml` (não versionado; sem ele, o padrão é `_targets/` na raiz).
- O caminho do `python-qgis.bat` (exportação dos layouts QGIS) vem da variável de ambiente `PYTHON_QGIS_BAT`, com fallback para a instalação padrão do OSGeo4W (ver `R/config.R`).
- Insumos congelados: as fontes que mudam com o tempo ficam em `dados/brutos/`, distribuídas no `insumos.zip` da release e listadas com SHA-256 em `dados/MANIFESTO_INSUMOS.csv`. `_targets.R` chama `verificar_insumos_congelados()` antes de qualquer alvo e para se algum faltar ou divergir. Toda função de importação de fonte que muda deve ler primeiro o arquivo em `dados/brutos/` (cache-then-fetch) e só baixar se ele não existir. Ao trocar um insumo congelado de propósito, regenere o manifesto e a release.
- `JURISDICOES_ATUALIZAR` (`nada`, `osrm`, `siorg`; ver `R/config.R` e README) define o que é refeito com dados atuais. As respostas do OSRM são separadas em insumo bruto (`extrair_respostas_osrm()`) e consolidação (`consolidar_resultados_osrm()`), que é sempre recalculada.
- Não há suíte de testes automatizados, linter ou CI configurados neste repositório.

## Arquitetura

### Organização das pastas

- `R/` — funções do pipeline; `_targets.R` (raiz) define os alvos.
- `dados/brutos/` e `dados/processados/` — downloads e intermediários (`.gpkg`, `.parquet`, `.duckdb`); não versionados.
- `dados/imputacoes/` — insumos manuais versionados: regras de imputação, imputações por setor, `cr_municipio.xlsx` (declaração de atendimento por CR) e dicionário de CRs.
- `documentos/` — fontes Quarto (`nt_jurisdicoes.qmd`, `relatorio_cr.qmd`), bibliografia, CSS e imagens. A apresentação para as CRs (`apresentacao_crs.qmd` e o template em `templates/`) é de uso interno, fica fora do git (`.gitignore`) e só é renderizada quando o arquivo existe localmente. O `_quarto.yml` da raiz faz os documentos executarem a partir da raiz (para `targets::tar_read()` e `source("R/...")` funcionarem); por isso imagens incluídas com `knitr::include_graphics()` precisam de caminho absoluto (`normalizePath()`).
- `qgis/` — projeto QGIS versionado, cujas camadas apontam para `dados/`.
- `saidas/` — tudo o que o pipeline gera para leitura humana, não versionado: `figuras/`, `tabelas/`, `relatorios_cr/`, `documentos/` (documentos Quarto renderizados) e `logs/`. Os diretórios vêm das constantes `DIR_*` de `R/config.R`; nunca grave saídas na raiz.
- `local/` — arquivos de trabalho pessoais, ignorados pelo git.

### Ordem de carregamento (`_targets.R`)

`tar_source()` carrega os scripts de `R/` nesta ordem, que importa para onde adicionar código novo:

1. `R/config.R` — constantes compartilhadas (CRS, diretórios, parâmetros da API OSRM, URLs de download, nomes de arquivos manuais). Sempre fonteado primeiro.
2. `R/importando_dados.R` — funções de ingestão (WFS, shapefiles, SIORG, SIDRA, Censo).
3. `R/siorg_geocode.R`, `R/gera_graficos_relatorio.R`, `R/define_divisoes_cr.R`, `R/visualizar_resultados.R`, `R/geoprocessamento.R`, `R/funcoes_imputacao.R`, `R/relatorio_cr.R`, `R/imputacao_norte_duckdb.R`.

Qualquer constante nova, URL de geoserver ou nome de arquivo manual deve ir para `R/config.R`, nunca hardcoded direto em `_targets.R` ou nas funções — é o único lugar fonteado antes de tudo o mais e evita duplicação de literais entre scripts.

### Fluxo do pipeline (três etapas)

1. **Ingestão de dados brutos** (`R/importando_dados.R`): downloads de malhas do IBGE (municípios, distritos, setores censitários), Censo 2022, WFS da FUNAI/IBGE (TIs, aldeias, aeroportos, hidrovias, drenagem), SIORG, SIAPE. A maioria dos alvos WFS é gerada dinamicamente via `tar_map(values = URLS_WFS, ...)` a partir da tabela de endpoints em `R/config.R`.
2. **Geoprocessamento e roteamento** (`R/geoprocessamento.R` + `R/define_divisoes_cr.R`): cálculo de rotas via API OSRM entre pontos de origem (CRs) e destino (setores/municípios), seguido de correção topológica (`corrigindo_topologia()`, `corrigir_enclaves_municipais()`) para eliminar furos/ilhas na divisão territorial resultante.
3. **Imputação e relatório** (`R/funcoes_imputacao.R`, `R/gera_graficos_relatorio.R`, `R/visualizar_resultados.R`): consolidação do atendimento por CR combinando critérios programáticos, manuais e por Terra Indígena; geração de mapas (`criar_mapa_final()`) e gráficos/tabelas para a nota técnica (`documentos/nt_jurisdicoes.qmd`).

### Padrão de versões sucessivas (v1 → v2 → v2_msol → mun)

Vários alvos existem em versões encadeadas: `arquivo_gpkg_imputado_final`/`arquivo_gpkg_suavizado` (v1) → `_v2` → `_v2_msol` → `_mun` (`arquivo_gpkg_imputado_final_mun`, `arquivo_gpkg_suavizado_mun_final`). Cada versão não substitui a anterior — ela parte do resultado suavizado da versão anterior e aplica uma nova rodada de regras:
- **v2**: realocação de setores (`sf_setores_realocados_v2`) e imputações da etapa `v2` (`df_imputacao_v2`).
- **v2_msol**: imputações da etapa `msol`, proposta do GT CR-MSOL (`df_imputacao_msol`).
- **mun**: imputação por município (`df_imputacao_mun`, algorítmica) e exclusividade municipal.

### Regras de imputação

Toda decisão manual do tipo "este território fica com aquela CR" fica em `dados/imputacoes/regras_imputacao.csv`, uma linha por regra, com justificativa e fonte (formato descrito em `dados/imputacoes/README.md`). A coluna `etapa` diz em qual versão a regra é aplicada (`v2`, `msol` ou `malha_basica`), e `prioridade`/`ordem` definem a precedência quando um setor casa com mais de uma regra. As imputações manuais setor a setor ficam em `dados/imputacoes/imputacoes_setor.csv`. O motor que aplica a tabela está em `R/funcoes_imputacao.R` (`aplicar_regras_imputacao()`). Nunca fixe códigos de CR, município, TI ou setor direto no código para criar uma imputação: acrescente uma linha na tabela.

Ao investigar ou alterar uma regra de imputação, confirme em qual etapa ela é aplicada antes de editar — editar a etapa errada não propaga para o resultado final (`arquivo_final_sc_mun`, `tabela_dados_final`).

### Sistemas de referência de coordenadas (CRS)

Definidos como constantes em `R/config.R`:
- `CRS_EXTRACAO` (EPSG:4674, SIRGAS 2000) — usado na extração/leitura dos dados brutos.
- `CRS_GEOPROCESSAMENTO` (EPSG:4326, WGS 84) — padrão de entrada/leitura para operações de geoprocessamento.
- `CRS_ALBERS_BR` (EPSG:10857, SIRGAS 2000 / Brasil Polyconic) — padrão do projeto para cálculos de área/distância em escala nacional.

## Regras de código (R)

- Use o pipe nativo do R (`|>`); nunca use o pipe do magrittr (`%>%`).
- Prefira `dplyr`/`tidyr` para manipulação de dados, com namespace explícito quando a função não for do R base (ex.: `dplyr::select`, `sf::st_transform`).
- Evite loops `for` tradicionais em bases grandes; priorize vetorização, `apply` ou `purrr::map`.
- Funções em `R/` devem ser puras (sem side-effects): recebem inputs, processam, e retornam um objeto (`data.frame`, `sf`, etc.) ou o caminho de um arquivo gerado.
- Chamadas a APIs externas (ex.: OSRM) devem sempre ter fallback, tratamento de erro (`tryCatch`) e logs.

## Armadilhas conhecidas

- **Coluna de geometria dos objetos `sf` não se chama `geometry`.** Vários `.gpkg` deste projeto (ex.: `arq_tis`, os setores/CRs produzidos por `unir_poligonos()`) têm a coluna de geometria nomeada `geom`, não `geometry`. Código como `dplyr::mutate(area = sf::st_area(geometry))` ou `sf::st_area(geometry)` dentro de um `mutate()` falha com `objeto 'geometry' não encontrado` (ou, pior, silenciosamente usa a coluna errada se por acaso existir uma variável `geometry` no escopo). O mesmo problema ocorre após `sf::st_intersection(x, y)`: o nome da coluna de geometria do resultado nem sempre é prevísivel. Nunca hardcode o nome da coluna de geometria — sempre passe o próprio objeto `sf` para `sf::st_area()`/`sf::st_length()` (ex.: `dplyr::mutate(area = as.numeric(sf::st_area(meu_objeto)))`, quebrando o pipe em duas atribuições se necessário para ter uma referência ao objeto).
- **`cd_uorg` (código numérico da CR) deve ser sempre atualizado em conjunto com `nome_uorg` e `sigla`.** Várias etapas do pipeline reatribuem `cd_uorg` de setores/municípios programaticamente (`corrigindo_topologia()` e `corrigir_enclaves_municipais()`/`reforcar_exclusividade_municipal()` em `R/geoprocessamento.R`, ao resolver enclaves/ilhas administrativas; `imputar_atendimento()` e `imputar_atendimento_declarado()` em `R/define_divisoes_cr.R`, ao aplicar imputações manuais). Se apenas `cd_uorg` for sobrescrito sem sincronizar `nome_uorg`/`sigla` com o novo código, o mesmo `cd_uorg` numérico passa a aparecer no arquivo final com dois nomes/siglas de CR diferentes (ex.: `cd_uorg` da CR de Manaus reaproveitado por uma linha rotulada "Coordenação Regional Madeira"). Isso não é pego por `verificar_exclusividade_municipal()` (que só checa unicidade de `cd_uorg` por município) nem pelo QA equivalente a nível de setor — só aparece como linhas fantasma duplicadas em relatórios que agrupam por `sigla`/`nome_uorg` (ex.: `tabela_dados_final`, `criar_mapa_divergencia_ti_sii()`). Ao escrever qualquer código que reatribua `cd_uorg`, sempre atualize `nome_uorg` e `sigla` na mesma operação, de preferência a partir de um lookup canônico `cd_uorg -> (nome_uorg, sigla)` construído a partir dos dados de origem (ex.: `sf_pontos_filtrados`, ou o rótulo mais frequente por `cd_uorg` nos próprios dados de entrada da função).

## Otimização de processamento e memória

As geometrias são pesadas (escala Brasil inteiro) e o pipeline roda com `memory = 'transient'` (um worker local, `crew_controller_local(workers = 1)`), então cada alvo é descartado da memória assim que não é mais necessário — o código de cada função deve ser econômico dentro desse ciclo de vida curto:

- Use `sf::st_drop_geometry()` antes de joins/agregações `dplyr` que não precisam de geometria.
- Projete o CRS uma única vez, no início da função/alvo (usando as constantes de `R/config.R`), e reaproveite o objeto já projetado — evite `st_transform()` repetido em funções auxiliares.
- Ao ler um GeoPackage grande, prefira `sf::st_read(..., query = "SELECT col1, col2 FROM tabela")` a carregar a tabela de atributos inteira quando só um subconjunto de colunas é necessário.
- Evite `st_make_valid()` redundante e use índices espaciais do `sf` (`st_intersects`, `st_join`) em vez de loops manuais sobre feições.
- Em loops que iteram sobre muitas geometrias (ex.: correção topológica), chame `gc()` periodicamente para conter o pico de memória.
