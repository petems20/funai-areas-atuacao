# Áreas de atuação das Coordenações Regionais da Funai

Pipeline em R que define a área de atuação de cada Coordenação Regional (CR) da Fundação Nacional dos Povos Indígenas (Funai) sobre o território brasileiro. Cada setor censitário do IBGE é atribuído à CR mais acessível por rota rodoviária ou multimodal, o resultado passa por correção topológica para eliminar enclaves e, por fim, recebe as imputações decididas pela gestão (vinculações vigentes no SII, Terras Indígenas atendidas por uma única CR, propostas de grupos de trabalho). A versão final é consolidada por município e por Terra Indígena.

O pipeline produz os polígonos de atuação de cada CR, a nota técnica que apresenta a proposta e um relatório em PDF por CR.

## Como reproduzir

Requisitos: R 4.5.0, [Quarto](https://quarto.org) e, para exportar os mapas do projeto QGIS, o QGIS instalado pelo OSGeo4W.

1. Clone o repositório e baixe, na página de *releases*, o arquivo `insumos.zip`. Descompacte-o na raiz do projeto: ele preenche `dados/brutos/` com os insumos usados na versão publicada.
2. No R, a partir da raiz do projeto:

```r
renv::restore()        # instala os pacotes nas versões do renv.lock
targets::tar_make()    # roda o pipeline
```

O pacote `indigenR` vem do GitHub e às vezes falha no `renv::restore()` por exigir autenticação. Nesse caso, instale-o com `remotes::install_github("petems20/indigenR")` e rode `renv::restore()` de novo.

A orquestração usa o pacote [`{targets}`](https://docs.ropensci.org/targets/): `targets::tar_visnetwork()` mostra o grafo de dependências e `targets::tar_read(nome_do_alvo)` lê qualquer resultado intermediário. O cache do `{targets}` fica, por padrão, em `_targets/`; para guardá-lo em outro lugar, crie um `_targets.yaml` com `targets::tar_config_set(store = "caminho")`. Se o QGIS não estiver no local padrão do OSGeo4W, defina a variável de ambiente `PYTHON_QGIS_BAT` no `.Renviron` com o caminho do `python-qgis.bat`.

### Insumos congelados

Várias fontes mudam com o tempo (o geoserver da Funai, o SIORG, o Cadastro Único, o servidor de rotas), e algumas não têm API pública (SIOP, SIASI, BDQueimadas). Para que as malhas intermediárias e finais saiam idênticas às publicadas, essas fontes foram congeladas no `insumos.zip`, que traz também as respostas do servidor de rotas (OSRM) e a geocodificação dos endereços das CRs. O arquivo [`dados/MANIFESTO_INSUMOS.csv`](dados/MANIFESTO_INSUMOS.csv) lista cada insumo com sua fonte e seu SHA-256. Antes de rodar qualquer etapa, o pipeline confere esses arquivos e para se algum faltar ou divergir.

Fontes com versão fixa (Censo Demográfico 2022, malhas territoriais e tabelas do IBGE, bases da ANTAQ) são baixadas pelo próprio pipeline. Também são baixadas na hora a malha viária e hidroviária do OpenStreetMap, usada no roteamento multimodal da região Norte, a camada de rodovias federais da INDE, que só aparece nos mapas, e a tabela de valores dos cargos comissionados, consultada na API do SIORG. A malha do OpenStreetMap é atualizada continuamente; por isso as atribuições decididas pelo roteamento multimodal (setores do Amazonas sem rota rodoviária e a realocação de setores da CR Médio Solimões) podem diferir pontualmente das publicadas. Gráficos e tabelas da nota técnica também podem ter diferenças pequenas, como a posição de rótulos nos mapas.

### Atualizar insumos

A variável de ambiente `JURISDICOES_ATUALIZAR`, definida no `.Renviron`, permite refazer parte do processo com dados atuais. Os insumos atualizados são gravados em `dados/processados/`, e os congelados de `dados/brutos/` não mudam.

| Valor | O que é refeito |
|---|---|
| `nada` (padrão) | nada; reproduz a versão publicada |
| `osrm` | as requisições ao servidor público do OSRM, com as mesmas CRs. Levam muitas horas, e o resultado pode variar porque a malha viária do servidor muda |
| `siorg` | baixa a estrutura atual do SIORG, geocodifica de novo os endereços das CRs e refaz as requisições ao OSRM. Incorpora CRs criadas depois da publicação e pode mudar bastante o resultado. CRs novas não constam do dicionário de CRs nem das regras de imputação |

As requisições ao OSRM são feitas em lotes, e um lote já respondido não é pedido de novo se a execução for interrompida. Para refazer requisições já concluídas, use `targets::tar_invalidate(resultados_api_brutos)`. Para baixar das fontes todos os demais insumos, sem a verificação do manifesto, defina `JURISDICOES_INSUMOS=livre`.

## Organização

```
_targets.R          definição do pipeline
R/                  funções (R/config.R concentra constantes, caminhos e URLs)
dados/imputacoes/   insumos manuais versionados
dados/brutos/       insumos externos: os congelados (insumos.zip) e os baixados pelo pipeline (não versionado)
dados/processados/  resultados intermediários em GeoPackage, Parquet e DuckDB (não versionado)
documentos/         fontes Quarto da nota técnica e do relatório por CR
qgis/               projeto QGIS com os layouts dos mapas
saidas/             figuras, tabelas, documentos renderizados, relatórios por CR e logs (não versionado)
```

## Imputações manuais

Toda decisão de vincular um território a uma CR que contraria ou complementa o resultado do roteamento está em `dados/imputacoes/regras_imputacao.csv`, uma linha por decisão, com justificativa e fonte. As imputações feitas setor a setor estão em `dados/imputacoes/imputacoes_setor.csv`. O formato dos dois arquivos e a ordem de precedência entre regras estão descritos em [`dados/imputacoes/README.md`](dados/imputacoes/README.md).

O arquivo `dados/imputacoes/cr_municipio.xlsx` registra quais municípios e Terras Indígenas cada CR declara atender hoje. Ele foi montado a partir de duas fontes: os ofícios e despachos das CRs reunidos no processo SEI 08620.010286/2021-89 e as páginas das CRs no site institucional da Funai. A aba `UORG_MUN` traz as combinações CR-município e a aba `UORG_TI`, as combinações CR-Terra Indígena. A coluna `sei` indica o documento de origem de cada linha (`0` para o site institucional). Na extração dos documentos do processo, a CR emissora foi identificada pelo cabeçalho do ofício, porque algumas unidades reaproveitaram modelos de outras CRs e deixaram o nome antigo no corpo do texto. Municípios foram conferidos contra a base do IBGE, e menções que não casaram com nenhum município não entraram na planilha.

## Licença

Código distribuído sob a licença MIT (ver [`LICENSE`](LICENSE)). Os dados de terceiros seguem as condições de uso das respectivas fontes.
