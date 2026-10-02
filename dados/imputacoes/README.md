# Imputações manuais

O algoritmo de roteamento atribui cada setor censitário à CR mais acessível. Parte dessas atribuições foi corrigida por decisão da gestão: vinculações institucionais vigentes, questões interétnicas, adstrição de uma Terra Indígena a uma só CR e propostas de GTs. Todas essas decisões ficam registradas nesta pasta.

| Arquivo | Conteúdo |
|---|---|
| `regras_imputacao.csv` | Uma linha por decisão de vinculação (território → CR), com justificativa e fonte |
| `imputacoes_setor.csv` | Imputações feitas setor a setor, elaboradas pela CGGE com as CRs |
| `dicionario_cr.rds` | Dicionário de CRs (códigos SIORG, SIAPE, UG e siglas) |

Os CSVs usam `;` como separador e codificação UTF-8.

## `regras_imputacao.csv`

| Coluna | Descrição |
|---|---|
| `etapa` | Versão do pipeline em que a regra se aplica: `v2`, `msol` (proposta do GT CR-MSOL) ou `malha_basica` (ajustes sobre a malha por TI e município) |
| `prioridade` | Quando um setor casa com regras de prioridades diferentes, vale a de menor número |
| `ordem` | Desempate dentro da mesma prioridade: vale a de menor número. Regras com a mesma `ordem` formam um grupo |
| `tipo` | Tipo de território (ver abaixo) |
| `codigo` | Código do território: UF ou município (IBGE), TI (`terrai_codigo` da Funai), setor censitário, hidrovia (`id` em `arquivos_hidroviario.gpkg`) |
| `descricao` | Nome do território, só para leitura |
| `somente_se_cr_atual` | Aplica a regra só aos setores hoje atribuídos a uma dessas CRs (códigos separados por vírgula). Vazio = sem condição |
| `exceto_se_cr_atual` | Não aplica a regra aos setores hoje atribuídos a uma dessas CRs. Vazio = sem condição |
| `cd_uorg_destino` | Código SIORG da CR que recebe o território |
| `sigla_destino` | Sigla da CR de destino, só para leitura (o pipeline avisa se divergir do dicionário) |
| `justificativa` | Motivo da decisão |
| `fonte` | Processo SEI, reunião ou documento que registra a decisão |

### Tipos de regra

- `uf`, `municipio`, `setor`: todos os setores do território vão para `cd_uorg_destino`, respeitando as condições de CR atual.
- `ti`: os setores associados à TI (alvo `df_setores_em_ti`). Se um setor toca mais de uma TI, prevalece a relação mais forte com a TI e, em empate, a menor `ordem`.
- `hidrovia`: os setores cortados pela hidrovia.
- `arquivo_setores`: aplica `imputacoes_setor.csv` naquela prioridade.
- `calculado`: atribuição calculada no código, como a melhor rota entre CRs candidatas ou a realocação dos setores restantes. A linha registra a posição dessa etapa na precedência e explica o que ela faz. O cálculo fica em `R/funcoes_imputacao.R` e é chamado em `_targets.R`.

Na etapa `malha_basica`, as regras valem por território inteiro (TI ou município) e se sobrepõem ao resultado algorítmico da malha básica. Um município sem setores censitários na malha de área de atuação, como um município instalado depois dela, só entra na malha por uma regra dessa etapa.

## Como acrescentar uma imputação

1. Acrescente uma linha em `regras_imputacao.csv` na etapa certa, preenchendo `justificativa` e `fonte`.
2. Para imputar setores avulsos em grande número, acrescente-os em `imputacoes_setor.csv` (colunas `cd_setor`, `cd_uorg_destino`, `lote`).
3. Rode `targets::tar_make()`. O `{targets}` detecta a alteração e recalcula só o que depende dela.
