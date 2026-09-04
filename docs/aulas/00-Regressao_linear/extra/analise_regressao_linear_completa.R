# =============================================================
# REGRESSÃO LINEAR - Banco AMES
# =============================================================
#
# Objetivo: identificar quais características dos imóveis estão
#   associadas ao preço de venda?
#
# Variáveis:
#
# Sale_Price       → variável resposta
#
# Gr_Liv_Area      → área habitável acima do solo
# Overall_Cond     → qualidade geral
# Year_Built       → ano de construção
# Year_Remod_Add   → ano da reforma
# Garage_Cars      → capacidade da garagem
# Garage_Area      → área da garagem
# Total_Bsmt_SF    → área total do porão
# First_Flr_SF     → área do primeiro andar
# Full_Bath        → número de banheiros completos
# Bedroom_AbvGr    → quartos acima do nível do solo
# TotRms_AbvGrd    → número de cômodos
# Lot_Area         → área do terreno
# Lot_Frontage     → frente do terreno
# Neighborhood     → bairro
#
# =============================================================

# =============================================================
# 1. Pacotes
# =============================================================

library(tidyverse)
library(modeldata)
library(car)
library(lmtest)
library(sandwich)
library(MASS)
library(broom)
library(skimr)


# =============================================================
# 2. Carregar banco de dados
# =============================================================

data(ames)

glimpse(ames) # Visualizar o banco

# Variaveis que vamos utilizar
dados <- ames |>
  dplyr::select(
    Sale_Price,
    Gr_Liv_Area,
    Overall_Cond,
    Year_Built,
    Year_Remod_Add,
    Garage_Cars,
    Garage_Area,
    Total_Bsmt_SF,
    First_Flr_SF,
    Full_Bath,
    Bedroom_AbvGr,
    TotRms_AbvGrd,
    Lot_Area,
    Lot_Frontage,
    Neighborhood
  )

# =============================================================
# 3. Conhecer o banco
# =============================================================

# 3.1. Estrutura

dim(dados) # numero de observacoes e de variaveis

glimpse(dados) # observar o tipo de cada variavel
# Variáveis categóricas devem ser fatores

# 3.2. Estatísticas descritivas

summary(dados) # Procurar por
#  - valores mínimos;
#  - valores máximos;
#  - mediana;
#  - quartis;
#  - possíveis valores estranhos;
#  - NA.

# 3.3. Percentual de NAs

skim(dados) # Se houver poucos, excluir as linhas
# Pode-se também imputar com MICE, por exemplo

# ============================================================
# 4. Distribuicao da variavel resposta
# ============================================================

# 4.1. Histograma
# Obter os breaks calculados pelo hist()
h <- hist(dados$Sale_Price, plot = FALSE)

# Histograma no ggplot2 usando os mesmos breaks
ggplot(dados, aes(x = Sale_Price)) +
  geom_histogram(
    breaks = h$breaks,
    fill = "#2F6DAE",
    color = "white"
  ) +
  labs(
    x = "Sale Price",
    y = "Frequência"
  ) +
  theme_minimal()

# 4.2. Boxplot

ggplot(dados, aes(y = Sale_Price)) +
  geom_boxplot(
    fill = "#2F6DAE",
    color = "black",
    width = 0.35
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Sale Price"
  )


# ============================================================
# 5. Correlacao entre preditores
# ============================================================

correlacoes <- dados |>
  dplyr::select(
    Gr_Liv_Area,
    Year_Built,
    Year_Remod_Add,
    Garage_Cars,
    Garage_Area,
    Total_Bsmt_SF,
    First_Flr_SF,
    Full_Bath,
    Bedroom_AbvGr,
    TotRms_AbvGrd,
    Lot_Area,
    Lot_Frontage
  ) |>
  cor()

round(correlacoes, 2)

# Algums variaveis tem correlacao alta.
# Talvez retiremos uma delas do modelo depois

# ============================================================
# 9. MODELO
# ============================================================

# 9.1. Modelo inicial
modelo_full <- lm(
  Sale_Price ~
    Gr_Liv_Area +
    Overall_Cond +
    Year_Built +
    Year_Remod_Add +
    Garage_Cars +
    Garage_Area +
    Total_Bsmt_SF +
    First_Flr_SF +
    Full_Bath +
    Bedroom_AbvGr +
    TotRms_AbvGrd +
    Lot_Area +
    Lot_Frontage +
    Neighborhood,
  data = dados
)

summary(modelo_full)

confint(modelo_full) # intervalos de confianca


# 9.2. Usando stepwise para selecionar variáveis

modelo_AIC <- stepAIC(modelo_full)

summary(modelo_AIC)

# 9.3. Comparar AIC e BIC

AIC(modelo_full, modelo_AIC) # quanto menor, melhor
BIC(modelo_full, modelo_AIC) # quanto menor, melhor

# Como todas as variaveis do modelo_AIC são significativas,
# nao vamos remover nenhuma e modelo_AIC é nosso candidato

# ============================================================
# 10. Diagnóstico
# ============================================================

par(mfrow = c(2, 2))
plot(modelo_AIC)
par(mfrow = c(1, 1))

# Os quatro gráficos:
#
# 1. Residuals vs Fitted
#    - Os pontos devem ser aleatórios.
#    - Se tiver algum padrão, existe alguma estrutura que o modelo
#      não está capturando adequadamente.
#    - Se aparecer uma CURVA:
#      → possível não linearidade;
#      → revisar a forma funcional;
#      → considerar transformação;
#      → considerar termo quadrático;
#      → considerar interação.
#    - Se aparecer um FUNIL:
#      → possível heterocedasticidade;
#      → investigar transformação;
#      → considerar erros-padrão robustos.
#    - Se aparecerem GRUPOS:
#      → pode haver variável omitida;
#      → investigar fatores/categorias.
#    - Se aparecerem PONTOS MUITO AFASTADOS:
#      → investigar observações extremas/influentes.
#
# 2. Normal Q-Q
#    - Os residuos devem ser aproximadamente normais
#    - Se nao estiverem sob a reta do qqplot, transformar
#      a variavel resposta
# 3. Scale-Location
# 4. Residuals vs Leverage

# Vamos utilizar o modelo com log da variavel resposta para ver se melhora

# 10.1. Modelo com log(Sale_Price)
modelo_log <- lm(
  log(Sale_Price) ~
    Gr_Liv_Area +
    Overall_Cond +
    Year_Built +
    Year_Remod_Add +
    Garage_Cars +
    Garage_Area +
    Total_Bsmt_SF +
    First_Flr_SF +
    Full_Bath +
    Bedroom_AbvGr +
    TotRms_AbvGrd +
    Lot_Area +
    Lot_Frontage +
    Neighborhood,
  data = dados
)

modelo_log <- stepAIC(modelo_log)

AIC(modelo_AIC, modelo_log)
BIC(modelo_AIC, modelo_log)

summary(modelo_log)

confint(modelo_log) # intervalos de confianca

par(mfrow = c(2, 2))
plot(modelo_log)
par(mfrow = c(1, 1))


# ============================================================
# 11. MULTICOLINEARIDADE
# ============================================================

car::vif(modelo_log)


# ============================================================
# 12. OBSERVAÇÕES INFLUENTES
# ============================================================

# ------------------------------------------------------------
# Distância de Cook
# ------------------------------------------------------------

plot(
  modelo_log,
  which = 4
)

# Identificar as maiores:

cooks <- cooks.distance(
  modelo_candidato
)

sort(
  cooks,
  decreasing = TRUE
) |>
  head(15)


# ------------------------------------------------------------
# Leverage
# ------------------------------------------------------------

lev <- hatvalues(
  modelo_log
)

sort(
  lev,
  decreasing = TRUE
) |>
  head(15)


# ------------------------------------------------------------
# DFBETAs
# ------------------------------------------------------------

dfbetas(
  modelo_candidato
)


# ------------------------------------------------------------
# Diagnóstico conjunto
# ------------------------------------------------------------

car::influencePlot(
  modelo_candidato
)

# O que investigar?
#
# Observações com:
#
# - resíduos grandes;
# - leverage alto;
# - Cook elevado.
#
# Uma observação influente NÃO é necessariamente um erro.
#
# Primeiro investigar a origem da observação.

# ============================================================
# 13. ANÁLISE DE SENSIBILIDADE
# ============================================================

# Se encontrarmos uma observação potencialmente influente,
# podemos comparar o modelo com e sem ela.
#
# Vamos identificar, por exemplo, a observação mais influente:

indice_influente <- which.max(
  cooks.distance(modelo_log)
)

indice_influente

dados[indice_influente, ]


# Modelo sem essa observação:

modelo_sem_influente <- update(
  modelo_log,
  data = dados[-indice_influente, ]
)

# Comparar coeficientes:

coef(modelo_log)

coef(modelo_sem_influente)


# Comparar intervalos:

confint(modelo_candidato)

confint(modelo_sem_influente)

# Pergunta:
#
# "A conclusão substantiva muda?"
#
# Se mudar muito:
#
# → documentar a influência;
# → investigar a origem;
# → considerar análise de sensibilidade;
# → discutir a robustez das conclusões.
#
# Não simplesmente excluir porque "atrapalhou".

ggplot(
  augment(modelo_AIC),
  aes(
    x = Gr_Liv_Area,
    y = .resid
  )
) +
  geom_point(alpha = 0.4) +
  geom_hline(
    yintercept = 0
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  )


dados <- readxl::read_xlsx(
  "/dados/UFS/Disciplinas-Graduacao/2026.2/MLG/aulas/01-Regressao_linear/extra/vendas.xlsx"
)


# ============================================================
# 1. CONHECER O BANCO
# ============================================================

dim(dados) # Número de linhas e colunas

str(dados) # Tipo de cada variável

glimpse(dados) # Uma visão compacta do banco

summary(dados) # Descritivas das variáveis

names(dados) # Nomes das variáveis


# ------------------------------------------------------------
# Buscando duplicidades
# ------------------------------------------------------------

dados |>
  count(loja) |>
  filter(n > 1) # Verificando se uma loja aparece mais de uma vez.


# ============================================================
# 2. ADEQUAR O BANCO
# ============================================================

# ------------------------------------------------------------
# 2.1 Procurar valores impossíveis
# ------------------------------------------------------------

dados |>
  filter(
    vendas < 0 |
      publicidade < 0 |
      preco_medio < 0 |
      fluxo_clientes < 0 |
      renda_media < 0 |
      concorrentes < 0
  ) # Verificando se há valores negativos


# ------------------------------------------------------------
# 2.2 Quantidade de NA
# ------------------------------------------------------------

colSums(is.na(dados))

# O que podemos encontrar?
# ------------------------------------------------------------
# Neste exemplo:
# - NA em preco_medio;
# - NA em publicidade.
#
# Próximo passo:
# ------------------------------------------------------------
# Avaliar a magnitude e o impacto desses NA.

# ------------------------------------------------------------
# 2.3 Percentual de NA
# ------------------------------------------------------------

round(
  colSums(is.na(dados)) / nrow(dados) * 100,
  2
) # Valores em percentuais


# ------------------------------------------------------------
# 2.4 Criar banco apenas com linhas completas
# ------------------------------------------------------------

dados_modelo <- dados |>
  drop_na(
    vendas,
    publicidade,
    preco_medio,
    fluxo_clientes,
    renda_media,
    concorrentes
  )


# ------------------------------------------------------------
# 2.5 Comparar tamanho amostral
# ------------------------------------------------------------

nrow(dados) # amostra inicial

nrow(dados_modelo) # amostra para modelagem

nrow(dados) - nrow(dados_modelo) # número de linhas removidas


# ------------------------------------------------------------
# 2.6 Comparar quem entrou e quem ficou de fora
# ------------------------------------------------------------

dados |>
  mutate(
    completo = complete.cases(
      vendas,
      publicidade,
      preco_medio,
      fluxo_clientes,
      renda_media,
      concorrentes
    )
  ) |>
  group_by(completo) |>
  summarise(
    n = n(),
    media_vendas = mean(vendas, na.rm = TRUE),
    media_fluxo = mean(fluxo_clientes, na.rm = TRUE),
    media_renda = mean(renda_media, na.rm = TRUE)
  )


# Se houver diferenças importantes:
# ------------------------------------------------------------
# Isso pode indicar que simplesmente excluir casos completos
# pode alterar a composição da amostra.

# ============================================================
# 3. EXPLORAR AS VARIÁVEIS
# ============================================================

# ------------------------------------------------------------
# 3.1 Distribuição das vendas
# ------------------------------------------------------------

ggplot(dados, aes(x = vendas)) +
  geom_histogram(bins = 30)

# O que procurar?
# ------------------------------------------------------------
# - assimetria;
# - caudas;
# - valores extremos;
# - concentração de valores.
#
# Se houver assimetria:
# ------------------------------------------------------------
# Não transformar automaticamente.
# Investigar se uma transformação faz sentido para o problema.

ggplot(dados, aes(y = vendas)) +
  geom_boxplot()


# ------------------------------------------------------------
# 3.2 Distribuições dos preditores
# ------------------------------------------------------------

ggplot(dados, aes(x = publicidade)) +
  geom_histogram(bins = 30)

ggplot(dados, aes(x = preco_medio)) +
  geom_histogram(bins = 30)

ggplot(dados, aes(x = fluxo_clientes)) +
  geom_histogram(bins = 30)

ggplot(dados, aes(x = renda_media)) +
  geom_histogram(bins = 30)

ggplot(dados, aes(x = concorrentes)) +
  geom_histogram(bins = 10)

# O que estamos procurando?
# ------------------------------------------------------------
# Conhecer a distribuição de cada variável.
#
# Importante:
# ------------------------------------------------------------
# Não precisamos que os preditores sejam normalmente distribuídos
# para ajustar uma regressão linear.

# ============================================================
# 4. EXPLORAR AS RELAÇÕES
# ============================================================

# O objetivo desta etapa é entender os padrões antes de
# construir o modelo.
#
# Pergunta central:
#
# "Como Y se relaciona com cada possível X?"

# ------------------------------------------------------------
# 4.1 Publicidade × vendas
# ------------------------------------------------------------

ggplot(
  dados,
  aes(
    x = publicidade,
    y = vendas
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = TRUE
  )

# Possíveis resultados:
# ------------------------------------------------------------
# 1. Associação aproximadamente linear.
# 2. Curvatura.
# 3. Ausência aparente de associação.
# 4. Grupos.
# 5. Valores extremos influentes visualmente.
#
# O que fazer?
# ------------------------------------------------------------
# - Linearidade → regressão linear pode ser adequada.
# - Curvatura → investigar transformação ou termo não linear.
# - Grupos → verificar se existe variável categórica relevante.
# - Pontos extremos → investigar, mas não excluir automaticamente.

# ------------------------------------------------------------
# 4.2 Preço médio × vendas
# ------------------------------------------------------------

ggplot(
  dados,
  aes(
    x = preco_medio,
    y = vendas
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = TRUE
  )


# ------------------------------------------------------------
# 4.3 Fluxo de clientes × vendas
# ------------------------------------------------------------

ggplot(
  dados,
  aes(
    x = fluxo_clientes,
    y = vendas
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = TRUE
  )


# ------------------------------------------------------------
# 4.4 Renda × vendas
# ------------------------------------------------------------

ggplot(
  dados,
  aes(
    x = renda_media,
    y = vendas
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = TRUE
  )


# ------------------------------------------------------------
# 4.5 Concorrentes × vendas
# ------------------------------------------------------------

ggplot(
  dados,
  aes(
    x = factor(concorrentes),
    y = vendas
  )
) +
  geom_boxplot()

# O que estamos fazendo?
# ------------------------------------------------------------
# Como concorrentes é uma contagem com poucos valores distintos,
# o boxplot por categoria ajuda a visualizar diferenças entre
# níveis.
#
# Se a variável fosse categórica nominal:
# ------------------------------------------------------------
# O mesmo raciocínio poderia ser usado para comparar grupos.

# ============================================================
# 5. AJUSTAR O MODELO INICIAL e
# ============================================================

modelo1 <- lm(
  vendas ~
    publicidade +
    preco_medio +
    fluxo_clientes +
    renda_media +
    concorrentes,
  data = dados_modelo
)

summary(modelo1)


# ============================================================
# 6. SELEÇÃO DE VARIÁVEIS
# ============================================================

# Removendo a variável 'fluxo_clientes'
modelo2 <- lm(
  vendas ~
    publicidade +
    preco_medio +
    renda_media +
    concorrentes,
  data = dados_modelo
)

summary(modelo2)


# Removendo a variável 'concorrentes'
modelo3 <- lm(
  vendas ~
    publicidade +
    preco_medio +
    renda_media,
  data = dados_modelo
)

summary(modelo3)

# Removendo a variável 'renda_media'
modelo4 <- lm(
  vendas ~
    publicidade +
    preco_medio,
  data = dados_modelo
)

summary(modelo4)

confint(modelo4) # Intervalos de confiança

# COMPARAR AIC
AIC(
  modelo1,
  modelo2,
  modelo3,
  modelo4
)

# Comparar BIC
BIC(
  modelo1,
  modelo2,
  modelo3,
  modelo4
)


# ============================================================
# 7. DIAGNÓSTICO DO MODELO
# ============================================================

par(mfrow = c(2, 2))
plot(modelo4)
par(mfrow = c(1, 1))

# 1. Residuals vs Fitted:
#    - Esperamos resíduos distribuídos de maneira aproximadamente
#      aleatória ao redor de zero.
#    - Se aparecer algum padrão podemos transformar a variável
#      resposta ou
#
# 2. Normal Q-Q
#    → comportamento da distribuição dos resíduos
#
# 3. Scale-Location
#    → avaliação da homogeneidade da variância
#
# 4. Residuals vs Leverage
#    → observações potencialmente influentes
#
# Não basta olhar os quatro gráficos rapidamente.
# As próximas etapas detalham cada problema.

# ============================================================
# 10. FORMA FUNCIONAL / LINEARIDADE
# ============================================================

plot(
  modelo4,
  which = 1
)

# O que procurar?
# ------------------------------------------------------------
# Esperamos resíduos distribuídos de maneira aproximadamente
# aleatória ao redor de zero.
#
# Problemas:
# ------------------------------------------------------------
# - curva;
# - padrão sistemático;
# - tendência;
# - estrutura evidente.
#
# Se aparecer uma curvatura:
# ------------------------------------------------------------
# Investigar:
# - transformação;
# - termo quadrático;
# - interação;
# - outra especificação justificável.
#
# Não corrigir apenas "para melhorar o p-valor".

# ------------------------------------------------------------
# Resíduos versus cada preditor
# ------------------------------------------------------------

par(mfrow = c(2, 2))

plot(
  residuals(modelo_candidato) ~
    dados_modelo$publicidade,
  xlab = "Publicidade",
  ylab = "Resíduos"
)

plot(
  residuals(modelo_candidato) ~
    dados_modelo$preco_medio,
  xlab = "Preço médio",
  ylab = "Resíduos"
)

plot(
  residuals(modelo_candidato) ~
    dados_modelo$fluxo_clientes,
  xlab = "Fluxo de clientes",
  ylab = "Resíduos"
)

plot(
  residuals(modelo_candidato) ~
    dados_modelo$renda_media,
  xlab = "Renda média",
  ylab = "Resíduos"
)

par(mfrow = c(1, 1))


# ============================================================
# 11. INDEPENDÊNCIA
# ============================================================

# A independência não é algo que possa ser decidido apenas por
# um teste estatístico.
#
# Precisamos conhecer o desenho dos dados.
#
# Neste exemplo:
# - cada linha = uma loja;
# - cada loja aparece uma vez;
# - não temos medidas repetidas.
#
# Portanto, a independência é plausível.
#
# Em outros bancos, investigar:
# - tempo;
# - medidas repetidas;
# - indivíduos agrupados;
# - escolas;
# - hospitais;
# - empresas;
# - regiões.
#
# Se houver dependência:
# ------------------------------------------------------------
# Pode ser necessário outro tipo de modelo.

# ============================================================
# 12. HOMOCEDASTICIDADE
# ============================================================

plot(
  modelo_candidato,
  which = 1
)

# O que procurar?
# ------------------------------------------------------------
# Resíduos com dispersão aproximadamente constante.
#
# Problema típico:
# ------------------------------------------------------------
# "Funil":
# a variância aumenta conforme aumentam os valores ajustados.
#
# Neste exemplo:
# ------------------------------------------------------------
# Esperamos encontrar algum grau de heterocedasticidade porque
# o erro foi construído com desvio-padrão crescente com o fluxo.

# ------------------------------------------------------------
# Teste de Breusch-Pagan
# ------------------------------------------------------------

lmtest::bptest(modelo_candidato)

# Interpretação:
# ------------------------------------------------------------
# p pequeno pode fornecer evidência de heterocedasticidade.
#
# Mas:
# ------------------------------------------------------------
# Não usar o teste isoladamente.
# Avaliar também o gráfico e a magnitude do problema.

# ============================================================
# 13. O QUE FAZER SE HOUVER HETEROCEDASTICIDADE?
# ============================================================

# Possibilidade 1:
# ------------------------------------------------------------
# Revisar a forma funcional.

# Possibilidade 2:
# ------------------------------------------------------------
# Transformar a variável resposta ou um preditor, se houver
# justificativa substantiva/estatística.

# Possibilidade 3:
# ------------------------------------------------------------
# Usar erros-padrão robustos quando o objetivo é fazer inferência
# sobre os coeficientes mantendo a especificação do modelo.

coeftest(
  modelo_candidato,
  vcov = vcovHC(
    modelo_candidato,
    type = "HC3"
  )
)

# Importante:
# ------------------------------------------------------------
# Erro-padrão robusto não corrige uma relação média mal especificada.
# Se houver curvatura, precisamos tratar a forma funcional.

# ============================================================
# 14. DISTRIBUIÇÃO DOS RESÍDUOS
# ============================================================

# ------------------------------------------------------------
# Q-Q plot
# ------------------------------------------------------------

qqnorm(
  residuals(modelo_candidato)
)

qqline(
  residuals(modelo_candidato)
)

# O que procurar?
# ------------------------------------------------------------
# Pontos aproximadamente próximos da linha.
#
# Problemas possíveis:
# ------------------------------------------------------------
# - caudas muito diferentes;
# - assimetria;
# - observações extremas.
#
# O que fazer?
# ------------------------------------------------------------
# Avaliar magnitude e causa dos desvios.
# Não exigir normalidade perfeita.

# ------------------------------------------------------------
# Histograma dos resíduos
# ------------------------------------------------------------

ggplot(
  tibble(
    residuo = residuals(modelo_candidato)
  ),
  aes(x = residuo)
) +
  geom_histogram(bins = 30)

# O histograma é complementar ao Q-Q plot.

# ------------------------------------------------------------
# IMPORTANTE: não transformar o diagnóstico em um teste mecânico
# ------------------------------------------------------------
#
# Evitar:
#
# shapiro.test(residuals(modelo_candidato))
#
# como único critério para decidir se o modelo "passou" ou "falhou".
#
# O tamanho da amostra, a magnitude do desvio, os gráficos e o
# objetivo da análise devem ser considerados.

# ============================================================
# 15. MULTICOLINEARIDADE
# ============================================================

# ------------------------------------------------------------
# Correlação entre preditores
# ------------------------------------------------------------

dados_modelo |>
  select(
    publicidade,
    preco_medio,
    fluxo_clientes,
    renda_media,
    concorrentes
  ) |>
  cor()

# O que procurar?
# ------------------------------------------------------------
# Correlações elevadas entre preditores.
#
# Se houver:
# ------------------------------------------------------------
# Investigar se as variáveis representam informações muito
# semelhantes.

# ------------------------------------------------------------
# VIF
# ------------------------------------------------------------

car::vif(modelo_candidato)

# Interpretação:
# ------------------------------------------------------------
# VIF elevado indica possível problema de multicolinearidade.
#
# Possíveis consequências:
# - erros-padrão elevados;
# - coeficientes instáveis;
# - dificuldade para separar efeitos individuais.
#
# O que fazer?
# ------------------------------------------------------------
# Não retirar automaticamente a variável.
#
# Investigar:
# - significado das variáveis;
# - objetivo da análise;
# - estabilidade dos coeficientes;
# - possibilidade de redundância.

# ============================================================
# 16. OBSERVAÇÕES INFLUENTES
# ============================================================

# ------------------------------------------------------------
# Distância de Cook
# ------------------------------------------------------------

plot(
  modelo_candidato,
  which = 4
)

# O que procurar?
# ------------------------------------------------------------
# Observações com influência potencialmente elevada.
#
# Não existe uma única linha de corte que resolva a decisão.
# Valores elevados indicam que devemos investigar a observação.

# ------------------------------------------------------------
# Valores de Cook
# ------------------------------------------------------------

cook <- cooks.distance(modelo_candidato)

sort(
  cook,
  decreasing = TRUE
) |>
  head(10)


# ------------------------------------------------------------
# Leverage
# ------------------------------------------------------------

leverage <- hatvalues(modelo_candidato)

sort(
  leverage,
  decreasing = TRUE
) |>
  head(10)

# O que significa?
# ------------------------------------------------------------
# Leverage mede o quanto uma observação é incomum em relação aos
# valores dos preditores.
#
# Uma observação pode ter alto leverage sem necessariamente ser
# muito influente.

# ------------------------------------------------------------
# DFBETAs
# ------------------------------------------------------------

dfbetas(modelo_candidato)

# O que estamos avaliando?
# ------------------------------------------------------------
# Quanto cada observação pode alterar cada coeficiente.

# ------------------------------------------------------------
# Diagnóstico conjunto
# ------------------------------------------------------------

car::influencePlot(modelo_candidato)

# O que procurar?
# ------------------------------------------------------------
# Observações que combinam:
# - resíduos elevados;
# - leverage elevado;
# - influência elevada.

# ============================================================
# 17. INVESTIGAR A OBSERVAÇÃO EXTREMA
# ============================================================

dados_modelo |>
  filter(loja == 17)

# A pergunta é:
#
# "A loja 17 é um erro ou uma observação real?"
#
# Neste exemplo, suponha que:
# ------------------------------------------------------------
# a venda excepcional seja verdadeira e esteja associada a uma
# campanha promocional extraordinária.
#
# Portanto:
# ------------------------------------------------------------
# NÃO devemos simplesmente excluir a observação.

# ============================================================
# 18. ANÁLISE DE SENSIBILIDADE
# ============================================================

# Mesmo sendo uma observação real, podemos avaliar quanto ela
# influencia os resultados.

modelo_sem_17 <- lm(
  formula(modelo_candidato),
  data = dados_modelo |>
    filter(loja != 17)
)

# Comparar os coeficientes:

coef(modelo_candidato)

coef(modelo_sem_17)

# Comparar os intervalos:

confint(modelo_candidato)

confint(modelo_sem_17)

# O que estamos perguntando?
# ------------------------------------------------------------
# "A conclusão substantiva muda quando essa observação é retirada?"
#
# Se mudar muito:
# ------------------------------------------------------------
# A influência da observação precisa ser explicitamente discutida.
#
# Se não mudar substancialmente:
# ------------------------------------------------------------
# A conclusão é mais robusta à presença dessa observação.

# ============================================================
# 19. REVISAR E REAJUSTAR
# ============================================================

# Esta é a etapa que transforma a análise em um CICLO.
#
# Se o diagnóstico indicar:
#
# - não linearidade → revisar forma funcional;
# - heterocedasticidade → considerar transformação ou erros robustos;
# - dependência → considerar estrutura de dependência apropriada;
# - multicolinearidade → investigar variáveis redundantes;
# - influência → investigar observações;
# - problemas nos resíduos → revisar especificação.
#
# Depois:
#
# 1. modificar;
# 2. reajustar;
# 3. diagnosticar novamente.

# ============================================================
# 20. EXEMPLO DE MODIFICAÇÃO DA FORMA FUNCIONAL
# ============================================================

# Este bloco é apenas um exemplo.
#
# Não devemos incluir termos quadráticos automaticamente.
#
# Se a exploração/diagnóstico sugerir uma relação não linear
# entre preço e vendas, poderíamos considerar:

modelo_quadratico <- lm(
  vendas ~
    publicidade +
    preco_medio +
    I(preco_medio^2) +
    fluxo_clientes +
    renda_media +
    concorrentes,
  data = dados_modelo
)

summary(modelo_quadratico)

# Comparar com o modelo anterior:

AIC(
  modelo_candidato,
  modelo_quadratico
)

# Mas ainda precisamos diagnosticar o novo modelo.

# ============================================================
# 21. DEFINIR O MODELO FINAL
# ============================================================

# Depois de todas as decisões, escolhemos a especificação final.
#
# ATENÇÃO:
# ------------------------------------------------------------
# A linha abaixo é apenas um exemplo.
# Em uma análise real, o modelo_final deve ser definido após
# avaliar todos os diagnósticos e decisões anteriores.

modelo_final <- modelo_candidato


# ============================================================
# 22. DIAGNÓSTICO FINAL
# ============================================================

# Depois de definir o modelo final, repetir os diagnósticos.
#
# Nunca finalizar a análise imediatamente depois de modificar
# o modelo.

# ------------------------------------------------------------
# Gráficos diagnósticos
# ------------------------------------------------------------

par(mfrow = c(2, 2))
plot(modelo_final)
par(mfrow = c(1, 1))


# ------------------------------------------------------------
# VIF
# ------------------------------------------------------------

car::vif(modelo_final)


# ------------------------------------------------------------
# Influência
# ------------------------------------------------------------

car::influencePlot(modelo_final)


# ------------------------------------------------------------
# Heterocedasticidade
# ------------------------------------------------------------

lmtest::bptest(modelo_final)


# ------------------------------------------------------------
# Q-Q plot
# ------------------------------------------------------------

qqnorm(
  residuals(modelo_final)
)

qqline(
  residuals(modelo_final)
)


# ============================================================
# 23. RESULTADOS DO MODELO FINAL
# ============================================================

summary(modelo_final)

# O que apresentar?
# ------------------------------------------------------------
# - coeficientes;
# - erros-padrão;
# - IC;
# - p-valores;
# - R²;
# - R² ajustado;
# - erro residual.
#
# A interpretação deve priorizar:
# ------------------------------------------------------------
# - direção;
# - magnitude;
# - incerteza;
# - relevância prática.

# ------------------------------------------------------------
# Intervalos de confiança
# ------------------------------------------------------------

confint(modelo_final)


# ------------------------------------------------------------
# Coeficientes
# ------------------------------------------------------------

coef(modelo_final)


# ------------------------------------------------------------
# R²
# ------------------------------------------------------------

summary(modelo_final)$r.squared

summary(modelo_final)$adj.r.squared


# ------------------------------------------------------------
# Erro-padrão residual
# ------------------------------------------------------------

sigma(modelo_final)


# ------------------------------------------------------------
# AIC
# ------------------------------------------------------------

AIC(modelo_final)


# ============================================================
# 24. TABELA FINAL
# ============================================================

tidy(
  modelo_final,
  conf.int = TRUE
)

# Esta tabela reúne:
# - estimativa;
# - erro-padrão;
# - estatística;
# - p-valor;
# - IC95%.

# ============================================================
# 25. INFORMAÇÕES GERAIS DO MODELO
# ============================================================

glance(modelo_final)

# Útil para obter:
# - R²;
# - R² ajustado;
# - AIC, quando disponível;
# - estatística F;
# - tamanho amostral;
# - erro residual.

# ============================================================
# 26. INTERPRETAÇÃO DOS COEFICIENTES
# ============================================================

# Para uma variável quantitativa:
#
# "Mantidas as demais variáveis constantes, um aumento de uma
# unidade em X está associado a uma mudança média de beta unidades
# em Y."
#
# Exemplo:
#
# Se o coeficiente de publicidade for 1000:
#
# "Mantidas as demais variáveis constantes, um aumento de
# R$ 1 mil no investimento em publicidade está associado a um
# aumento médio de R$ 1.000 nas vendas mensais."
#
# IMPORTANTE:
# ------------------------------------------------------------
# Em um estudo observacional, associação não deve ser descrita
# automaticamente como efeito causal.

# ============================================================
# 27. CHECKLIST FINAL
# ============================================================

# Antes de encerrar a análise, verificar:
#
# [ ] Conheci a estrutura do banco?
# [ ] Verifiquei os tipos das variáveis?
# [ ] Verifiquei duplicidades?
# [ ] Identifiquei valores impossíveis?
# [ ] Avaliei os dados ausentes?
# [ ] Documentei o tratamento dos NA?
# [ ] Examinei as distribuições?
# [ ] Usei gráficos para explorar Y × X?
# [ ] Investiguei valores extremos?
# [ ] Ajustei o modelo inicial?
# [ ] Considerei estratégias de seleção de variáveis?
# [ ] Comparei modelos candidatos?
# [ ] Avaliei a forma funcional?
# [ ] Avaliei a independência?
# [ ] Avaliei a homocedasticidade?
# [ ] Avaliei os resíduos?
# [ ] Avaliei multicolinearidade?
# [ ] Avaliei observações influentes?
# [ ] Investiguei observações problemáticas?
# [ ] Reajustei o modelo quando necessário?
# [ ] Repeti o diagnóstico após modificar o modelo?
# [ ] Defini o modelo final?
# [ ] Interpretei magnitude e incerteza?
# [ ] Avaliei a relevância prática?
#
# Se alguma resposta for "não", a análise provavelmente ainda
# não está concluída.

# ============================================================
# FIM
# ============================================================
#
# A ideia central:
#
#          DADOS
#            ↓
#        INFORMAÇÃO
#            ↓
#          DECISÃO
#
# E, na regressão:
#
# DADOS → MODELO → DIAGNÓSTICO → DECISÃO
#
# Se o diagnóstico indicar problema:
#
# MODELO → DIAGNÓSTICO → MODIFICAÇÃO → NOVO MODELO
#
# O objetivo não é "passar nos testes".
# O objetivo é construir um modelo adequado à pergunta, aos dados
# e às condições sob as quais a inferência será realizada.
# ============================================================
