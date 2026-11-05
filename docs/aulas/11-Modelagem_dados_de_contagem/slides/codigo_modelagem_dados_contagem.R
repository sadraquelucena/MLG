# ============================================================
# MODELOS LINEARES GENERALIZADOS PARA DADOS DE CONTAGEM
# Base: MASS::quine | Resposta: Days
# ============================================================

# PACOTES

library(MASS)
library(dplyr)
library(ggplot2)
library(knitr)
library(hnp)
library(lmtest)

data("quine", package = "MASS")
dados <- quine


# 1. INSPEÇÃO INICIAL ----------------------------------------

str(dados)
head(dados)
summary(dados)
lapply(dados[c("Eth", "Sex", "Age", "Lrn")], table)

mean(dados$Days)
var(dados$Days)


# 2. ANÁLISE EXPLORATÓRIA ------------------------------------

## 2.1 Estatísticas descritivas

resumo_days <- dados |>
  summarise(
    n = n(),
    media = mean(Days),
    desvio_padrao = sd(Days),
    variancia = var(Days),
    mediana = median(Days),
    minimo = min(Days),
    maximo = max(Days),
    Q1 = quantile(Days, 0.25),
    Q3 = quantile(Days, 0.75)
  )

knitr::kable(resumo_days, digits = 2)

## 2.2 Distribuição da resposta

ggplot(dados, aes(x = Days)) +
  geom_histogram(binwidth = 5, boundary = 0, color = "white") +
  labs(
    title = "Distribuição dos dias de ausência escolar",
    x = "Dias de ausência",
    y = "Número de estudantes"
  ) +
  theme_minimal()

## 2.3 Distribuição por covariável

ggplot(dados, aes(x = Age, y = Days)) +
  geom_boxplot() +
  labs(
    title = "Dias de ausência segundo faixa etária",
    x = "Faixa etária (código original)",
    y = "Dias de ausência"
  ) +
  theme_minimal()

ggplot(dados, aes(x = Eth, y = Days)) +
  geom_boxplot() +
  labs(
    title = "Dias de ausência segundo grupo étnico",
    x = "Grupo étnico (código original)",
    y = "Dias de ausência"
  ) +
  theme_minimal()

ggplot(dados, aes(x = Sex, y = Days)) +
  geom_boxplot() +
  labs(
    title = "Dias de ausência segundo sexo",
    x = "Sexo (código original)",
    y = "Dias de ausência"
  ) +
  theme_minimal()

ggplot(dados, aes(x = Lrn, y = Days)) +
  geom_boxplot() +
  labs(
    title = "Dias de ausência segundo categoria de aprendizagem",
    x = "Categoria de aprendizagem (código original)",
    y = "Dias de ausência"
  ) +
  theme_minimal()


# 3. AJUSTE DOS MODELOS COMPLETOS ----------------------------

## 3.1 Poisson

modelo_pois_log <- glm(
  Days ~ Eth + Sex + Age + Lrn,
  family = poisson(link = "log"),
  data = dados
)

modelo_pois_sqrt <- glm(
  Days ~ Eth + Sex + Age + Lrn,
  family = poisson(link = "sqrt"),
  data = dados
)

## 3.2 Binomial negativa

modelo_bn_log <- MASS::glm.nb(
  Days ~ Eth + Sex + Age + Lrn,
  data = dados,
  link = "log"
)

modelo_bn_sqrt <- MASS::glm.nb(
  Days ~ Eth + Sex + Age + Lrn,
  data = dados,
  link = "sqrt"
)


# 4. COMPARAÇÃO INICIAL DOS MODELOS --------------------------

AIC(modelo_pois_log, modelo_pois_sqrt, modelo_bn_log, modelo_bn_sqrt)

BIC(modelo_pois_log, modelo_pois_sqrt, modelo_bn_log, modelo_bn_sqrt)


## 4.1 Razão entre deviance e graus de liberdade

deviance(modelo_pois_log) / df.residual(modelo_pois_log)
deviance(modelo_pois_sqrt) / df.residual(modelo_pois_sqrt)
deviance(modelo_bn_log) / df.residual(modelo_bn_log)
deviance(modelo_bn_sqrt) / df.residual(modelo_bn_sqrt)


# 5. SELEÇÃO DE VARIÁVEIS ------------------------------------

## 5.1 Testes de retirada individual no modelo completo

selecao_drop1 <- drop1(modelo_bn_log, test = "Chisq")
selecao_drop1

# Sem Sex
modelo_bn_log2 <- MASS::glm.nb(
  Days ~ Eth + Age + Lrn,
  data = dados,
  link = "log"
)

drop1(modelo_bn_log2, test = "Chisq")


## 5.2 Modelo reduzido: Eth + Age (Sem Sex e Lrn)

modelo_bn_log2 <- MASS::glm.nb(
  Days ~ Eth + Age,
  data = dados,
  link = "log"
)

drop1(modelo_bn_log2, test = "Chisq")


## 5.3 Comparação do modelo completo com o reduzido

teste_modelos <- lmtest::lrtest(modelo_bn_log2, modelo_bn_log)
teste_modelos


# 6. MODELO FINAL E INTERPRETAÇÃO ----------------------------
# Modelo final: binomial negativo com ligação log
# Fórmula: Days ~ Eth + Age

summary(modelo_bn_log2)

## 6.1 Coeficientes, razões de médias e IC95%

summary(modelo_bn_log2)
confint(modelo_bn_log2)

Razao_de_medias <- exp(coef(modelo_bn_log2))
IC95_inferior <- exp(confint(modelo_bn_log2))[, 1]
IC95_superior <- exp(confint(modelo_bn_log2))[, 2]

cbind(
  Razao_de_medias,
  IC95_inferior,
  IC95_superior
)

## 6.2 Parâmetro de dispersão

modelo_bn_log2$theta
summary(modelo_bn_log2)$SE.theta

## 6.3 Médias ajustadas

dados$media_ajustada_bn <- predict(
  modelo_bn_log2,
  type = "response"
)

head(dados[, c("Days", "media_ajustada_bn")])

## 6.4 Observados versus previstos

ggplot(dados, aes(x = media_ajustada_bn, y = Days)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = "Dias observados versus previstos",
    x = "Média prevista pelo modelo binomial negativo",
    y = "Dias de ausência observados"
  ) +
  theme_minimal()


# 7. DIAGNÓSTICO DO MODELO FINAL -----------------------------

## 7.1 Envelope simulado meio-normal

set.seed(123)

hnp::hnp(
  modelo_bn_log2,
  sim = 999,
  resid.type = "deviance",
  paint.out = TRUE,
  print.on = TRUE
)

## 7.2 Resíduos de Pearson versus valores ajustados

plot(
  fitted(modelo_bn_log2),
  residuals(modelo_bn_log2, type = "pearson"),
  xlab = "Valores ajustados",
  ylab = "Resíduos de Pearson",
  main = "Resíduos de Pearson versus valores ajustados"
)
abline(h = 0, lty = 2)

## 7.3 Resíduos de deviance versus valores ajustados

plot(
  fitted(modelo_bn_log2),
  residuals(modelo_bn_log2, type = "deviance"),
  xlab = "Valores ajustados",
  ylab = "Resíduos de deviance",
  main = "Resíduos de deviance versus valores ajustados"
)
abline(h = 0, lty = 2)

## 7.4 Distância de Cook

cook <- cooks.distance(modelo_bn_log2)

plot(
  cook,
  type = "h",
  xlab = "Observação",
  ylab = "Distância de Cook",
  main = "Distância de Cook — modelo final"
)
abline(h = 4 / nobs(modelo_bn_log2), lty = 2)

# Observações acima da referência exploratória 4/n
observacoes_influentes <- which(
  cook > 4 / nobs(modelo_bn_log2)
)

observacoes_influentes

# Inspeção dos registros identificados
dados[observacoes_influentes, ]
