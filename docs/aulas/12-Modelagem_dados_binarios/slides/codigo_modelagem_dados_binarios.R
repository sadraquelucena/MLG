# ============================================================
# MODELOS LINEARES GENERALIZADOS PARA DADOS BINÁRIOS
# Modelagem de inadimplência de cartão de crédito.
# Base: ISLR2::Default | Resposta: default
# ============================================================

# PACOTES
library(ISLR2)
library(MASS)
library(tidyverse)
library(hnp)
library(lmtest)
library(pROC)

data("Default", package = "ISLR2")

# Traduindo as variáveis para português
dados <- Default |>
  dplyr::rename(
    inadimplencia = default,
    estudante = student,
    saldo_devedor = balance,
    renda_anual = income
  ) |>
  dplyr::mutate(
    inadimplencia = case_when(
      inadimplencia == "No" ~ 0,
      inadimplencia == "Yes" ~ 1
    ),
    estudante = factor(
      estudante,
      levels = c("No", "Yes"),
      labels = c("Não", "Sim")
    )
  )


# 1. INSPEÇÃO INICIAL ----------------------------------------

str(dados)
head(dados)
summary(dados)

# Contagem de clientes inadimplentes (1) e adimplentes (0)
dados |>
  count(inadimplencia) |>
  mutate(percentual = round(100 * n / sum(n), 1))


# 2. AJUSTE DOS MODELOS COMPLETOS ----------------------------

# Modelo logístico
modelo_logit <- glm(
  inadimplencia ~ estudante + saldo_devedor + renda_anual,
  family = binomial(link = "logit"),
  data = dados
)

# Modelo com ligação probit
modelo_probit <- glm(
  inadimplencia ~ estudante + saldo_devedor + renda_anual,
  family = binomial(link = "probit"),
  data = dados
)

# Modelo com ligação cauchit
modelo_cauchit <- glm(
  inadimplencia ~ estudante + saldo_devedor + renda_anual,
  family = binomial(link = "cauchit"),
  data = dados
)

# Modelo com ligação cloglog
modelo_cloglog <- glm(
  inadimplencia ~ estudante + saldo_devedor + renda_anual,
  family = binomial(link = "cloglog"),
  data = dados
)


# 3. COMPARAÇÃO DOS MODELOS --------------------------

AIC(modelo_logit, modelo_probit, modelo_cauchit, modelo_cloglog)

BIC(modelo_logit, modelo_probit, modelo_cauchit, modelo_cloglog)


# 4. SELEÇÃO DE VARIÁVEIS ------------------------------------

## 4.1 Testes de retirada individual no modelo completo

selecao_drop1 <- drop1(modelo_logit, test = "Chisq")
selecao_drop1

# Sem renda_anual
modelo_logit2 <- glm(
  inadimplencia ~ estudante + saldo_devedor,
  family = binomial(link = "logit"),
  data = dados
)

drop1(modelo_logit2, test = "Chisq")

AIC(modelo_logit, modelo_logit2)

BIC(modelo_logit, modelo_logit2)

## 4.2 Comparação do modelo completo com o reduzido
lmtest::lrtest(modelo_logit2, modelo_logit)


# 5. ANÁLISE DE RESÍDUOS -------------------------------------

## 5.1 Envelope simulado meio-normal

set.seed(123)

hnp::hnp(
  modelo_logit2,
  sim = 999,
  resid.type = "deviance",
  paint.out = TRUE,
  print.on = TRUE
)


# 5.2 Resíduos de Pearson versus valores ajustados

plot(
  fitted(modelo_logit2),
  residuals(modelo_logit2, type = "pearson"),
  xlab = "Valores ajustados",
  ylab = "Resíduos de Pearson",
  main = "Resíduos de Pearson versus valores ajustados"
)
abline(h = 0, lty = 2)

## 5.3 Resíduos de deviance versus valores ajustados

plot(
  fitted(modelo_logit2),
  residuals(modelo_logit2, type = "deviance"),
  xlab = "Valores ajustados",
  ylab = "Resíduos de deviance",
  main = "Resíduos de deviance versus valores ajustados"
)
abline(h = 0, lty = 2)

## 5.4 Distância de Cook

cook <- cooks.distance(modelo_logit2)

plot(
  cook,
  type = "h",
  xlab = "Observação",
  ylab = "Distância de Cook",
  main = "Distância de Cook — modelo final"
)
abline(h = 4 / nobs(modelo_logit2), lty = 2)

# Observações acima da referência exploratória 4/n
observacoes_influentes <- which(
  cook > 4 / nobs(modelo_logit2)
)

observacoes_influentes

# Inspeção dos registros identificados
dados[observacoes_influentes, ]


# 6. CURVA ROC e AUC -----------------------------------------

# Probabilidades estimadas
probabilidades <- predict(modelo_logit2, type = "response")

# Curva ROC
roc_modelo <- pROC::roc(
  response = dados$inadimplencia,
  predictor = probabilidades,
  levels = c(0, 1),
  direction = "<"
)

# Gráfico da curva ROC
plot(
  roc_modelo,
  col = "blue",
  lwd = 2,
  main = "Curva ROC - Modelo Logístico",
  legacy.axes = TRUE
)

# Linha de classificação aleatória
abline(a = 0, b = 1, lty = 2, col = "red")

# Área sob a curva
pROC::auc(roc_modelo)

# Intervalo de confiança de 95%
pROC::ci.auc(roc_modelo)


# Ponto de corte
ponto_corte <- pROC::coords(
  roc_modelo,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity"),
  transpose = FALSE
)

ponto_corte

# 7. MODELO FINAL E INTERPRETAÇÃO ----------------------------
# Modelo final: moelo logístico
# Fórmula: inadimplencia ~ estudante + saldo_devedor

## 7.1 Coeficientes e Odds Ratio

summary(modelo_logit2)
confint(modelo_logit2)

OR <- exp(coef(modelo_logit2))
IC95_inferior <- exp(confint(modelo_logit2))[, 1]
IC95_superior <- exp(confint(modelo_logit2))[, 2]

cbind(
  OR,
  IC95_inferior,
  IC95_superior
)


# 8. CLASSIFICAÇÃO  DE NOVA OBSERVAÇÕES ----------------------

# Novas observações
novos_clientes <- data.frame(
  cliente = c("Cliente 1", "Cliente 2", "Cliente 3"),
  estudante = factor(c("Não", "Não", "Sim"), levels = levels(dados$estudante)),
  saldo_devedor = c(500, 1200, 2000)
)

novos_clientes

# Probabilidades de inadimplẽncia estimadas
novos_clientes$probabilidade <- predict(
  modelo_logit2,
  newdata = novos_clientes,
  type = "response"
)

novos_clientes

# Classificação usando o ponto de corte

# Classificação
corte <- ponto_corte$threshold
novos_clientes <- novos_clientes |>
  mutate(
    classificacao = ifelse(
      probabilidade >= corte,
      "Inadimplente",
      "Não inadimplente"
    )
  )

novos_clientes
