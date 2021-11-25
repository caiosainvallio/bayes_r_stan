############################################################################
#                      Regressao Robusta                                   #
############################################################################

# Usadas para:
## Outliers
## Super(sub)dispersao - zero inflated

# Exemplos
## t-Student <- Normal
## Beta_binomial <- Binomial
## t-Student <- Binomial
## Binomial-negativa <- Poisson
## Mistura de Binomial-negativa <- Poisson


# Esses modelos estao apenas no `brms`

# outliers:
# modelos baseados em distribuicao normal não são robustos para dados com outliers.
# isso gera instabilidade na inferencia


# Overdispersion:
# adiciona um (hiper)parametro que permite a superdispersao.


# t-Studant ao inves da Normal

# y ~ Student(Nu, Alpha + X*Beta, Sigma)
# Alpha ~ Normal(Mu_Alpha, Sigma_Alpha)
# Beta ~ Normal(Mu_Beta, Sigma_Beta)
# Nu ~ Log-Normal(2, 1)
# Sigma ~ Exponencial(Lambda_Alpha)


# distribuicao t-Sdudant com graus de libredade igual a 1 é chamada de 
# distribuicao Cauchy - não tem média (não tem o primeiro momento)

# distribuicao t-Sdudant com graus de libredade igual a 2,
# não tem vari6ancia (não tem o segundo momento)

# Distribuicao com cauda longa com mais de 2 graus de liberdade



