############################################################################
#                      Regressao Poisson                                   #
############################################################################



# Exemplo dataset Roaches Gelman, 2007

# É uma basse de dados com 262 observacoes sonre a eficacia de um sistema
# de controle de pragas em reduzir o numero de baratas (roaches)
# em apartamentos urbanos.

# Variaveis:
#
# y: variavel dependente - numero de baratas mortas
# roach1: numero de baratas antes da detetizacao
# treatment: dummy para indicar se o apto foi detetizado
# senior: dummy para indicar se ha apenas idosos no apto
# exposure2: numero de dias de exposicao a detetizacao


# rstanarm --------------------------------------------------------------------

library(rstanarm)
library(ggplot2)
library(dplyr)
options(mc.cores = parallel::detectCores())
data("roaches")


# vizualizar a variavel dependente
roaches %>% 
  ggplot(aes(y)) + 
  geom_histogram()


sd(roaches$roach1) %>% log
sd(roaches$treatment) %>% log
sd(roaches$senior) %>% log

sum(roaches$y == 0)
log(0)


m1 <- stan_glm(
  y ~ roach1 + treatment + senior,
  data = roaches,
  family = poisson(link='log'),
  prior = normal(c(0, 0, 0), c(0.1, 5, 5)),
  prior_intercept = normal(0, 2.5)
)

summary(m1)
pp_check(m1) # esta errando muito, ele nao consegue estimar o zero


# ctrl + shift + F10 = restar session


# brms --------------------------------------------------------------------
library(brms)
library(ggplot2)

roaches <- rstanarm::roaches

m2 <- brm(
  y ~ roach1 + treatment + senior,
  data = roaches,
  family = poisson(link="log"),
  prior = c(
    set_prior("normal(0, 0.1)", coef = "roach1"),
    set_prior("normal(0, 5)"),
    set_prior("normal(0, 2.5)", class = "Intercept")
  )
)

summary(m2)
pp_check(m2, ndraws = 50) + xlim(0, 200)




