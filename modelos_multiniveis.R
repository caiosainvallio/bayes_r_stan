############################################################################
#                      Modelos Multiniveis                                 #
############################################################################


# hiperpriori: priori de uma priori

# y ~ Normal(10, Theta)
# Theta ~ Noemal(0, Phi)
# Phi ~ Exponencial(1)


# Random-Intercept model (linear) ----------------------------------------------

# y ~ Normal(Alpha + Alpha_j + X*Beta, Sigma)
# Alpha ~ Normal(Mu_Alpha, Sigma_alpha)
# Alpha_j ~ Normal(0, Tau)
# Beta ~ Normal(Mu_Beta, Sigma_Beta)
# Tau ~ Caichy+(0, Phi_Alpha) # parecido com uma Exponencial()
# Sigma ~ Exponencial(Lambda_Alpha)


# y ~ x1 + x2 + (1 | group)

# Rstanarm
stan_glmer(
  y ~ (1 + x1 | group) + (1 + x2 | group),
  prior_intercept = ...,
  prior_covariance = decov(1) # LKI com Nu = 1
)

# brms
brm(
  y ~ (1 + x1 | group) + (1 + x2 | group),
  prior = c(
    prior(lkj_corr_cholesky(1), class = L)
  )
)



# PRATICA ----------------------------------------------------------------------
library(brms)
library(tidyverse)
options(mc.cores = parallel::detectCores())


URL <- "https://raw.githubusercontent.com/storopoli/Estatistica-Bayesiana/master/datasets/cheese.csv"
cheese <- read_csv2(URL, col_types = "fffi")

cheese

cheese$y %>% median
cheese$y %>% mad
1/cheese$y %>% median


## Random-intyercept -----------------------------------------------------------

m1 <- brm(
  y ~ (1 | cheese) + background,
  data = cheese,
  family = gaussian(link="identity"),
  prior = c(
    set_prior("normal(0, 1)", class="b"),
    set_prior("student_t(3, 72, 13)", class="Intercept"),
    set_prior("exponential(0.05)", class="sigma"),
    set_prior("student_t(3, 0, 13)", class="sd", group="cheese")
  )
)

summary(m1)
pp_check(m1, ndraws = 50) + xlim(0, 150)
ranef(m1)
plot(m1)

pairs(m1)




m2 <- brm(
  y ~ (1 | rater) + background,
  data = cheese,
  family = gaussian(link="identity"),
  prior = c(
    set_prior("normal(0, 1)", class="b"),
    set_prior("student_t(3, 72, 13)", class="Intercept"),
    set_prior("exponential(0.05)", class="sigma"),
    set_prior("student_t(3, 0, 13)", class="sd", group="rater")
  )
)

summary(m2)
pp_check(m2, ndraws = 50) + xlim(0, 150)
ranef(m2)
plot(m2)




m3 <- brm(
  y ~ (1 | cheese) + (1 | rater) + background,
  data = cheese,
  family = gaussian(link="identity"),
  prior = c(
    set_prior("normal(0, 1)", class="b"),
    set_prior("student_t(3, 72, 13)", class="Intercept"),
    set_prior("exponential(0.05)", class="sigma"),
    set_prior("student_t(3, 0, 13)", class="sd")
  )
)

summary(m3)
pp_check(m3, ndraws = 50)# + xlim(0, 150)
ranef(m3)
plot(m3)




## Random-Slope ----------------------------------------------------------------

m4 <- brm(
  y ~ (0 + background | cheese),
  data = cheese,
  family = gaussian(link="identity"),
  prior = c(
    set_prior("student_t(3, 72, 13)", class="Intercept"),
    set_prior("exponential(0.05)", class="sigma"),
    set_prior("lkj(1)", class="cor")
  )
)

summary(m4)
pp_check(m4, ndraws = 50)# + xlim(0, 150)
ranef(m4)
plot(m4)



## Random-intercept-slope ------------------------------------------------------

m5 <- brm(
  y ~ (1 + background | cheese),
  data = cheese,
  family = gaussian(link="identity"),
  prior = c(
    # set_prior("normal(0, 1)", class="b"),
    set_prior("student_t(3, 72, 13)", class="Intercept"),
    set_prior("exponential(0.05)", class="sigma"),
    set_prior("student_t(3, 0, 13)", class="sd"),
    set_prior("lkj(1)", class="cor")
  )
)

summary(m5)
pp_check(m5, ndraws = 50)# + xlim(0, 150)
ranef(m5)
plot(m5)


