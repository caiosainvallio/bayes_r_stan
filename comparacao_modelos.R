############################################################################
#                      comparacao de modelos                               #
############################################################################

# lppd = log posterior predictive density
# elpd = expected log pointwise predictive density
# loo = leave one out (cross-validation)
# waic = widely applicable information criteria
# psis-loo = pareto smoothed importance sampling loo

# bibliotecas
library(brms)
options(mc.cores = parallel::detectCores())

# dataset
roaches <- rstanarm::roaches

# ajustar modelos
m1 <- brm(y ~ roach1 + treatment + senior,
          data = roaches,
          family = poisson,
          prior = c(
            set_prior("normal(0, 0.1)", class = "b", coef = "roach1"),
            set_prior("normal(0, 5)", class = "b", coef = "treatment"),
            set_prior("normal(0, 5)", class = "b", coef = "senior"),
            set_prior("normal(0, 2.5)", class = "Intercept")
          ))
summary(m1)


m2 <- brm(y ~ roach1 + treatment + senior,
          data = roaches,
          family = negbinomial,
          prior = c(
            set_prior("normal(0, 0.1)", class = "b", coef = "roach1"),
            set_prior("normal(0, 5)", class = "b", coef = "treatment"),
            set_prior("normal(0, 5)", class = "b", coef = "senior"),
            set_prior("normal(0, 2.5)", class = "Intercept"),
            set_prior("exponential(1)", class = "shape")
          ))
summary(m2)


m3 <- brm(y ~ roach1 + treatment + senior,
          data = roaches,
          family = zero_inflated_negbinomial,
          prior = c(
            set_prior("normal(0, 0.1)", class = "b", coef = "roach1"),
            set_prior("normal(0, 5)", class = "b", coef = "treatment"),
            set_prior("normal(0, 5)", class = "b", coef = "senior"),
            set_prior("normal(0, 2.5)", class = "Intercept"),
            set_prior("exponential(1)", class = "shape"),
            set_prior("beta(1, 1)", class = "zi")
          ))
summary(m3)


# comparar modelos

loo_m1 <- loo(m1)
loo_m2 <- loo(m2)
loo_m3 <- loo(m3)


loo_compare(loo_m1, loo_m2, loo_m3)


#   elpd_diff se_diff 
# m2      0.0       0.0
# m3     -0.6       0.5
# m1 -30727.3    3935.5

# Se 2x standard error é maior do que o tamanho da diferença, significa que tem 
# "evidencias para que não haja diferença entre os modelos" 





