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




