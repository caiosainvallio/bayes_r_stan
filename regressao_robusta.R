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




# t-Studant ao inves da Normal -------------------------------------------------
# Modelos com heterocedasticidade

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

brm(
  family = Student(link="identity") # mesma identidade da Gaussiana
)






# Beta-Binomial ao inves da Binomial -------------------------------------------

# A distribuicao binomial tem um a limitacao pratica de que temos somente
# um parametro livre (n, p) [n ja vem dos dados], o que implica em a __variancia 
# ser determinada pela media__. Isso faz com que a verossimilhanca binomial 
# __nao__ seja robusta a superdispersao.


# y ~ Beta-Binomial(n, p, Phi)
# p ~ Logistica/Probit(Alpha + X*Beta)
# Alpha ~ Normal(Mu_Alpha, Sigma_Alpha)
# Beta ~ Normal(Mu_Beta, Sigma_Beta)
# Phi ~ Exponencial(1)


# Verossimilhanca customizada
beta_binomial2 <- custom_family("beta_binomial2",
                                dpars = c("mu", "phi"),
                                links = c("logit", "log"),
                                lb = c(NA, 0),
                                type = "int",
                                vars = "vint1[n]")
stan_funs <- "
real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
  return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
}
int beta_binomial2_rgn(real mu, real phi, int T) {
  return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
}"
stanvars <- stanvar(scode = stan_funs, block = "functions")

brms(
  family = beta_binomial2,
  prior = c(
    prior(exponetial(1), class = phi)
  )
)





# t-Student ao inves de Binomial -----------------------------------------------

# modelo Robit
# Tornar a regressao logidtica mais robusta usando dados latentes `z` e usar
# uma distribuicao t-Student para os erros latentes `e`.


# y_i = {
#   0 se z_i < 0
#   1 se z_i > 0
# }
# z_i = X*Beta + e_i
# e_i ~ Student(Nu, 0, sqrt(Nu - 2 / Nu))
# Nu ~ Gamma(2, 0.1) \in [2, \inf)

stan_inv_robit <- "
real inv_robit(real y, real nu) {
  return(student_t_cdf(y, nu, 0, sqrt((nu - 2) / nu)));
}"
stanvar_inv_robit <- stanvar(scode = stan_inv_robit, block = "functions")
robit_formula <- bf(
  y_c | trials(1) ~ inv_robit(eta, nu),
  nlf(eta ~ b0 + b1 * x),
  b0 + b1 ~ 1,
  nu ~ 1,
  nl = TRUE
)

brm(
  formula = robit_formula,
  family = binomial("identity"),
  prior = c(
    prior(normal(0, 1), npar = b0),
    prior(normal(0, 1), npar = b1),
    prior(gamma(2, 0.1), nlpar = nu, lb = 2)
  ),
  stanvars = stanvar_inv_robit
)






# Binomial-Negativa ao inves de Poisson ----------------------------------------

# usada para superdispersao

# y ~ Binomial_Negativa(exp(Alpha + X*Beta), Phi)
# Phi ~ Gamma(0.01, 0.01) # cauda longa e positiva
# Alpha ~ Normal(Mu_Alpha, Sigma_Alpha)
# Beta ~ Normal(Mu_Beta, Sigma_Beta)

brm(
  family = negbinomial(link = "log")
)









