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
  }
"
stanvars <- stanvar(scode = stan_funs, block = "functions")

brms(
  family = beta_binomial2,
  prior = c(
    prior(exponetial(1), class = phi)
  )
)







