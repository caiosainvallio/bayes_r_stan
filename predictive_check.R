
# Worklow de modelos Bayesianos

#               Prior           Posterior
#             Predictive       Predictive
#               check            check
#            <--------->      <--------->
# especificacao       Elicita??o        Infer?ncia
#   do modelo         das Prioris       Posterior


# dataset
data(mtcars)


########################## Prior Predictive Check ###########################
# inspecao visual
# rstanarm  prior_PD = TRUE
# brms      sample_prior = "only"

# brms ----------------------------------------------------------------------
library(brms)

median(mtcars$mpg)    # Intercept
mad(mtcars$mpg)       # Intercept
mean(mtcars$mpg)      # b
2.5 * sd(mtcars$mpg)  # b
1 / sd(mtcars$mpg)    # sigma

m1_prior <- brm(
  mpg ~ wt + hp,
  data = mtcars,
  family = gaussian(),
  prior = c(
    set_prior("student_t(3, 19.2, 5.41149)", class = "Intercept"),
    set_prior("normal(20.09062, 15.06737)", class = "b"),
    set_prior("exponential(0.1659215)", class = "sigma")
  ),
  sample_prior = "only"
)
m1_prior
plot(m1_prior)






# rstanarm ---------------------------------------------------------------
library(rstanarm)

y <- mtcars$mpg

m2_prior <- stan_glm(
  mpg ~ wt + hp,
  data = mtcars,
  prior_intercept = normal(mean(y), 2.5 * sd(y)),
  prior = student_t(3, 0, 1),
  prior_aux = exponential(.1),
  prior_PD = TRUE
)
m2_prior
plot(m2_prior)
plot(m2_prior, "mcmc_dens")





########################## Posterior Predictive Check ###########################
# inspecao visual
# rstanarm  pp_check(modelo)
# brms      pp_check(modelo)  pp_check(modelo, type = "ecdf_overlap")


# brms ----------------------------------------------------------------------
library(brms)

median(mtcars$mpg)    # Intercept
mad(mtcars$mpg)       # Intercept
mean(mtcars$mpg)      # b
2.5 * sd(mtcars$mpg)  # b
1 / sd(mtcars$mpg)    # sigma

m1_posterior <- brm(
  mpg ~ wt + hp,
  data = mtcars,
  family = gaussian(),
  prior = c(
    set_prior("student_t(3, 19.2, 5.41149)", class = "Intercept"),
    set_prior("normal(20.09062, 15.06737)", class = "b"),
    set_prior("exponential(0.1659215)", class = "sigma")
  ),
  sample_prior = "only"
)
m1_posterior
plot(m1_posterior)
pp_check(m1_posterior)




# rstanarm ---------------------------------------------------------------
library(rstanarm)

y <- mtcars$mpg

m2_posterior <- stan_glm(
  mpg ~ wt + hp,
  data = mtcars,
  prior_intercept = normal(mean(y), 2.5 * sd(y)),
  prior = student_t(3, 0, 1),
  prior_aux = exponential(.1),
  prior_PD = TRUE
)
m2_posterior
plot(m2_posterior)
plot(m2_posterior, "mcmc_dens")
pp_check(m2_posterior)




