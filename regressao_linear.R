############################################################################
#                      Regressao Linear                                    #
############################################################################


# dataset
data("kidiq")
?kidiq


# Pacotes
library(rstanarm)
library(brms)

# multi-threading
options(mc.cores = parallel::detectCores())
options(Ncpus = parallel::detectCores())
options(brms.backend = "cmdstanr", brms.normalize = FALSE)



# rstanarm -----------------------------------------------------------
# modelo 1
m1 <- stan_glm(
  kid_score ~ mom_hs,
  data = kidiq,
  family = gaussian(link = "identity"),
  prior = normal(0, 2.5),
  prior_intercept = normal(mean(kidiq$kid_score), 2.5 * sd(kidiq$kid_score)),
  prior_aux = exponential(1 / sd(kidiq$kid_score))
)
m1
summary(m1, probs = c(0.025, 0.975))  # sigma = erro +|- sigma/2
pp_check(m1)
prior_summary(m1)



# modelo 2
m2 <- stan_glm(
  kid_score ~ mom_hs + mom_iq,
  data = kidiq,
  family = gaussian(link = "identity"),
  prior = normal(c(0, 0), c(2.5, 3)),
  prior_intercept = normal(mean(kidiq$kid_score), 2.5 * sd(kidiq$kid_score)),
  prior_aux = exponential(1 / sd(kidiq$kid_score))
)
m2
summary(m2, probs = c(0.025, 0.975))  # sigma = erro +|- sigma/2
pp_check(m2)
prior_summary(m2)



# modelo 3
m3 <- stan_glm(
  kid_score ~ mom_hs + mom_iq,
  data = kidiq,
  family = gaussian(link = "identity"),
  prior = c(
    normal(0, 2.5),        # mom_hs
    student_t(3, 0, 2.5)   # mom_iq
  ),
  prior_intercept = normal(mean(kidiq$kid_score), 2.5 * sd(kidiq$kid_score)),
  prior_aux = exponential(1 / sd(kidiq$kid_score))
)
m3
summary(m3, probs = c(0.025, 0.975))  # sigma = erro +|- sigma/2
pp_check(m3)
prior_summary(m3)



