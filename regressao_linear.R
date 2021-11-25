############################################################################
#                      Regressao Linear                                    #
############################################################################


# rstanarm -----------------------------------------------------------
# Pacotes
library(rstanarm)


# dataset
?kidiq
data("kidiq")


# multi-threading
options(mc.cores = parallel::detectCores())
# options(Ncpus = parallel::detectCores())



# modelo 1
1 / sd(kidiq$kid_score)
m1 <- stan_glm(
  kid_score ~ mom_hs,
  data = kidiq,
  family = gaussian(link = "identity"),
  prior = normal(0, 2.5),
  prior_intercept = normal(mean(kidiq$kid_score), 2.5 * sd(kidiq$kid_score)),
  prior_aux = exponential(0.048)
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




# modelo 4
m4 <- stan_glm(
  kid_score ~ mom_hs + mom_iq + mom_age,
  data = kidiq,
  family = gaussian(link = "identity"),
  prior = c(normal(0, 2.5)),
  prior_intercept = normal(mean(kidiq$kid_score), 2.5 * sd(kidiq$kid_score)),
  prior_aux = exponential(1 / sd(kidiq$kid_score))
)
m4
summary(m4, probs = c(0.025, 0.975))  # sigma = erro +|- sigma/2
pp_check(m4)
prior_summary(m4)



# modelo 5
m5 <- stan_glm(
  kid_score ~ mom_hs * mom_iq + mom_age,
  data = kidiq,
  family = gaussian(link = "identity"),
  prior = c(normal(0, 2.5)),
  prior_intercept = normal(mean(kidiq$kid_score), 2.5 * sd(kidiq$kid_score)),
  prior_aux = exponential(1 / sd(kidiq$kid_score))
)
m5
summary(m5, probs = c(0.025, 0.975))  # sigma = erro +|- sigma/2
pp_check(m5)
prior_summary(m5)





# brms -----------------------------------------------------------
# Pacotes
library(brms)


# multi-threading
options(mc.cores = parallel::detectCores())
# options(Ncpus = parallel::detectCores())



# modelo 6

median(kidiq$kid_score)
2.5*mad(kidiq$kid_score)
1/sd(kidiq$kid_score)


m6 <- brm(
  kid_score ~ mom_hs,
  data = kidiq,
  family = gaussian(link = "identity"),
  prior = c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("student_t(3, 90, 48)", class = "Intercept"),
    set_prior("exponential(0.05)", class = "sigma")
  )
)
summary(m6)



# modelo 7
m7 <- brm(
  kid_score ~ mom_hs,
  data = kidiq,
  family = gaussian(link = "identity"),
  prior = c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("student_t(3, 90, 48)", class = "Intercept"),
    set_prior("exponential(0.05)", class = "sigma")
  ),
  sample_prior = "yes"
)
summary(m7)
hypothesis(m7, "mom_hs > 0")
plot(hypothesis(m7, "mom_hs > 0"))




# modelo 8
m8 <- brm(
  kid_score ~ mom_hs * mom_iq + mom_age,
  data = kidiq,
  family = gaussian(link = "identity"),
  prior = c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("student_t(3, 90, 48)", class = "Intercept"),
    set_prior("exponential(0.05)", class = "sigma")
  ),
  sample_prior = "yes"
)
summary(m8)
hypothesis(m8, "mom_hs > 0")
plot(hypothesis(m8, "mom_hs > 0"))
pp_check(m8, ndraws = 50)



# modelo 9 - variáveis correlacionadas (decomposicao QR)
m9 <- brm(
  bf(kid_score ~ mom_hs * mom_iq + mom_age, decomp = "QR"),
  data = kidiq,
  family = gaussian(link = "identity"),
  prior = c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("student_t(3, 90, 48)", class = "Intercept"),
    set_prior("exponential(0.05)", class = "sigma")
  ),
  sample_prior = "yes"
)
summary(m9)
hypothesis(m9, "mom_hs > 0")
plot(hypothesis(m9, "mom_hs > 0"))
pp_check(m9, ndraws = 50)



# offtopic mediation

# indep -> med -> dep
# bf_dep <- bf(dep ~ med + indep)
# bf_med <- bf(med ~ indep)
# brm(bf_dep + bf_med + set_rescor(FALSE))









