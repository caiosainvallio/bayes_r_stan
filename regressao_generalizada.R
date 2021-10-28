

odds2prob <- function(odds){
  return(odds / (1 + odds))
}


############################# rstanarm ###############################
# pacotes
library(rstanarm)

# multi-threading
options(mc.cores = parallel::detectCores())

?wells
data("wells")



# modelo 1
m1 <- stan_glm(
  switch ~ arsenic + dist + assoc + educ,
  data = wells,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 2.5)
)
summary(m1, probs = c(.025, .975))


# modelo 2
library(dplyr)
wells <- wells %>% 
  mutate(dist_km = dist / 1e3)

m2 <- stan_glm(
  switch ~ arsenic + dist_km + assoc + educ,
  data = wells,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 2.5)
)
summary(m2, probs = c(.025, .975))


exp(m2$coefficients)
odds2prob(exp(m2$coefficients))




############################# brms ###############################
# pacotes
library(brms)

# multi-threading
options(mc.cores = parallel::detectCores())

# modelo 3
m3 <- brm(
  switch ~ arsenic + dist_km + assoc + educ,
  data = wells,
  family = binomial(link = "logit"),
  prior = c(
    set_prior("normal(0, 2.5)", class = "b"),
    set_prior("normal(0, 2.5)", class = "Intercept")
  )
)
summary(m3)


# modelo 4
m4 <- brm(
  switch ~ arsenic + dist_km + assoc + educ,
  data = wells,
  family = bernoulli(link = "logit"),
  prior = c(
    set_prior("normal(0, 2.5)", class = "b"),
    set_prior("normal(0, 2.5)", class = "Intercept")
  )
)
summary(m4)
plot(m4)
fixef(m4)
exp(fixef(m4))
odds2prob(exp(fixef(m4)))
pp_check(m4, ndraws = 50)


