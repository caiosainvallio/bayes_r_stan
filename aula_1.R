
# dataset
data(mtcars)


####################### reg linear #########################
# rstanarm -------------------------------------------------
# r
# stan
# applyed regression models

library(rstanarm)

m1 <- stan_glm(mpg ~ hp + carb + gear, data=mtcars)
summary(m1)
plot(m1)




# brms ----------------------------------------------------
# bayesian
# regression
# models
# using Stan

library(brms)
# file.edit(file.path(Sys.getenv("HOME"), ".R", "Makevars.win"))
# excluir duplicatas

m2 <- brm(mpg ~ hp + carb + gear, data=mtcars)
summary(m2)
plot(m2)




####################### reg binomial #########################

m3 <- brm(am ~ hp + cyl + carb, data = mtcars, family = bernoulli)
summary(m3)
plot(m3)








