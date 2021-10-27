
########################### rsrtanarm #################################
library(rstanarm)

?mtcars

?stan_glm

# prior maluca
m1 <- stan_glm(
  mpg ~ wt + hp, 
  data = mtcars,
  prior_intercept = normal(mean(mtcars$mpg), 2.5 * sd(mtcars$mpg)),
  prior = student_t(df = 5, 0, 42),
  prior_aux = student_t(df = 3, 0, 1729),
  family = gaussian()
)
summary(m1)


# prior maluca^2
m2 <- stan_glm(
  mpg ~ wt + hp, 
  data = mtcars,
  prior_intercept = normal(mean(mtcars$mpg), 2.5 * sd(mtcars$mpg)),
  prior = student_t(df = 5, 5432, 0.1),
  prior_aux = student_t(df = 3, 3200, 2),
  family = gaussian()
)
summary(m2)




########################### brms #################################
library(brms)

?brm()

m3 <- brm(
  mpg ~ wt + hp,
  data = mtcars,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 2)", class = "b", coef = "wt"),
    set_prior("normal(0, 3)", class = "b", coef = "hp"),
    set_prior("student_t(3, 40, 10)", class = "Intercept")
  )
)
m3

median(mtcars$mpg)
mad(mtcars$mpg)

mean(mtcars$mpg)
2.5 * sd(mtcars$mpg)

1 / sd(mtcars$mpg)

m4 <- brm(
  mpg ~ wt + hp,
  data = mtcars,
  family = gaussian(),
  prior = c(
    set_prior("student_t(3, 19.2, 5.41149)", class = "Intercept"),
    set_prior("normal(20.09062, 15.06737)", class = "b"),
    set_prior("exponential(0.1659215)", class = "sigma")
  )
)
m4







