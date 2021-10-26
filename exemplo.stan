// exemplo y ~ x1 + x2

data {
  int<lower=0> N;
  vector[N] x1;
  vector[N] x2;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha;
  vector[2] beta;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  sigma ~ cauchy(0, 25);
  y ~ normal(alpha + beta[1] * x1 + beta[2] * x2, sigma);
}

