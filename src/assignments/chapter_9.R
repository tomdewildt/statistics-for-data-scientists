# Problem 9.1

#### Setup
set.seed(78)

### 1. Generate data y containing n = 50 observations from a normal distribution
###    with μ = 10 and s2 = 2.
mu <- 10
s2 <- 2
n <- 50

y <- rnorm(n, mean = mu, sd = sqrt(s2))
y

### 2. Write a function that takes as arguments the data y and the prior mean
###    and variance μ0, t20, and returns the posterior mean and variance μ1,
###    t21.
posterior <- function(y, prior_mu, prior_s2) {
  ybar <- mean(y)
  n <- length(y)

  post_mu <- (prior_mu / prior_s2 + (n * ybar) / s2) / (1 / prior_s2 + n / s2)
  post_s2 <- (1 / prior_s2 + n / s2)^-1

  c(post_mu, post_s2)
}

prior_mu <- 12
prior_s2 <- 0.1

post <- posterior(y, prior_mu, prior_s2)
post

### 3. Plot the density of the prior distribution.
curve(
  dnorm(x, mean = prior_mu, sd = sqrt(prior_s2)),
  from = 9,
  to = 13,
  ylim = c(0, 2.5),
  xlab = expression(mu),
  ylab = "Density"
)

### 4. Add to the previous plot the density of the posterior distribution in
###    red. Furthermore, add a vertical grey line at  ̄y.
ybar <- mean(y)

curve(
  dnorm(x, mean = post[1], sd = sqrt(post[2])),
  add = TRUE,
  col = "red"
)
abline(v = ybar, col = "grey")

### 5. Play around with different choices of μ0, t20, and n, and see how these
###    affect the posterior distribution p(μ|y).
prior_mu <- 9
prior_s2 <- 0.5

post <- posterior(y, prior_mu, prior_s2)
post

curve(
  dnorm(x, mean = prior_mu, sd = sqrt(prior_s2)),
  from = 7,
  to = 11,
  ylim = c(0, 2.5),
  xlab = expression(mu),
  ylab = "Density"
)
curve(
  dnorm(x, mean = post[1], sd = sqrt(post[2])),
  add = TRUE,
  col = "red"
)
abline(v = ybar, col = "grey")
