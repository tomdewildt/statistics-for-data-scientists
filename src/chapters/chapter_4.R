# 4.10 Working with distributions in [R]

### 4.10.1 [R] built-in functions
mu <- 0
variance <- 1
sd <- sqrt(variance)

x <- 1

#### Calculate PDF of normal distribution
dnorm(x, mean = mu, sd = sd)

#### Calculate CDF of normal distribution
pnorm(x, mean = mu, sd = sd)

#### Calculate quantile function Q(p) = x if and only if p = F(x)
p <- pnorm(x, mean = mu, sd = sd)
qnorm(p, mean = mu, sd = sd)

#### Take 10 draws from the normal distribution
set.seed(982749)
rnorm(10, mean = mu, sd = sd)

### 4.10.2 Using MC methods to approximate expectations and variances
draws <- rnorm(10^6, mean = 2, sd = 3)
mean(draws)

#### Setup
n <- 10^5
p <- 1 / 3
mu <- 10
sd <- 1

#### Flip coin (Bernoulli distribution)
y <- rbinom(n, size = 1, prob = p)

#### Generate draws
z <- rnorm(n, mean = (y * 10) + mu, sd = (y * 4) + sd)

#### Plot draws
hist(z, freq = FALSE, breaks = 50)

#### Calculate mean & variance
mean_z <- mean(z)
mean_z

var_z <- mean((z - mean(z))^2)
var_z
