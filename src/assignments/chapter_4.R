# Problem 4.1

#### Using binomial distribution
#### Group A = "success", Pr(X=1) = 3/6 = 1/2
#### Group B = "failure", Pr(X=0) = 3/6 = 1/2

### 1. What is the probability that exactly 10 patients will be in group A?

#### Calculate PMF Pr(S_n = k) = f(k; n, p)
k <- 10
n <- 20
p <- 0.5

dbinom(k, size = n, prob = p)

### 2. What is the probability that at most 9 patients will be allocated to
###    group A?

#### Calculate CDF Pr(S_n ≤ k)
k <- 9
n <- 20
p <- 0.5

pbinom(k, size = n, prob = p)

# Problem 4.2 (Part 1)

### 1. Derive the CDF of the Bernoulli distribution.

####           { p     if x = 1
#### Pr(X=x) = {
####           { 1 - p if x =0

####           { 0     if x < 0
#### Pr(X≤x) = { 1 - p if 0 ≤ x < 1
####           { 1     if x ≥ 1

# Problem 4.3
# Let us assume that the probability for a person in the Netherlands to be
# left-handed is 0.10. What is the probability that in a random group of 20
# persons from the Netherlands you will find at least three left-handed persons?

#### Calculate CDF Pr(S_n ≥ k)
k <- 3
n <- 20
p <- 0.10

pbinom(k - 1, size = n, prob = p, lower.tail = FALSE)

# Problem 4.4
# A specific diagnostic test has a known sensitivity of 0.9 for the related
# disease. Five patients, all carriers of the disease, do the diagnostic test.
# Give the probability distribution of the number of positive tests. This means
# that you need to calculate P(S5 = 0), P(S5 = 1), ..., P(S5 = 5), with S5 the
# random variable that indicates the number of positive tests.

#### Calculate probabilities (binomial distribution)
n <- 5
p <- 0.9

probabilities <- rep(NA, n + 1)
for (k in 0:n) {
  probabilities[k + 1] <- dbinom(k, size = n, prob = p)
}
probabilities

# Problem 4.6

### 1. Demonstrate that the function is indeed a density.

#### The following must hold for the function to be a density:
#### ∫1-0 f(x) * dx = 1

#### By using wolfram alpha to solve this integral: "integral 3*x^2 from 0 to 1"
#### We get the following: ∫1-0 3x^2 * dx = 1, the result of this integral is 1
#### so we can indeed conclude that the function is a density.

### 2. What is the mean, variance and standard deviation?

#### The formula for the mean is: ∫1-0 x * f(x) * dx = 1
#### By using wolfram alpha to solve this integral: "integral x*3*x^2 from 0 to
#### 1". We get the following ∫1-0 x * 3x^2 * dx = 0.75, the result of this
#### integral is 0.75, so the mean is 0.75 (3/4)

#### The formula for the variance is: ∫1-0 (x-mu)^2 * f(x) * dx = 1
#### By using wolfram alpha to solve this integral: "integral (x-0.75)^2*3*x^2
#### from 0 to 1". We get the following ∫1-0 (x-0.75)^2 * 3x^2 * dx = 0.0375,
#### the result of this integral is 0.0375, so the variance is 0.0375 (3/80)

#### The formula for the standard deviation is: sqrt(sigma^2)
#### So the standard deviation is: sqrt(0.0375) (sqrt(3/80))

### 3. How likely is it that the outcome will be in between 0.25 and 0.75?

#### a < x ≤ b = a < x and x < b = x > a and x < b

#### The formula for this probability is: Pr(a < x ≤ b) = ∫b-a f(x) * dx
#### a = 0.25
#### b = 0.75
#### By using wolfram alpha to solve this integral: "integral 3*x^2 from 0.25 to
#### 0.75". We get the following ∫0.75-0.25 3x^2 * dx = 0.40625, so the
#### probability for the outcome to be in between 0.25 and 0.75 is 0.40625

# Problem 4.8 (Part 2)

### 2. Compute the expected value and variance for the N (μ = 10, s2 = 3)
###    distribution using Monte Carlo (MC) simulation.

#### Setup
set.seed(42)

#### Simulate 10^6 = 1.000.000 draws
n <- 10^6
mu <- 10
sigma <- sqrt(3)

draws <- rnorm(n, mean = mu, sd = sigma)

#### Calculate mean
mean(draws)

#### Calculate variance
var(draws)
