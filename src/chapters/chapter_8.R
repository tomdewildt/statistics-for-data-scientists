# 8.2 Simulated data for model exploration

#### Set the number of subjects
n <- 100

#### Generate randomly uniform ages between 20 and 80
age <- runif(n, min = 20, max = 80)

#### Model usage time as a quadratic function of age plus noise
time <- 320 + 25 * age - 0.3 * age^2 + rnorm(n, mean = 0, sd = 80)

#### Put everything together in a data frame
data1 <- data.frame(n = 1:n, age = age, time = time)

# 8.3 Linear Models

#### Make a call to the default plotting function and provide labels
plot(data1$age, data1$time, xlab = "Age", ylab = "Usage time")

### 8.3.3 Using [R] to fit lines and curves

#### Add a column to the dataset containing age squared:
data1$age_squared <- data1$age^2

#### Fit an "intercept only" model
m0 <- lm(time ~ 1, data = data1)
summary(m0)

#### Fit a model with an intercept and a linear effect of age:
m1 <- lm(time ~ 1 + age, data = data1)
summary(m1)

#### Fit a model with a linear and a quadratic effect of age:
m2 <- lm(time ~ 1 + age + age_squared, data = data1)
summary(m2)

### 8.3.4 Simple approaches to model comparisons

#### Print the rounded AIC value for each model
c(round(AIC(m0)), round(AIC(m1)), round(AIC(m2)))

#### Print the rounded BIC value for each model
c(round(BIC(m0)), round(BIC(m1)), round(BIC(m2)))

#### Print an ANOVA (F-test) comparison table
anova(m0, m1, m2)

### 8.3.5 Visualizing regression models

#### Setup the scatterplot
plot(data1$age, data1$time, xlab = "Age", ylab = "Usage time")

#### Add the (horizontal) line for m0
abline(m0, lty = 1)

#### Add the line for m1
abline(m1, lty = 2)

#### Sort the data (necessary to add the line for m2)
data1 <- data1[order(data1$age), ]

#### Add a line using age and the predictions from m2 (note the use of the
#### predict function)
lines(data1$age, predict(m2, newdata = data1), lty = 3)

# 8.4 Overfitting and regularization

#### Fit a high order polynomial to the data using the poly function
m3 <- lm(time ~ poly(data1$age, degree = 30, raw = TRUE), data = data1)

#### Create the scatterplot
plot(data1$age, data1$time, xlab = "Age", ylab = "Usage time")

#### Add the line predicted by model m3
lines(data1$age, predict(m3, data = data1))

#### Compute predictions for model m2
yhat_m2_in <- predict(m2, data = data1)

#### Compute the within-sample mean squared error for model m2
sum((data1$time - yhat_m2_in)^2) / n

#### Compute predictions for model m3
yhat_m3_in <- predict(m3, data = data1)

#### Compute the within-sample mean squared error for model m3
sum((data1$time - yhat_m3_in)^2) / n

#### Create a new dataset called data2. Note that we use the same parameters as
#### before (albeit a larger n)
n <- 1000
age <- runif(n, min = 20, max = 80)
time <- 320 + 25 * age - 0.3 * age^2 + rnorm(n, mean = 0, sd = 80)
data2 <- data.frame(n = 1:n, age = age, time = time)

#### Generate predictions for the new dataset (data2) using the estimated
#### coefficients in m2
yhat_m2_out <- predict(m2, data = data2)

#### Compute the out-of-sample mean squared error for model m2
sum((data2$time - yhat_m2_out)^2) / n

#### Generate predictions for the new dataset (data2) using the estimated
#### coefficients in m3
yhat_m3_out <- predict(m3, data = data2)

#### Compute the out-of-sample mean squared error for model m3
sum((data2$time - yhat_m3_out)^2) / n

# 8.6 Generalized Linear Models

#### Setup
library(arm)

#### Set the number of people and generate random ages
n <- 100
age <- runif(n, min = 20, max = 80)

#### Use an "inverse logit" function to compute the probability of age
probability_adopt <- invlogit(
  320 + 25 * age - 0.3 * age^2 + rnorm(n, mean = 0, sd = 80) - 700
)

#### Simulate usage from a Bernoulli(probability_adopt) distribution
adopt <- rbinom(n, size = 1, prob = probability_adopt)

#### Create the new dataset
data3 <- data.frame(n = 1:n, age = age, adopt = adopt)

#### Quick plot of the invlogit function
plot(invlogit, xlim = c(-10, 10), ylim = c(0, 1))

#### Scatterplot of the choice to adopt (0, 1) the application as a function of
#### age (note the call to jitter())
plot(data3$age, jitter(data3$adopt))

#### Create age squared in the new dataset
data3$age_squared <- data3$age^2

#### Fit the Generalized Linear Model
gm <- glm(
  adopt ~ age + age_squared,
  data = data3,
  family = binomial(link = "logit")
)

#### Make a scatterplot of the data
plot(data3$age, jitter(data3$adopt))

#### Sort the data (necessary to add the line for gm)
data3 <- data3[order(data3$age), ]

#### Add the predicted line to the plot
lines(data3$age, predict(gm, data = data3, type = "response"))
