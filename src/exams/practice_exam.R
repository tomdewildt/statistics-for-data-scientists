library(e1071)
library(MASS)
library(outliers)

# Question 1: B

# Question 2: C

# Question 3: B
S1 <- c(1, 2, 3)
S2 <- c(3, 4, 5, 6, 7, 8)
S3 <- c(8, 9, 10)

s1 <- S1^2 - S1
s2 <- S2^2 - S2
s3 <- S3^2 - S3

pi1 <- 2 / 9
pi2 <- 5 / 9
pi3 <- 2 / 9

(mean(s1) * pi1 + mean(s2) * pi2 + mean(s3) * pi3) - 33

# Question 4: -

# Question 5: -

# Question 6: B

# Question 7: D

# Question 8: D

# Question 9: A

# Question 10: B
cor_x1_x2 <- -0.8788331
cov_x1_x2 <- -4.777778
sigma2_x1 <- 10

(cov_x1_x2 / (cor_x1_x2 * sqrt(sigma2_x1)))^2

# Question 11: B

# Question 12: C
M <- matrix(c(1:6), ncol = 2)
M[4,]

# Question 13: C

# Question 14: D

# Question 15: B

# Question 16: C

# Question 17: A
f_yx_1_2 <- 5 / 12
f_yx_2_2 <- 1 / 12
f_x_1 <- 6 / 12

1 * (f_yx_1_2 / f_x_1) + 2 * (f_yx_2_2 / f_x_1)

# Question 18: A
min_pages <- 1
max_pages <- 15

max_pages - min_pages

# Question 19: C

# Question 20: B
b0 <- -6.445749
b1 <- -0.170094
b2 <- 0.380921

b0 + b1 * 6 + b2 * 10
b0 + b1 * 6 + b2 * 20

# Question 21: A

# Question 22: A

# Question 23: D
time <- 2
pages <- 3

1 / (1 + exp(-1 * (-9.8 + 2 * time - 0.3 * time^2 + 0.4 * pages)))

# Question 24: A

# Question 25: C

# Question 26: C
b0 <- 12003
b1 <- 564
b2 <- -4

x <- 20
y <- 23203

yhat <- b0 + b1 * x^1 + b2 * x^2

(y - yhat)^2

# Question 27: A
x <- c(rep(18, 2), rep(19, 8), rep(20, 7), rep(21, 1), rep(22, 1), rep(31, 1))

sd(x)

# Question 28: A

# Question 29: B

# Question 30: A
x <- c(63 / 470, 153 / 470, 30 / 470, 224 / 470)

ct <- matrix(x, nrow = 2)
ct <- addmargins(ct)
rownames(ct) <- c("Smokers (E)",  "Non-Smokers (E^c)", "Total")
colnames(ct) <- c("Heart Failure (D)", "No Heart Failure (D^c)", "Total")

P_D_E <- ct[1, 1] / ct[1, 3]
P_D_Ec <- ct[2, 1] / ct[2, 3]

OE <- P_D_E / (1 - P_D_E)
OEc <- P_D_Ec / (1 - P_D_Ec)

OE / OEc

# Question 31: D
#
#          X2
#   | - | 0 | 1 | 2 |
# X | 0 | 0 | 1 | 2 |
# 1 | 1 | 1 | 2 | - |
#   | 2 | 2 | - | - |
#
P_x1_0_x2_0 <- dbinom(0, 20, 0.1) * dbinom(0, 40, 0.05)
P_x1_1_x2_0 <- dbinom(1, 20, 0.1) * dbinom(0, 40, 0.05)
P_x1_2_x2_0 <- dbinom(2, 20, 0.1) * dbinom(0, 40, 0.05)
P_x1_0_x2_1 <- dbinom(0, 20, 0.1) * dbinom(1, 40, 0.05)
P_x1_0_x2_2 <- dbinom(0, 20, 0.1) * dbinom(2, 40, 0.05)
P_x1_1_x2_1 <- dbinom(1, 20, 0.1) * dbinom(1, 40, 0.05)

P_x1_0_x2_0 + P_x1_1_x2_0 + P_x1_2_x2_0 +
  P_x1_0_x2_1 + P_x1_0_x2_2 + P_x1_1_x2_1

# Question 32: D
x <- c(rep(0, 5), rep(1, 30), rep(2, 45), rep(3, 60), rep(4, 60))

p <- 0.025
n <- length(x)

uncertainty <- qnorm(1 - p, mean = 0, sd = 1) * sqrt(var(x) / n)
lower <- mean(x) - uncertainty
upper <- mean(x) + uncertainty

c(lower, upper)

# Question 33: B

# Question 34: C

# Question 35: D

# Question 36: A
batch1 <- c(6, 7, 56, 6, 8, 22, 48, 10, 13, 14, 40, 8)
batch2 <- c(12, 16, 23, 25, 17, 13, 22, 18, 23, 20, 16, 15)

fitdistr(c(batch1, batch2), densfun = "Poisson")

# Question 37: D
skewness(c(batch1, batch2))
kurtosis(c(batch1, batch2))

# Question 38: B
batch1 <- c(5.2, 3.2, 4.9, 3.7, 2.5, 2.7, 4.1, 3.6, 6.0, 5.7, 4.6, 5.6)
batch2 <- c(2.7, 4.9, 5.2, 3.8, 3.4, 3.4, 2.8, 4.4, 4.8, 4.7, 3.6, 4.0)

cov(batch1, batch2)

# Question 39: -

# Question 40: C
