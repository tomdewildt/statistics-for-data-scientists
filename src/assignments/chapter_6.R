# Problem 6.1

### Setup
path <- "./data/"
file <- "voting-demo.csv"

voting_data <- read.csv(paste(path, file, sep = ""),
                        sep = ",",
                        dec = ".",
                        header = TRUE,
                        stringsAsFactors = TRUE)

### 1. Make a contingency table of Vote by Choice.
voting_data_table <- table(voting_data$Vote, voting_data$Choice)
voting_data_table

voting_data_table_margins <- addmargins(voting_data_table)
voting_data_table_margins

n <- voting_data_table_margins[3, 4]

### 2. Compute the chi-squared (x^2), phi (correlation coefficient), and
###    Cramer's V (association) value for this table. Do so using [R] and do so
###    by hand.

#### Compute chi-squared (x^2)
chi_squared <- unname(chisq.test(voting_data_table)$statistic)
chi_squared

#### Compute phi (correlation coefficient)
phi <- sqrt(chi_squared / n)
phi

#### Compute Cramer's V (association)
t <- min(nrow(voting_data_table) - 1, ncol(voting_data_table) - 1)
v <- sqrt(chi_squared / (n * t))
v

#### Observed frequencies table (O)
#### |              | CDU/CSU | FDP | SDP | Sum |
#### |--------------|---------|-----|-----|-----|
#### | Did not vote | 102     | 45  | 99  | 246 |
#### | Did vote     | 230     | 86  | 186 | 502 |
#### | Sum          | 332     | 131 | 285 | 748 |

#### Probabilities table (P)
#### |              | CDU/CSU | FDP     | SDP     | Total   |
#### |--------------|---------|---------|---------|---------|
#### | Did not vote | 0.15    | 0.06    | 0.13    | 246/748 |
#### | Did vote     | 0.30    | 0.11    | 0.26    | 502/748 |
#### | Total        | 332/748 | 131/748 | 285/748 | 1       |
####
#### P(A∩B) = P(A) * P(B)

#### Expected frequencies table (E)
#### |              | CDU/CSU | FDP   | SDP    | Total |
#### |--------------|---------|-------|--------|-------|
#### | Did not vote | 109.19  | 48.08 | 93.72  | 246   |
#### | Did vote     | 222.81  | 87.92 | 191.27 | 502   |
#### | Total        | 332     | 131   | 285    | 748   |

#### Calculate chi-squared (x^2)

#### x^2 = sum_i (O_i - E_i)^2 / E_i
#### x^2 = (102 - 109.19)^2 / 109.19 + ... + (186 - 191.27)^2 / 191.27 = 1.27

#### Calculate phi (correlation coefficient)

#### phi = sqrt(x^2 / n)
#### phi = sqrt(1.27 / 748) = 0.41

#### Calculate Cramer's V (association)

#### V = sqrt(x^2 / (n * t))
#### t = min(rows - 1, columns - 1)
####
#### t = min(2 - 1, 3 - 1) = 1
#### V = sqrt(1.27 / 748 * 1) = 0.41

### 4. Create a new variable called Age2, which is the age in months, using
###    Age2 <- 12 * Age. What is the correlation between Age and Age2?
age_in_years <- voting_data$Age
age_in_months <- 12 * age_in_years

plot(age_in_months, age_in_years)
cor(age_in_months, age_in_years, use = "complete.obs")

### 5. Now, let us add some noise to Age2 using Age2 <- Age2 +
###    rnorm(length(Age2), mean = 0, sd = x) where you choose different values
###    for x. Each time plot the relationship and compute the correlation
###    coefficient.
x <- 100

age_with_noise <- age_in_months + rnorm(
  length(age_in_months),
  mean = 0,
  sd = x
)

plot(age_with_noise, age_in_years)
cor(age_with_noise, age_in_years, use = "complete.obs")

# Problem 6.2
probabilities <- c(2 / 12, 1 / 12, 3 / 12, 1 / 12, 1 / 12, 4 / 12)
contingency_table <- matrix(probabilities, nrow = 2)

### 1. What is the (marginal) expectation of X?
px_0 <- sum(contingency_table[1, ])
px_1 <- sum(contingency_table[2, ])

ex <- 0 * px_0 + 1 * px_1
ex

### 2. What is the conditional expectation of Y|X = 1?
pxy_1_0 <- contingency_table[2, 1] / fx_1
pxy_1_1 <- contingency_table[2, 2] / fx_1
pxy_1_2 <- contingency_table[2, 3] / fx_1
                                           
cex <- 0 * pxy_1_0 + 1 * pxy_1_1 + 2 * pxy_1_2
cex

### 3. Compute E(XY)
pxy_0_0 <- contingency_table[1, 1]
pxy_0_1 <- contingency_table[1, 2]
pxy_0_2 <- contingency_table[1, 3]
pxy_1_0 <- contingency_table[2, 1]
pxy_1_1 <- contingency_table[2, 2]
pxy_1_2 <- contingency_table[2, 3]

exy_0 <- 0 * 0 * pxy_0_0 + 0 * 1 * pxy_0_1 + 0 * 2 * pxy_0_2
exy_1 <- 1 * 0 * pxy_1_0 + 1 * 1 * pxy_1_1 + 1 * 2 * pxy_1_2
exy <- exy_0 + exy_1
exy

### 4. Are X and Y independent?
px_0 <- sum(contingency_table[1, ])
px_1 <- sum(contingency_table[2, ])
py_0 <- sum(contingency_table[, 1])
py_1 <- sum(contingency_table[, 2])

#### Pxy(0, 0) != Px(0) * Py(0), so X and Y are not independent
pxy_0_0 == px_0 * py_0
