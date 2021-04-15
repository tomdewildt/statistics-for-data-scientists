# Problem 7.1

### Setup
path <- "./data/"
file <- "voting-demo.csv"

voting_data <- read.csv(paste(path, file, sep = ""),
                        sep = ",",
                        dec = ".",
                        header = TRUE,
                        stringsAsFactors = TRUE)

### 1. Look at the frequency distribution of the variable Choice. Use a
###    non-parametric bootstrap (with K = 10000) to create a histogram of the
###    distribution of the proportion of people who vote for the SPD. Does this
###    follow a bell curve?
k <- 10000
n <- nrow(voting_data)

proportions <- rep(NA, times = k)
for (k in 1:k) {
  resample <- sample(voting_data$Choice, size = n, replace = TRUE)

  proportions[k] <- sum(resample == "SPD") / n
}

hist(proportions, breaks = 50, freq = FALSE)

#### Yes, the distribution of the variable choice follows a bell curve

# Problem 7.2
# Consider the data of the high school children. The variable of interest is the
# allowance that children receive per week in euros. We are only interested in
# those children that do receive an allowance (thus eliminate the children that
# do not get any allowance).

### Setup
path <- "./data/"
file <- "high-school.csv"

high_school_data <- read.csv(paste(path, file, sep = ""))

#### Remove cases that do not receive an allowance
high_school_data <- high_school_data[high_school_data$ALLOWANCE > 0, ]

### 1. Test if the mean allowance is equal to 10 euros per month using a
###    two-sided test.
t.test(high_school_data$ALLOWANCE, alternative = "two.sided", mu = 10)

### 2. Test if the mean allowance is equivalent to 10 euros with equivalence
###    margin D = 2 euros.
s <- var(high_school_data$ALLOWANCE)
n <- length(high_school_data$ALLOWANCE)
a <- 0.05

#### Calculate ((ȳ-mu_0) - Z_1-a * sqrt(S^2/n), (ȳ-mu_0) + Z_1-a * sqrt(S^2/n)]
uncertainty <- qnorm(1 - a, mean = 0, sd = 1) * sqrt(s / n)
difference <- mean(high_school_data$ALLOWANCE) - 10

lower <- difference - uncertainty
upper <- difference + uncertainty
ci <- c(lower, upper)

#### CI falls inside the range therefore we reject H0
-2 < ci[1] & ci[2] < 2

#### The mean allowance is equivalent to 10 euros

### 3. Investigate if the variable allowance is normally distributed.
qqnorm(high_school_data$ALLOWANCE)
qqline(high_school_data$ALLOWANCE)

#### No, the variable allowance is not normally distributed

### 4. Test if the mean log transformed allowances of boys and girls are equal
###    using a t-test. Would you use equal or unequal variances? Why?
high_school_data$LOG_ALLOWANCE <- log(high_school_data$ALLOWANCE) # nolint

high_school_boys_data <- high_school_data[high_school_data$GENDER == "Boy", ]
high_school_girls_data <- high_school_data[high_school_data$GENDER == "Girl", ]

#### Test if log allowance variances are equal
var.test(
  high_school_boys_data$LOG_ALLOWANCE,
  high_school_girls_data$LOG_ALLOWANCE
)

#### No, the variances are not equal because the p-value = 2.2e-16 which is less
#### then 0.05

#### Test if log allowance means are equal
t.test(
  high_school_boys_data$LOG_ALLOWANCE,
  high_school_girls_data$LOG_ALLOWANCE,
  alternative = "two.sided",
  var.equal = FALSE
)

#### No, the log allowance means between boys and girls are not equal because
#### the p-value = 2.723e-13 which is less then 0.05

### 5. Create a box plot of the log transformed allowance. How many observations
###    are considered outliers according to Tukey’s criterion?
boxplot(high_school_data$LOG_ALLOWANCE)

q1 <- quantile(high_school_data$LOG_ALLOWANCE, 0.25)
q3 <- quantile(high_school_data$LOG_ALLOWANCE, 0.75)
iqr <- q3 - q1

lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

sum(high_school_data$LOG_ALLOWANCE < lower
    | high_school_data$LOG_ALLOWANCE > upper)

# Problem 7.3
# This time the variable of interest is the amount of money that children earn
# per week, which is the variable WORK. We will focus on those children who earn
# money.

### Setup
library(outliers)

#### Remove cases that do not work
high_school_data <- high_school_data[high_school_data$WORK > 0, ]

#### Create log work variable
high_school_data$LOG_WORK <- log(high_school_data$WORK) # nolint

### 1. Investigate if the log transformed amount of money for boys is normally
###    distributed.
high_school_boys_data <- high_school_data[high_school_data$GENDER == "Boy", ]

qqnorm(high_school_boys_data$LOG_WORK)
qqline(high_school_boys_data$LOG_WORK)

#### Yes, the log amount of money for boys is normally distributed

### 2. Investigate if the log transformed amount of money for girls has an
###    outlier.
high_school_girls_data <- high_school_data[high_school_data$GENDER == "Girl", ]

grubbs.test(high_school_girls_data$LOG_WORK)

#### Yes, the log amount of money for girls does contain an outlier because the
#### p-value = 0.9988 which is larger then 0.05

### 3. Is there a difference in the average log transformed amount of money
###    between boys and girls?

#### Test if log amount of money variances are equal
var.test(
  high_school_boys_data$LOG_WORK,
  high_school_girls_data$LOG_WORK
)

#### No, the variances are not equal because the p-value = 2.2e-16 which is less
#### then 0.05

#### Test if log amount of money means are equal
t.test(
  high_school_boys_data$LOG_WORK,
  high_school_girls_data$LOG_WORK,
  alternative = "two.sided",
  var.equal = FALSE
)

#### Yes, there is a difference between the log amount of money for boys and
#### girls because the p-value = 2.2e-16 which is less then 0.05

### 5. Do breakfast eaters earn more money than non-breakfast eaters?
high_school_breakfast_data <- high_school_data[
  high_school_data$BREAKFAST == "Yes",
]
high_school_no_breakfast_data <- high_school_data[
  high_school_data$BREAKFAST == "No",
]

#### Test if log amount of money variances are equal
var.test(
  high_school_breakfast_data$LOG_WORK,
  high_school_no_breakfast_data$LOG_WORK
)

#### No, the variances are not equal because the p-value = 4.159e-06 which is
#### less then 0.05

#### Test if log amount of money are greater
t.test(
  high_school_breakfast_data$LOG_WORK,
  high_school_no_breakfast_data$LOG_WORK,
  alternative = "greater",
  var.equal = FALSE
)

#### Yes, breakfast eaters earn more money than non-breakfast eaters because the
#### p-value is 1 which is greater then 0.05
