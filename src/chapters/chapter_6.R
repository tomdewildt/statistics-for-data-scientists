# 6.6 Exploring multiple variables using [R]

#### Setup
library(Hmisc)

set.seed(90276)

#### Create the vectors of data
gender <- c(rep("Male", times = 100), rep("Female", times = 50))
agree <- c(
  rep(1, times = 75),
  rep(0, times = 25),
  rep(1, times = 25),
  rep(0, times = 25)
)

survey_data <- data.frame("gender" = gender, "agree" = agree)

#### Create a simple table
survey_data_table <- table(survey_data$agree, survey_data$gender)
survey_data_table

#### Add margins to the table
survey_data_table_margins <- addmargins(survey_data_table)
survey_data_table_margins

#### Compute the chi-squared value (x^2)
chisq.test(survey_data_table)

#### Create two vectors of data and compute correlation (without ties in this
#### case)
x <- c(1, 2, 3, 4, 5, 6, 7)
y <- c(1, 3, 6, 2, 7, 4, 5)

rcorr(x, y, type = "spearman")

#### Create some correlated data
x <- rnorm(100, mean = 0, sd = 10)
y <- 2 * x + rnorm(100, mean = 0, sd = 10)

#### Compute the covariance
cov(x, y)

#### Compute the correlation
cor(x, y)

# 6.7 Linear regression

### 6.7.3 Regression in [R]

#### Setup
set.seed(14534)

#### First, let's generate our data and then store it in a data frame
n <- 100
x <- runif(n, min = 20, max = 260)
y <- 50000 + 2000 * x - 3.9 * x^2 + rnorm(n, mean = 0, sd = 10000)

house_data <- data.frame("Price" = y, "Size" = x)

#### Let's fit the model using the lm() function:
m1 <- lm(Price ~ Size, data = house_data)

#### We look at a summary
summary(m1)

#### This is our vector of estimated betas
beta <- coef(m1)
beta

#### And this is the estimate for the standard deviation of the errors
sigma(m1)
