# Problem 2.1

### Setup
population_size <- 9
sample_size <- 3

population <- 1:population_size

### 1. How many subsets of three units can be formed ignoring the time
###    structure?
simple_random_samples <- combn(population_size, sample_size)
simple_random_samples

unique_simple_random_samples <- ncol(simple_random_samples)
unique_simple_random_samples

### 2. Considering the time structure, how many systematic subsets of size 3 can
###    be formed?
systematic_samples <- rbind(population[1:3],
                            population[4:6],
                            population[7:9])
systematic_samples

unique_systematic_samples <- ncol(systematic_samples)
unique_systematic_samples

### 3. Considering the time structure, how many stratified subsets of size 3 can
###    be formed?
stratified_samples <- unname(t(expand.grid(population[1:3],
                                           population[4:6],
                                           population[7:9])))
stratified_samples

unique_stratified_samples <- ncol(stratified_samples)
unique_stratified_samples

### 4. For assignments (1), (2), and (3) separately, use [R] to select one
###    random subset from all possible subsets that you have formed.
simple_random_sample_index <- sample(1:unique_simple_random_samples, size = 1)
simple_random_sample_index

systematic_sample_index <- sample(1:unique_systematic_samples, size = 1)
systematic_sample_index

stratified_sample_index <- sample(1:unique_stratified_samples, size = 1)
stratified_sample_index

### 5. Use [R] to draw directly from the population, without creating any
###    subsets, for simple random sampling, systematic sampling, and stratified
###    sampling.
population_group_1 <- population[1:3]
population_group_2 <- population[4:6]
population_group_3 <- population[7:9]

simple_random_sample <- sample(population, size = 3, replace = FALSE)
simple_random_sample

systematic_sample <- rep(NA, 3)
systematic_sample[1] <- sample(population_group_1, size = 1)
systematic_sample[2] <- systematic_sample[1] + 3
systematic_sample[3] <- systematic_sample[2] + 3
systematic_sample

stratified_sample <- rep(NA, 3)
stratified_sample[1] <- sample(population_group_1, size = 1)
stratified_sample[2] <- sample(population_group_2, size = 1)
stratified_sample[3] <- sample(population_group_3, size = 1)
stratified_sample

# Problem 2.2

### Setup
school <- 1:6
students <- c(59, 28, 90, 44, 36, 57)
not_immunized <- c(4, 5, 3, 3, 7, 8)

student_data <- as.data.frame(cbind(school, students, not_immunized))
student_data

population_size <- nrow(student_data)
sample_size <- 3

theta <- sum(student_data$not_immunized) / sum(student_data$students)
theta

### 1. How many possible simple random samples of 3 schools are there? Make a
###    collection of all possible simple random samples of 3 schools and
###    calculate the bias, standard error, and mean square error of this
###    sampling approach.

#### Unique samples
samples <- combn(population_size, sample_size)
samples

unique_samples <- ncol(samples)
unique_samples

#### Estimate population mean by calculating the mean for each sample
estimators <- rep(NA, unique_samples)
for (i in 1:unique_samples) {
  sample_indices <- samples[, i]
  sample_data <- student_data[sample_indices, ]

  estimators[i] <- sum(sample_data$not_immunized) / sum(sample_data$students)
}

#### Calculate Bias, MSE, SE
bias <- mean(estimators) - theta
mse <- mean((estimators - theta)^2)
se <- sqrt(mean((estimators - mean(estimators))^2))

#### Show Bias, MSE, SE
data.frame(bias = bias, mse = mse, se = se)

### 2. Now stratify the schools in three strata: Stratum 1 is {1, 3}, stratum 2
###    is {4, 6}, and stratum 3 is {2, 5}. How many possible stratified samples
###    of 3 schools are there given these strata? Make a collection of all
###    possible stratified samples of 3 schools and calculate the bias, standard
###    error, and mean square error of this stratified sampling approach.

#### Unique samples
samples <- unname(t(expand.grid(c(1, 3), c(4, 6), c(2, 5))))
samples

unique_samples <- ncol(samples)
unique_samples

#### Estimate population mean by calculating the mean for each sample
estimators <- rep(NA, unique_samples)
for (i in 1:unique_samples) {
  sample_indices <- samples[, i]
  sample_data <- student_data[sample_indices, ]

  estimators[i] <- sum(sample_data$not_immunized) / sum(sample_data$students)
}

#### Calculate Bias, MSE, SE
bias <- mean(estimators) - theta
mse <- mean((estimators - theta)^2)
se <- sqrt(mean((estimators - mean(estimators))^2))

#### Show Bias, MSE, SE
data.frame(bias = bias, mse = mse, se = se)

### 3. Next, stratify the schools as follows: Stratum 1 is {1, 2}, stratum 2 is
###    {3, 5}, and stratum 3 is {4, 6}. Make a collection of all possible
###    stratified samples of 3 schools and calculate the bias, standard error,
###    and mean square error of this alternative stratified sampling approach.

#### Unique samples
samples <- unname(t(expand.grid(c(1, 2), c(3, 5), c(4, 6))))
samples

unique_samples <- ncol(samples)
unique_samples

#### Estimate population mean by calculating the mean for each sample
estimators <- rep(NA, unique_samples)
for (i in 1:unique_samples) {
  sample_indices <- samples[, i]
  sample_data <- student_data[sample_indices, ]

  estimators[i] <- sum(sample_data$not_immunized) / sum(sample_data$students)
}

#### Calculate Bias, MSE, SE
bias <- mean(estimators) - theta
mse <- mean((estimators - theta)^2)
se <- sqrt(mean((estimators - mean(estimators))^2))

#### Show Bias, MSE, SE
data.frame(bias = bias, mse = mse, se = se)

### 4. Which sampling procedure would you prefer if you have to choose from the
###    three options in (1), (2), or (3)?

#### The sampling procedure from option 2 is the best because the bias, MSE and
#### SE from this procedure are the smallest.

# Problem 2.3

### Setup
path <- "./data/"
file <- "high-school.csv"

high_school_data <- read.csv(paste(path, file, sep = ""))

### 1. For the numerical variables, what are the population means and population
###    variances?
population_size <- nrow(high_school_data)

numerical <- c(4:8, 10:11)
population_means <- apply(high_school_data[, numerical], 2, mean)
population_means

numerical <- c(4:8, 10:11)
population_variances <- apply(high_school_data[, numerical], 2, function(x) {
  # Multiply 'var' by (N - 1) / N, because 'var' is built for sample variance
  var(x) * (population_size - 1) / population_size
})
population_variances

### 2. What are the population proportions of children that do not spend time on
###    sports, watch television or computer (determine this for the three
###    variables separately)?
high_school_no_sports <- as.numeric(high_school_data$SPORTS == 0)
high_school_no_tv <- as.numeric(high_school_data$TV == 0)
high_school_no_computer <- as.numeric(high_school_data$COMPUTER == 0)

high_school_binary <- data.frame(NO_SPORTS = high_school_no_sports,
                                 NO_TV = high_school_no_tv,
                                 NO_COMPUTER = high_school_no_computer)

population_proportions <- apply(high_school_binary, 2, mean)
population_proportions

### 4. Use [R] to draw a simple random sample of n = 1200 children.
set.seed(19670912)

population_size <- nrow(high_school_data)
sample_size <- 1200

sample_indices <- sample(high_school_data$Nr,
                         size = sample_size,
                         replace = FALSE)
sample_data <- high_school_data[sample_indices, ]

#### a. Estimate the means of the numerical variables and provide an estimate of
####    the corresponding standard errors.
numerical <- c(4:8, 10:11)

sample_means <- apply(sample_data[, numerical], 2, mean)
sample_means

sample_variances <- apply(sample_data[, numerical], 2, var)
sample_variances

sample_ses <- sapply(sample_variances, function(x) {
  sqrt(((population_size - sample_size) * x) / (population_size * sample_size))
})
sample_ses

#### b. Estimate the proportions of children that do not spend time on sports,
####    watch television or computer and provide the standard errors.
sample_no_sports <- as.numeric(sample_data$SPORTS == 0)
sample_no_tv <- as.numeric(sample_data$TV == 0)
sample_no_computer <- as.numeric(sample_data$COMPUTER == 0)

sample_binary <- data.frame(NO_SPORTS = sample_no_sports,
                            NO_TV = sample_no_tv,
                            NO_COMPUTER = sample_no_computer)

sample_proportions <- apply(sample_binary, 2, mean)
sample_proportions

sample_ses <- sapply(sample_proportions, function(x) {
  sqrt(((population_size - sample_size) * (x * (1 - x))) /
         (population_size * sample_size))
})
sample_ses
