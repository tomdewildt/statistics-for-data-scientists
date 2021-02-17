# Problem 1.1

### Setup
path <- "./data/"
file <- "demographics-synthetic.csv"

demographics_data <- read.csv(paste(path, file, sep = ""),
                      sep = ",",
                      dec = ".",
                      header = TRUE,
                      stringsAsFactors = TRUE)

### Inspect data
dim(demographics_data)
head(demographics_data)
summary(demographics_data)

### 1. Compute the mean, mode, and median of the variables Age, Weight, and
###    Voting.
mode <- function(values) {
  unique_values <- unique(values)
  unique_values[which.max(tabulate(match(values, unique_values)))]
}

mean(demographics_data$Age)
median(demographics_data$Age)
mode(demographics_data$Age)

mean(demographics_data$Weight)
median(demographics_data$Weight)
mode(demographics_data$Weight)

mean(demographics_data$Voting)
median(demographics_data$Voting)
mode(demographics_data$Voting)

### 2. Find any values that you believe are erroneous. Remove the rows that
###    contain these "errors".
summary(demographics_data)

boxplot(demographics_data$Age)
demographics_data <- demographics_data[demographics_data$Age <= 150, ]
boxplot(demographics_data$Age)

summary(demographics_data$Weight)
demographics_data <- demographics_data[!is.na(demographics_data$Weight), ]
summary(demographics_data$Weight)

unique(demographics_data$Voting)
demographics_data <- demographics_data[demographics_data$Voting <= 3, ]
unique(demographics_data$Voting)

nrow(demographics_data)

### 3. Compute the mean, mode, and median of the variable Age again.
mean(demographics_data$Age)
median(demographics_data$Age)
mode(demographics_data$Age)

### 4. Compare the (sample) variance of the variables Height and Weight.
var(demographics_data$Height)
var(demographics_data$Weight)

### 5. Use the quantile function to compute the 18th percentile of Age.
quantile(demographics_data$Age, probs = c(0.18))

### 6. Create a scatterplot relating the Weight (x-axis) and Age (y-axis) of
###    participants.
plot(demographics_data$Weight, demographics_data$Age)

### 7. Redo the same plot, but now color the points and add meaningful labels to
###    the axis.
plot(demographics_data$Weight,
     demographics_data$Age,
     col = "red",
     main = "Relation between weight & age",
     xlab = "Weight",
     ylab = "Age")

### 8. Add a horizontal line to the plot at Age = 30 and add a vertical
###    line to the plot at Weight = 90.
abline(h = 30)
abline(v = 90)

### 9. Create a boxplot comparing the distribution of Age for males and females.
plot(demographics_data$Gender, demographics_data$Age)

### 10. Create a figure with two panels, one with the scatterplot you just
###     created and one with the boxplot you just created.
par(mfrow = c(1, 2))

plot(demographics_data$Weight,
     demographics_data$Age,
     col = "red",
     main = "Relation between weight & age",
     xlab = "Weight",
     ylab = "Age")

boxplot(demographics_data$Age ~ demographics_data$Gender,
        col = c("red", "blue"),
        main = "Distribution of age for males & females",
        xlab = "Gender",
        ylab = "Age")

### 11. Create a histogram of the variable Weight. What do you think is a good
###     number of breaks?
min(demographics_data$Weight)
max(demographics_data$Weight)

par(mfrow = c(1, 1))
hist(demographics_data$Weight, breaks = "FD")

# Problem 1.2 (except part 4)

### Setup
library(e1071)

path <- "./data/"
file <- "voting-demo.csv"

voting_data <- read.csv(paste(path, file, sep = ""),
                              sep = ",",
                              dec = ".",
                              header = TRUE,
                              stringsAsFactors = TRUE)

### 1. Inspect the dataset: How many observations (rows) does it contain? And
###    how many variables (columns)? What do the variables mean?
dim(voting_data)
head(voting_data)
summary(voting_data)

### Clean the dataset
voting_data <- voting_data[!is.na(voting_data$Vote), ]
voting_data <- voting_data[!is.na(voting_data$Age), ]
voting_data <- voting_data[!is.na(voting_data$Church), ]

### 2. Write down the measurement level of each of the variables.

#### X: nominal
#### Vote: nominal
#### Age: ratio
#### Church: nominal
#### Choice: nominal
#### Educ: ordinal
#### agegr: ordinal

### 3. Compute all of the descriptive statistics described in this chapter for
###    each of the variables.
summary(voting_data)

#### Mean, median, mode
mean(voting_data$Age)
median(voting_data$Age)
mode(voting_data$Age)

#### Frequencies
transform(table(voting_data$Vote),
          cumulative = cumsum(Freq),
          relative = prop.table(Freq))
transform(table(voting_data$Church),
          cumulative = cumsum(Freq),
          relative = prop.table(Freq))
transform(table(voting_data$Choice),
          cumulative = cumsum(Freq),
          relative = prop.table(Freq))
transform(table(voting_data$agegr),
          cumulative = cumsum(Freq),
          relative = prop.table(Freq))

#### Range
max(voting_data$Age) - min(voting_data$Age)

#### Interquartile Range (IQR)
first_quartile <- quantile(voting_data$Age, probs = c(0.25))
third_quartile <- quantile(voting_data$Age, probs = c(0.75))

third_quartile - first_quartile

#### Mean Absolute Deviation (MAD)
sum(abs(voting_data$Age - mean(voting_data$Age))) / length(voting_data$Age)

#### Mean Squared Deviation (MSD)
(length(voting_data$Age) - 1) * var(voting_data$Age) / length(voting_data$Age)

#### Variance
var(voting_data$Age)

#### Standard Deviation
sd(voting_data$Age)

#### Skewness
skewness(voting_data$Age)

#### Kurtosis
kurtosis(voting_data$Age)

# Problem 1.3

### 2. Create a function to compute the mean of a variable using only the sum
###    and length functions.
compute_mean_with_sum <- function(values) {
  sum(values) / length(values)
}

compute_mean_with_sum(c(1, 2, 3, 4, 5))

### 3. Create a function to compute the mean of a variable using only the for
###    function.
compute_mean_with_loop <- function(values) {
  accumulator <- 0
  count <- 0
  for (value in values) {
    accumulator <- accumulator + value
    count <- count + 1
  }
  accumulator / count
}

compute_mean_with_loop(c(1, 2, 3, 4, 5))

### 5. Run the command x <- rnorm(100, mean=0, sd=1) to create a new variable
###    called x. What is the size of x?
x <- rnorm(100, mean = 0, sd = 1)

length(x)

### 6. Compute descriptive statistics for x that you think are useful.
summary(x)
sd(x)

### 7. Visualize the data in x. What plot do you select and why?
plot(density(x))

### 8. Now try to examine in the same way, by computing descriptives and by
###    plotting, the variable x2.

summary(x2)
sd(x2)

plot(density(x2))
