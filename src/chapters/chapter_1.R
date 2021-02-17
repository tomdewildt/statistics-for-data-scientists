# 1.2 Getting Started with [R]

### 1.2.1 Opening a dataset: face-data.csv
path <- "./data/"
file <- "face-data.csv"

face_data <- read.csv(paste(path, file, sep = ""),
                      sep = ",",
                      dec = ".",
                      header = TRUE,
                      stringsAsFactors = TRUE)

### 1.2.2 Some useful commands for exploring a dataset
face_data

head(face_data, 10L)
tail(face_data, 10L)

summary(face_data)
summary(face_data$rating)
summary(as.factor(face_data$rating))

dim(face_data)

### 1.2.3 Scalars, Vectors, Matrices, Data.frames, Objects
id <- 10
id

face_data$id

m <- matrix(c(1:9), nrow = 3)
m
m[2, 3]
m[3, 1]
m[1, ]
m[, 2]

id
id <- c(id, 11)
id
id[2]

face_data[3, 5]
face_data[3, ]
face_data[1, ]

face_data$gender[3]
face_data$gender[1:5]
face_data$gender[face_data$rating > 95]

# 1.3 Measurement Levels

### 1.3.1 Outliers and unrealistic values
condition <- face_data$gender == "Female" | face_data$gender == "Male"
face_data <- face_data[condition, ]
nrow(face_data)

summary(face_data$gender)
face_data$gender <- droplevels(face_data$gender)
summary(face_data$gender)

summary(face_data$age)
face_data$age <- droplevels(face_data$age)
summary(face_data$age)

# 1.4 Describing data

### 1.4.1 Frequency
t1 <- table(face_data$age)
t2 <- transform(t1, cumulative = cumsum(Freq), relative = prop.table(Freq))
t2

bins <- 5
rating_binned <- factor(cut(face_data$rating, breaks = bins))
t3 <- table(rating_binned)
t4 <- transform(t3, cumulative = cumsum(Freq), relative = prop.table(Freq))
t4

### 1.4.2 Central Tendency
mode <- function(values) {
  unique_values <- unique(values)
  unique_values[which.max(tabulate(match(values, unique_values)))]
}
mode(face_data$rating)

quantile(face_data$rating)
quantile(face_data$rating, c(.2))

x <- c(2, 4, 5, 6)
quantile(x)
quantile(x, type = 6)

### 1.4.3 Dispersion, skewness, and kurtosis
max(face_data$rating) - min(face_data$rating)

quantile(face_data$rating, c(0.75)) - quantile(face_data$rating, c(0.25))

x <- face_data$rating
sum(abs(x - mean(x))) / length(x)

# 1.5 Visualizing data

plot(face_data)
plot(face_data$rating)

### 1.5.1 Describing Nominal / Ordinal Variables
counts <- table(face_data$age)
barplot(counts)
pie(counts)

### 1.5.2 Describing Interval / Ratio Variables
boxplot(face_data$rating)

hist(face_data$rating)
hist(face_data$rating, breaks = 5)
hist(face_data$rating, breaks = 50)

plot(density(face_data$rating))

### 1.5.3 Relations between variables
plot(face_data$dim1, face_data$rating)

plot(face_data$gender, face_data$rating)

### 1.5.4 Multi-panel plots
par(mfrow = c(3, 2))
plot(face_data$gender)
plot(face_data$gender, face_data$rating)
pie(counts)
barplot(counts)
hist(face_data$dim1)
plot(density(face_data$dim2))

### 1.5.5 Plotting mathematical functions
square <- function(x) {
  x^2
}
square(3)

x <- seq(-2, 3, by = 0.1)
x

y <- square(x)
y

par(mfrow = c(1, 2))
plot(x, y)
plot(x, y, type = "l")

curve(square, xlim = c(-2, 2))

### 1.5.6 Frequently used arguments
plot(face_data$gender,
     face_data$rating,
     col = c("blue", "red"),
     lwd = 5,
     main = "Comparing ratings",
     xlab = "Gender (males and females)",
     ylim = c(1, 100))
