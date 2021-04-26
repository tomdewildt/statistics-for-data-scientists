# Problem 8.1
# In Section 8.3.6 we introduced a number of model formulas. For each formula
# write down:

### 1. The model formula using mathematics instead of [R] code.

#### Removal of intercept (β0): y = β1*x1 + ...

#### Interaction between (x1 and x2): y = β0 + β1*x1 + β2*x2 + β3*x1*x2 + ...

#### Nominal / ordinal variables (factors): y = β0 + β1*1(x1 = B) + β2*1(x2 = C)

### 2. The design matrix for n = 5. Please make sure to add the distinctive
###    part of the formula (e.g., the interaction or the dummy variable).

#### Removal of intercept (β0)
#### [ x11 x12 ... ]
#### [ x21 x22 ... ]
#### [ x31 x32 ... ]
#### [ x41 x42 ... ]
#### [ x51 x52 ... ]

#### Interaction between (x1 and x2)
#### [ 1 x11 x12 x11*x12 ... ]
#### [ 1 x21 x22 x21*x22 ... ]
#### [ 1 x31 x32 x31*x32 ... ]
#### [ 1 x41 x42 x41*x42 ... ]
#### [ 1 x51 x52 x51*x52 ... ]

#### Nominal / ordinal variables (factors)
#### [ 1 1(x11 = B) 1(x12 = C) ... ]
#### [ 1 1(x21 = B) 1(x22 = C) ... ]
#### [ 1 1(x31 = B) 1(x32 = C) ... ]
#### [ 1 1(x41 = B) 1(x42 = C) ... ]
#### [ 1 1(x51 = B) 1(x52 = C) ... ]

# Problem 8.3
# Consider the dataset called houses.csv which can be downloaded at
# http://www.nth-iteration.com/statistics-for-data-scientist/. The dataset
# contains the prices of 546 houses and some information about the houses (the
# variable names are self-explanatory).

#### Setup
path <- "./data/"
file <- "houses.csv"

houses_data <- read.csv(paste(path, file, sep = ""))

### 1. Try to create a model in [R] that predicts the house prices as accurately
###    as possible based on the other variables (or functions thereof). Start
###    off with the simple “intercept-slope” model (using lotsize as the
###    predictor) and build the model from there. Each time make sure to check
###    the design matrix X and the estimated coefficients β. To structure
###    things a bit, at least build the following models one by one:
###      * price = β0+β1*lotsize
###      * price = β0+β1*lotsize+β2*lotsize2
###      * price = β0+β1*lotsize+β2*lotsize2+β3*bedrooms
###      * price = β0+β1*lotsize+β2*lotsize2+β3*bedrooms+β4(lotsize*bedrooms)
###    For each model compute the mean squared prediction error (within-sample)
###    and make a plot that visualizes the model.
n <- nrow(houses_data)

houses_data$lotsize_squared <- houses_data$lotsize^2

#### price = β0+β1*lotsize
m1 <- lm(price ~ 1 + lotsize, data = houses_data)

design_matrix1 <- model.matrix(m1)
design_matrix1

coefficients1 <- coef(m1)
coefficients1

mse1 <- sum((houses_data$price - predict(m1, data = houses_data))^2) / n
mse1

#### price = β0+β1*lotsize+β2*lotsize2
m2 <- lm(price ~ 1 + lotsize + lotsize_squared, data = houses_data)

design_matrix2 <- model.matrix(m2)
design_matrix2

coefficients2 <- coef(m2)
coefficients2

mse2 <- sum((houses_data$price - predict(m2, data = houses_data))^2) / n
mse2

#### price = β0+β1*lotsize+β2*lotsize2+β3*bedrooms
m3 <- lm(price ~ 1 + lotsize + lotsize_squared + bedrooms, data = houses_data)

design_matrix3 <- model.matrix(m3)
design_matrix3

coefficients3 <- coef(m3)
coefficients3

mse3 <- sum((houses_data$price - predict(m3, data = houses_data))^2) / n
mse3

#### price = β0+β1*lotsize+β2*lotsize2+β3*bedrooms+β4(lotsize*bedrooms)
m4 <- lm(price ~ 1 + lotsize * bedrooms + lotsize_squared, data = houses_data)

design_matrix4 <- model.matrix(m4)
design_matrix4

coefficients4 <- coef(m4)
coefficients4

mse4 <- sum((houses_data$price - predict(m4, data = houses_data))^2) / n
mse4

### 2. Use the AIC or BIC to select the "best" model.
AIC(m1, m2, m3, m4)
BIC(m1, m2, m3, m4)

#### The AIC and the BIC both show that model 4 has the lowest value and
#### therefore we can consider model 4 to be the best according to the AIC and
#### the BIC.
