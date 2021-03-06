# 3.4 Conditional probability

### 3.4.2 Computing probabilities using [R]

#### Setup
p_a_given_b <- 0.95
p_not_a_given_not_b <- 0.9
p_b <- 0.7

#### Calculate P(B|A)
p_a_given_b * p_b / (p_a_given_b * p_b + (1 - p_not_a_given_not_b) * (1 - p_b))

##### Calculate P(B^c|A^c)
p_not_a_given_not_b * (1 - p_b) / (p_not_a_given_not_b * (1 - p_b) +
  (1 - p_a_given_b) * p_b)
