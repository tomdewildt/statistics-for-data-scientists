# Problem 3.1

### 1. What is the probability that the first die shows an odd number of eyes
###    facing up?

#### Calculate P(A) = N_A / N
#### P(A) = probability of showing an odd number
#### N_A = all eyes that are odd numbers
#### N = all eyes
3 / 6

### 2. What is the probability that the sum of the eyes of the two dice is
###    eleven?

#### Calculate P(A) = N_A / N
#### P(A) = probability of sum being eleven
#### N_A = all eyes that sum to eleven
#### N = all eyes (of both dices)
2 / (6 * 6)

# Problem 3.2

### 1. What is the probability that the card is "clubs"?

#### Calculate P(A) = N_A / N
#### P(A) = probability of drawing clubs
#### N_A = all clubs
#### N = all cards
13 / 51

### 2. What is the probability that the card is a "queen"?

#### Calculate P(A) = N_A / N
#### P(A) = probability of drawing a queen
#### N_A = all queens
#### N = all cards
4 / 51

### 3. Are the events "clubs" and "queen" independent?

#### Not Independent, P(A∩B) != P(A) * P(B)
#### P(A∩B) = probability of drawing a clubs queen
#### P(A) = probability of drawing clubs
#### P(B) = probability of drawing a queen
1 / 51 == 13 / 51 * 4 / 51

# Problem 3.3

### 1. What is the probability that a randomly chosen child from this group has
###    had measles?

#### Calculate P(A) = N_A / N
#### P(A) = probability of a child having measles
#### N_A = all children having measles
#### N = all children
(9 + 6) / (18 + 15)

### 2. If we randomly choose one person from the group of 18 girls, what is the
###    probability that this girl has had measles?

#### Calculate P(A) = N_A / N
#### P(A) = probability of being a girl and having measles
#### N_A = all girls having measles
#### N = all girls
9 / 18

### 3. Are the events "boy" and "measles" in this example independent?

#### Not Independent, P(A∩B) != P(A) * P(B)
#### P(A∩B) = probability of being a boy and having measles
#### P(A) = probability of being a boy
#### P(B) = probability of having measles
6 / (18 + 15) == 15 / (18 + 15) * 6 / (18 + 15)

# Problem 3.4

### 1. What is the probability that a randomly chosen non-smoker from this group
###    developed lung cancer?

#### Calculate P(A) = N_A / N
#### P(A) = probability of being a non-smoker and having lung cancer
#### N_A = all non-smokers having lung cancer
#### N = all non-smokers
16 / 5322

### 2. What is the probability that a randomly chosen smoker from this group
###   developed lung cancer?

#### Calculate P(A) = N_A / N
#### P(A) = probability of being a smoker and having lung cancer
#### N_A = all smokers having lung cancer
#### N = all smokers
77 / 7019

### 3. Are the events "smoking" and "lung cancer" in this example independent?

#### Not Independent, P(A∩B) != P(A) * P(B)
#### P(A∩B) = probability of being a smoker and having lung cancer
#### P(A) = probability of being a smoker
#### P(B) = probability of having lung cancer
77 / (5322 + 7019) == 7019 / (5322 + 7019) * (16 + 77) / (5322 + 7019)

### 4. What is the conditional probability that the patient is a smoker when he
###    has developed lung cancer?

#### Calculate P(A|B) = P(A∩B) / P(B)
#### P(A) = probability of being a smoker
#### P(B) = probability of having lung cancer
#### P(A∩B) = N_A / N
#### N_A = all smokers having lung cancer
#### N = all participants
#### P(B) = N_B / N
#### N_B = all lung cancer patients
#### N = all participants
p_a_and_b <- 77 / (5322 + 7019)
p_b <- (16 + 77) / (5322 + 7019)

p_a_and_b / p_b

# Problem 3.7 (Part 1)

### 1. What is the probability that the patient has the disease when the patient
###    is tested positively?

#### A,A^c = positive test, negative test
#### B,B^c = disease, no disease
#### P(B) = 0.6 (population)
#### P(A|B) = 0.9 (sensitivity)
#### P(A^c|B^c) = 0.9 (specificity)
p_b <- 0.6
p_a_given_b <- 0.9
p_a_c_given_b_c <- 0.9

#### P(A∩B) = P(A|B) * P(B)
p_a_and_b <- p_a_given_b * p_b

#### P(A|B^c) = 1 - P(A^c|B^c)
p_a_given_b_c <- 1 - p_a_c_given_b_c

#### P(B_c) = 1 - P(B)
p_b_c <- 1 - p_b

#### P(A∩B^c) = P(A|B_c) * P(B_c)
p_a_and_b_c <- p_a_given_b_c * p_b_c

#### P(A) = P(A∩B) + P(A∩B^c)
p_a <- p_a_and_b + p_a_and_b_c

#### P(B|A) = P(A|B) * P(B) / P(A)
(p_a_given_b * p_b) / p_a

# Problem 3.9 (Parts 1 & 2)

### 1. What do you think is the study design that the researchers of the removal
###    of kidney stones have selected?

#### Cohort study, because the number of open surgery and small incision
#### operations is equal

### 2. Calculate the risk difference, the relative risk, and the odds ratio for
###    a successful removal of kidney stones for a small incision with respect
###    to the open surgery. Based on these results, formulate your conclusion.

#### Calculate P(D∩E) & P(D∩E^c)
p_d_and_e <- 273 / 700
p_d_and_e_c <- 289 / 700

#### Calculate P(B) & P(B^c)
p_e <- 350 / 700
p_e_c <- 1 - p_e

#### Calculate P(D|E) = P(D∩E) / P(E)
p_d_given_e <- p_d_and_e / p_e

#### Calculate P(D|E^c) = P(D∩E^c) / P(E^c)
p_d_given_e_c <- p_d_and_e_c / p_e_c

#### Calculate ER = P(D|E) - P(D|E^c)
p_d_given_e - p_d_given_e_c

#### Calculate RR = P(D|E) / P(D|E^c)
p_d_given_e / p_d_given_e_c

#### Calculate OR = (P(D|E) / 1 - P(D|E)) / (P(D|E^c) / 1 - P(D|E^c))
(p_d_given_e / (1 - p_d_given_e)) / (p_d_given_e_c / (1 - p_d_given_e_c))

#### ER < 0, RR > 1 and OR < 1. Thus the probability of a successful kidney
#### stone removal is greater for the small incision than the open surgery.
