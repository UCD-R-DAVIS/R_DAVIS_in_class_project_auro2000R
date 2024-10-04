### R-Data Analysis & Visualization In Science ###
#### Homework Week 2 ###
### Aurojeet Jena ###

set.seed(15)
hw2 <- runif(50, 4, 50)
hw2 <- replace(hw2, c(4,12,22,27), NA)
hw2


# Removing all the NAs
hw2_clean <- na.omit(hw2)

# Selecting all the numbers between 14 and 38 inclusive
prob1 <- hw2_clean[hw2_clean >= 14 & hw2_clean <= 38]


# Multiplying each number in prob1 by 3
times3 <- prob1 * 3

# Adding 10 to each number in times3
plus10 <- times3 + 10


# Select every other number, starting with the first
every_other <- plus10[seq(1, length(plus10), by = 2)]

every_other