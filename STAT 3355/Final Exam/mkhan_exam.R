#Problem 2.1
n1 <- 74
lambda_1 <- n1 / 7

n2 <- 137
lambda_2 <- n2 / 7

z <- (lambda_2 - lambda_1) / sqrt((lambda_2 / n2) + (lambda_1 / n1))
print(z)

#Problem 2.2 
n1 <- 137
n2 <- 74
p1 <- 51 / n1
p2 <- 18 / n2

p_hat <- (p1 * n1 + p2 * n2) / (n1 + n2) 
se <- sqrt(p_hat*(1-p_hat)*((1/n1) + (1/n2)))
z <- (p1-p2)/se
p_value <- 1 - pnorm(z, mean = 0, sd = 1)

#Problem 2.3
print(1-pnorm(abs(1.91), mean = 0, sd = 1))

# Problem 3.1
bob_scores <- c(75, 82, 79, 88, 84, 86, 81, 85)
n_bob <- length(bob_scores)
average_professor <- 78.8

# Problem 3.2


# Step 4: Compute the test statistic
sample_mean_bob <- mean(bob_scores)
sample_sd_bob <- sd(bob_scores)
test_statistic <- (sample_mean_bob - average_professor) / (sample_sd_bob / sqrt(n_bob))

# Step 5: Determine the critical region
critical_value <- qt(1 - 0.05, df = n_bob - 1)

# Step 6: Make a decision
if (test_statistic > critical_value) {
  cat("Reject the null hypothesis. The data supports the belief that Bob is an above-average student.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to suggest that Bob is an above-average student.\n")
}

#Problem 3.2
# Alice's homework scores
alice_scores <- c(85, 92, 78, 90, 88, 85, 87, 89)

# Bob's homework scores
bob_scores <- c(75, 82, 79, 88, 84, 86, 81, 85)

# Calculate sample mean and sample standard deviation for Alice
x_bar_alice <- mean(alice_scores)
s_alice <- sd(alice_scores)

# Calculate sample mean and sample standard deviation for Bob
x_bar_bob <- mean(bob_scores)
s_bob <- sd(bob_scores)

# Assuming equal variances
degrees_of_freedom_difference <- length(alice_scores) + length(bob_scores) - 2

# Calculate the test statistic
t_statistic_difference <- (x_bar_alice - x_bar_bob) / sqrt((s_alice^2 + s_bob^2) / length(alice_scores))

# Calculate the p-value
p_value_difference <- 1 - pt(t_statistic_difference, df = degrees_of_freedom_difference)

# Set the significance level
alpha <- 0.05

# Make a decision
if (p_value_difference < alpha) {
  cat("Reject the null hypothesis. There is enough evidence to support that Alice performed statistically better than Bob.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to support that Alice performed statistically better than Bob.\n")
}


