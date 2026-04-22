# Create data for population
dataset <- rnorm(n = 18, mean = 1845, sd = 100)

# Use population data to obtain a power curve graph

lower_lim <- 1845 - qnorm(0.95) * sd(dataset) / sqrt(length(dataset))
upper_lim <- 1845 + pnorm(0.95) * sd(dataset) / sqrt(length(dataset))
b_list <- c()

for (a in 1:30) {
  lower_dif <- (lower_lim - (1845 - 150 + a * 10)) * sqrt(length(dataset)) / sd(dataset)
  upper_dif <- (upper_lim - (1845 - 150 + a * 10)) * sqrt(length(dataset)) / sd(dataset)
  b <- 1 - pnorm(lower_dif) - pnorm(upper_dif, lower.tail = F)
  b_list <- c(b_list, b)
}

plot( 1 - b_list)

# This plot answers the question "if the null hypothesis is false, what is the probability to reject it?"
