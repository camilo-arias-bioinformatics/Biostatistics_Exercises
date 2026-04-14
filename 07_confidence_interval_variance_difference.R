# Create a sample with normal distribution
sample_1 <- rnorm(n = 48, mean = 28, sd = 8)
sample_2 <- rnorm(n = 32, mean = 31, sd = 12)

# Create a function to get a confidence interval for the difference between population variances
variance_proportion <- function(alpha) {
  if (var(sample_1) > var(sample_2)) {
    var <- c(var(sample_1), var(sample_2))
    df <- c(length(sample_1) - 1, length(sample_2) - 1)
  }
  else {
    var <- c(var(sample_2), var(sample_1))
    df <- c(length(sample_2) - 1, length(sample_1) - 1)
  }
  vs <- c((var[1] * qf((1 - alpha) / 2, df[1], df[2]) / var[2]), (var[1] * qf(alpha / 2, df[1], df[2]) / var[2]))
  return(vs)
}

# Use the function for alpha = 0.95
variance_proportion(0.95)