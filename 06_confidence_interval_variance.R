# Create a sample with normal distribution
sample_exercise <- rnorm(n = 48, mean = 28, sd = 8)

# Create a function to get a confidence interval for the population variance
variance_estimation <- function(alpha) {
  ve <- c((length(sample_exercise)-1) * var(sample_exercise) / qchisq(alpha/2, length(sample_exercise)-1), (length(sample_exercise)-1) * var(sample_exercise) / qchisq((1 - alpha)/2, length(sample_exercise)-1))
  return(ve)
}

# Use the function for alpha = 0.95
variance_estimation(0.95)
