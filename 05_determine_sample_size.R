 # Create a sample
sample_exercise <- rnorm(n = 35, mean = 56, sd = 6)

# Create a function to obtain n based on the desired error and variance
sample_size <- function(error, confidence) {
  n = ((qnorm((1 - confidence)/2))^2) * var(sample_exercise) / (error^2)
  return(n)
}

# Run function for a given variation
sample_size(2, 0.99)