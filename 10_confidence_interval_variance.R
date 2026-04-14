# Create a function to get the estimated range of values for the variance

variance_hypothesis <- function(sample_exercise, expected_variance) {
  range <- c(qchisq(0.025, length(sample_exercise) - 1), qchisq(0.975, length(sample_exercise) - 1))
  value = (length(sample_exercise) - 1) *
          var(sample_exercise) / expected_variance
  return(paste("range = ", range[1], range[2], "value = ", value))
}

# Use the function with a sample

sample_created <- rnorm(n = 23, mean = 28, sd = 3)
variance_hypothesis(sample_created, 6)
