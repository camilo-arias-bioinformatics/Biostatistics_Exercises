# Createa sample
sample_exercise <- rnorm(n = 51, mean = 23, sd = 1.8)

# Create a function that evaluates null hypothesis and returns probability

hypothesis_mean <- function(expected_value) {
    z <- (mean(sample_exercise) - expected_value) /
      (sd(sample_exercise) / sqrt(length(sample_exercise)))
    p <- pt(z, (length(sample_exercise) - 1))
    return(paste("probability = ", p))
}

# Use function
hypothesis_mean(25)
