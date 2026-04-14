# Createa a function that receives a normal distribution and the expected difference between means

mean_difference <- function(sample_1, sample_2, expected_mean, equal_variances, equal_variances_value) {
  if (equal_variances == T) {
    t_value <- (mean(sample_1) - mean(sample_2)) - expected_mean /
                  sqrt((equal_variances_value / length(sample_1)) + 
                  (equal_variances_value / length(sample_2)))
  }
  else if (equal_variances == F) {
    t_value <- ((sd(sample_1)/length(sample_1)) * qt(0.975, length(sample_1)) +
                (sd(sample_2)/length(sample_2)) * qt(0.975, length(sample_2))) /
                (sd(sample_1)/length(sample_1) + (sd(sample_2)/length(sample_2)))
  }
  p <- pt(t_value, (length(sample_1) + length(sample_2) - 2))
  return(paste("probability = ", p))
}

# Create two normal distributions
sample_a <- rnorm(n = 34, mean = 28, sd = 5)
sample_b <- rnorm(n = 58, mean = 36, sd = 8)

# Use the function with a expected difference between means
mean_difference(sample_a, sample_b, 0, T, 7)
mean_difference(sample_a, sample_b, 0, F)
