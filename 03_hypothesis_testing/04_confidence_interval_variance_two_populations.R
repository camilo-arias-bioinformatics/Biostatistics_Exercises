# Create a function to compare the variance of two samples

compare_variances <- function(sample_1, sample_2, one_tail) {
  df <- length(sample_1) + length(sample_2) - 2
  if (length(sample_1) >= length(sample_2)) {
    upper_value <- var(sample_1)
    lower_value <- var(sample_2)
  }
  else if (length(sample_1) < length(sample_2)) {
    upper_value <- var(sample_2)
    lower_value <- var(sample_1)
  }
  
  if (one_tail == T) {
    alpha <- 0.05
  }
  else if (one_tail == F) {
    alpha <- 0.0255
  }
  
   crit_value <- c(qchisq(alpha, df), qchisq(1 - alpha, df))
   value <- upper_value / lower_value
  return(paste("critic_values = ", crit_value[1], crit_value[2], "your value = ", value))
}

# Create samples
sample_a <- rnorm(n = 28, mean = 28, sd = 3)
sample_b <- rnorm(n = 14, mean = 23, sd = 6)

# Try the function
compare_variances(sample_a, sample_b, T)
