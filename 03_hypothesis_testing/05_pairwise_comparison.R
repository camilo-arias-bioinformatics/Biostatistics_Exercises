# Datos
differences <- rnorm(mean = 25, n = 25, sd = 5)

# Function
compare_pairs <- function(data, expected_mean) {
  value <- (mean(differences) - expected_mean) * sqrt(length(differences)) /
        sd(differences)
  prob <- pt(0.05, length(differences) - 1)
  return(prob)
}

# Try function
compare_pairs(differences, 0)
t.test(differences)

# Using the classical t test approach for the dataset generated from the differences is equally useful as running the whole intuitive process
