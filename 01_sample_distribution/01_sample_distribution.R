# Create a population with normal distribution
population <- rnorm(n = 985, mean = 38, sd = 10)

# Obtain samples with repetition and its distribution
samples_replace <- c()
for (a in 1:100) {
  samples_replace[a] <- mean(sample(population, size = 50, replace = T))
}

plot(density(samples_replace), main = "Sample mean density (replacement)")

# Obtain samples without repetition
samples_no_replace <- c()
for (a in 1:100) {
  samples_no_replace[a] <- mean(sample(population, size = 50, replace = F))
}

plot(density(samples_no_replace), main = "Sample mean distribution (no replacement)")
