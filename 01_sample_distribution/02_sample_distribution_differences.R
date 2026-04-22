# Create two sample distributions
population_1 <- rnorm(n = 800, mean = 6, sd = 20)
population_2 <- rnorm(n = 500, mean = 9, sd = 18)

# Create a graph for the difference of possible sample distributions
sample_1 <- sample(population_1, size = 100, replace = F)
sample_2 <- sample(population_2, size = 93, replace = F)

sample_differences <- c()
for (a in sample_2) {
  sample_differences <- c(sample_differences, sample_1 - sample_2[a])
}

plot(density(sample_differences), main = "Sample mean differences")
