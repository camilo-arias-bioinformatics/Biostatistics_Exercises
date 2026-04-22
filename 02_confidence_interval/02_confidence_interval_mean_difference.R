# Population is unknown, create samples
sample_1 <- rnorm(n = 45, mean = 23, sd = 23)
sample_2 <- rnorm(n = 83, mean = 58, sd = 28)

# Estimate population mean with confidence 0.9, 0.95, 0.99 (the relation between variance and population size is used as an estimator of the sample variance)
alpha <- c(0.9, 0.95, 0.99)
for (a in alpha) {
  estimate <- c((mean(sample_1) - mean(sample_2)) - (qnorm(1 - a/2) * ((sd(sample_1)/45) + (sd(sample_2)/83))), (mean(sample_1) - mean(sample_2)) + (qnorm(1 - a/2) * ((sd(sample_1)/45) + (sd(sample_2)/83))))
  print(paste("confidence ", a, "= ", estimate[1], estimate[2]))
}
