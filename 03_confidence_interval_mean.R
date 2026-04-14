# Population is unknown, create a sample
sample_exercise <- rnorm(n = 285, mean = 23, sd = 23)

# Estimate population mean with confidence 0.9, 0.95, 0.99 (the relation between variance and population size is used as an estimator of the sample variance)
alpha <- c(0.9, 0.95, 0.99)
for (a in alpha) {
  estimate <- c(mean(sample_exercise) - (qnorm(1 - a/2) * (sd(sample_exercise)/sqrt(285))), mean(sample_exercise) + (qnorm(1 - a/2) * (sd(sample_exercise)/sqrt(285))))
  print(paste("confidence ", a, "= ", estimate[1], estimate[2]))
}