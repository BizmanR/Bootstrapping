# Bootstrapping
bootstrap <- read.csv("SSURVEY.csv", header = T)
View(bootstrap)

# Summary statistics
summary(bootstrap)
sd (bootstrap$schoolwork)
length(bootstrap$schoolwork)


sd (bootstrap$credits)
length(bootstrap$credits)

sd (bootstrap$rent)
length(bootstrap$rent)

sd (bootstrap$emails)
length(bootstrap$emails)

sd (bootstrap$time)
length(bootstrap$time)

sd (bootstrap$texts)
length(bootstrap$texts)


# Histogram code
hist(bootstrap$emails, main = "Distribution of emails received in the last 24 hrs",
     xlab = "number of email received", col = "indianred3")


bootmean <- rep(0,10000)
for (i in 1:10000) {
  bootmean[i] <- mean(sample(bootstrap$emails, length(bootstrap$emails), replace = TRUE))
  
}

summary(bootmean)
sd(bootmean)
hist(bootmean, main = "Bootstrap distribution of sample mean",
     xlab = "bootstrapped sample means", col = "green")


# Confidence interval using the formula method
lb <- mean(bootstrap$emails) - 2*sd(bootmean)
lb

ub <- mean(bootstrap$emails) + 2*sd(bootmean)
ub


# Confidence interval using the percentile method

quantile(bootmean, probs= c(0.025, 0.975))



# EXTRA CREDIT

bootmedian <- rep(0,10000)
for (i in 1:10000) {
  bootmedian[i] <- median(sample(bootstrap$emails, length(bootstrap$emails), replace = TRUE))
  
}

summary(bootmedian)
sd(bootmedian)
hist(bootmean, main = "Bootstrap distribution of sample mean",
     xlab = "bootstrapped sample means", col = "green")



# Confidence interval using the percentile method

quantile(bootmedian, probs= c(0.025, 0.975))

median(bootstrap$emails)

