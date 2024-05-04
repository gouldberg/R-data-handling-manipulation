setwd("//media//kswada//MyFiles//R_basics")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# One run of t test wtih autocorrelated data
# ------------------------------------------------------------------------------
lambda <- 0.4 #Autocorrelation term

set.seed(123)
Y <- numeric(20)
for (i in 2:20) Y[i] <- lambda * Y[i - 1] + rnorm(1)
Y <- Y[11:20]

Y.ttest <- t.test(Y, alternative = "two.sided")



# ----------
# Assign the value 1 to a Type I error
TypeI <- as.numeric(Y.ttest$p.value < 0.05)
Ybar <- mean(Y)
Yse <- sqrt(var(Y) / 10)
c(TypeI, Ybar, Yse)



# ------------------------------------------------------------------------------
# Monte Carlo simulation of t test wtih autocorrelated data
# ------------------------------------------------------------------------------
set.seed(123)
lambda <- 0.4

ttest <- function(lambda){
  Y <- numeric(20)
  for (i in 2:20) Y[i] <- lambda * Y[i - 1] + rnorm(1, sd = 1)
  Y <- Y[11:20]
  Y.ttest <- t.test(Y,alternative = "two.sided")
  TypeI <- as.numeric(Y.ttest$p.value < 0.05)
  Ybar <- mean(Y)
  Yse <- sqrt(var(Y) / 10)
  return(c(TypeI, Ybar, Yse))
}

U <- replicate(10000, ttest(lambda))
mean(U[1,]) # Type I error rate
mean(U[2,]) # Mean value of Ybar
mean(U[3,]) # Mean est. standard error
sd(U[2,]) # Sample std. dev. of Ybar



# ----------
# Plot of Type I errors vs. lambda
error.rate <- numeric(10)
mean.Y <- numeric(10)
sd.Y <- numeric(10)
sem.Y <- numeric(10)
lambda <- seq(0.0, 0.9, 0.1)
ttest <- function(lambda){
  Y <- numeric(20)
  for (i in 2:20) Y[i] <- lambda * Y[i - 1] + rnorm(1, sd = 1)
  Y <- Y[11:20]
  t.ttest <- t.test(Y,alternative = "two.sided")
  TypeI <- as.numeric(t.ttest$p.value < 0.05)
  Ybar <- mean(Y)
  Yse <- sqrt(var(Y) / 10)
  return(c(TypeI, Ybar, Yse))
}

for (j in 1:10){
  set.seed(1)
  U <- replicate(10000, ttest(lambda[j]))
  error.rate[j] <- mean(U[1,])
  mean.Y[j] <- mean(U[2,])
  sem.Y[j] <- mean(U[3,])
  sd.Y[j] <- sd(U[2,])
}


graphics.off()
par(mfrow = c(2,2))
par(mai = c(0.4, 0.4, 0.4, 0.4))  #Bottom, Left, Top, Right
plot(lambda, error.rate, xlab = expression(lambda), ylab = "Error Rate", cex.lab = 1.5)
title(main = "Error Rate", cex.main = 2)
plot(lambda, mean.Y, xlab = expression(lambda), ylab = expression(Mean~italic(bar(Y))), ylim = c(-0.1, 0.1), cex.lab = 1.5)
title(main = "Mean of Sample Means", cex.main = 2)
plot(lambda, sem.Y, xlab = expression(lambda), ylab = expression(Mean~"s{"*italic(bar(Y))*"}"), ylim = c(0,0.5), cex.lab = 1.5)
title(main = "Mean Estimated Standard Error", cex.main = 2)
plot(lambda,sd.Y, xlab = expression(lambda), ylab = expression("Std. Dev. of "*italic(bar(Y))), ylim = c(0,2), cex.lab = 1.5)
title(main = "Standard Deviation of Mean", cex.main = 2)


# -->
# The estimate of Y-bar of the population mean is unbiased whether or not the errors are autocorrelated.

# On the other hand,
# The effect of temporal autocorrelation, that the s / sqrt(n) underestimates the true standard deviation of Y,
# means that the denominator of the t statistic is smaller than it should be to properly take into account the variability of Y,
# and therefore the t statistic is larger than it would be if the random variables Y(i) were uncorrelated.

# The effect of inflating the value of the t statistic is to increase the Type I error rate, the fraction of times
# that the test exceeds the threshold for rejection of the null hypothesis.

# When the data are autocorrelated, each data value provides some information about the other data values near it.
# When the data are independent, each of the n data values carries only information about itself, and,,
# in statistical terms, brings a full defree of freedom to the statistic.
# Each autocorrelated data value, however, brings less than a full defree of freedom, and thus the effective sample size,
# which is in the denominator is not n but rather a value less than that.

