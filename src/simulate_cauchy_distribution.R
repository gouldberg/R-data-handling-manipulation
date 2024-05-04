setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Simulate Cauchy distribution
# 
#   - The Cauchy has no defined mean nor variance, so the location and scale are not its mean and, say, standard deviation.
#     The reason the Cauchy has no mean and variance is that it is a very thick-tailed distribution.
#     At any moment in a Cauchy sampling process, it is possible to draw an extreme value that overwhelms all of the previous draws.
#     The consequence of this fact is that the sequence never converges to a stable mean and variance. It just keeps moving.
# ------------------------------------------------------------------------------
# Take 4 samples with same parameter
y <- rcauchy(1e4, 0, 5)
y2 <- rcauchy(1e4, 0, 5)
y3 <- rcauchy(1e4, 0, 5)
y4 <- rcauchy(1e4, 0, 5)


# running mean at each sample
mu <- sapply(1:length(y), function(i) sum(y[1:i]) / i)
mu2 <- sapply(1:length(y2), function(i) sum(y2[1:i]) / i)
mu3 <- sapply(1:length(y3), function(i) sum(y3[1:i]) / i)
mu4 <- sapply(1:length(y4), function(i) sum(y4[1:i]) / i)


# The trace of the mean is highly unpredictable
par(mfrow = c(2,2))
plot(mu, type = "l", ylim = c(-20,20))
plot(mu2, type = "l", ylim = c(-20,20))
plot(mu3, type = "l", ylim = c(-20,20))
plot(mu4, type = "l", ylim = c(-20,20))


# histogram of the sample
par(mfrow = c(2,2))
hist(mu)
hist(mu2)
hist(mu3)
hist(mu4)




# ----------
# For comparison, Gaussian
# Take 4 samples with same parameter
y <- rnorm(1e4, 0, 5)
y2 <- rnorm(1e4, 0, 5)
y3 <- rnorm(1e4, 0, 5)
y4 <- rnorm(1e4, 0, 5)


# running mean at each sample
mu <- sapply(1:length(y), function(i) sum(y[1:i]) / i)
mu2 <- sapply(1:length(y2), function(i) sum(y2[1:i]) / i)
mu3 <- sapply(1:length(y3), function(i) sum(y3[1:i]) / i)
mu4 <- sapply(1:length(y4), function(i) sum(y4[1:i]) / i)


# The trace of the mean is highly unpredictable
par(mfrow = c(2,2))
plot(mu, type = "l", ylim = c(-5,5))
plot(mu2, type = "l", ylim = c(-5,5))
plot(mu3, type = "l", ylim = c(-5,5))
plot(mu4, type = "l", ylim = c(-5,5))


# histogram of the sample
par(mfrow = c(2,2))
hist(mu)
hist(mu2)
hist(mu3)
hist(mu4)
