setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Simulate the distribution of deviance by number of parameters
#
#   - The procedure outline is:
#       1. Suppose there's a training sample of size N.
#       2. Fit a model to the training sample, and compute the deviance on the training sample. Call this deviance D(train)
#       3, Suppose another sample of size N from the same process. This is the test sample.
#       4. Compute the deviance on the test sample.  This means using the MAP estimates from step (2) to compute the deviance for the data in the test sample.
#          Call this deviance D(test)
# ------------------------------------------------------------------------------
# We conduct the above thought experiment 10,000 times, for each of five different linear regression models.
# The model that generates the data is:
# y(i) ~ Normal(mu(i), 1)
# mu(i) = (0.15) * x1(i) - (0.4) * x2(i)
# This corresponds to a Gaussian outcome y for which the intercept is alpha = 0 and the slopes for each of two predictors are beta1 = 0.15 and beta2 = -0.4
# The models for anayzing the data are lnear regression with between 1 and 5 free parameters.
# The first model, with 1 free parameter to estimate, is just a linear regression with an unknown mean and fixed sigma = 1
# Each parameter added to the model adds a predictor variable and its beta-coefficient.

# ----------
# N = 20 case
N <- 20
kseq <- 1:5
n.cores <- 20


# You can parallelize the simulations by replacing the replicate() line with mcreplicate()
dev <- sapply(kseq, function(k){
  print(k);
  # r <- replicate(1e4, sim.train.test(N = N, k = k));
  r <- mcreplicate(1e4, sim.train.test(N = N, k = k, rho=c(0.15, -0.4), b_sigma = 100), mc.cores = n.cores);
  c(mean(r[1,]), mean(r[2,]), sd(r[1,]), sd(r[2,]))
})
  

# ----------
# Blue points and line segments: the mean plus-and-minus one standard deviation of the deviance calculated on the training sample
# Black points and line segments:  for test sample
# While the training deviance always gets better with and additional parameter, the test deviance is smallest on average for 3 parameters
# The size of the standard deviation bars may surprise you.
# While it is always true on average that deviance out-of-sample is worse thatn deviance in-sample, any individual pair of train and test samples
# may reverse the expectation.
plot(kseq, dev[1,], ylim = c(min(dev[1:2,]) - 5, max(dev[1:2]) + 10), xlim = c(1, 5.1), xlab = "number of parameters", ylab = "deviance", pch = 16, col = rangi2)
mtext(concat("N = ", N))
points(kseq + 0.1, dev[2,])


for(i in kseq){
  pts_in <- dev[1, i] + c(-1, +1) * dev[3, i]
  pts_out <- dev[2, i] + c(-1, +1) * dev[4, i]
  lines(c(i, i), pts_in, col = rangi2)
  lines(c(i, i) + 0.1, pts_out)
}


# ----------
# N = 100 case
N <- 100
kseq <- 1:5
n.cores <- 20


# You can parallelize the simulations by replacing the replicate() line with mcreplicate()
dev2 <- sapply(kseq, function(k){
  print(k);
  # r <- replicate(1e4, sim.train.test(N = N, k = k));
  r <- mcreplicate(1e4, sim.train.test(N = N, k = k, rho=c(0.15, -0.4), b_sigma = 100), mc.cores = n.cores);
  c(mean(r[1,]), mean(r[2,]), sd(r[1,]), sd(r[2,]))
})


# ----------
plot(kseq, dev2[1,], ylim = c(min(dev2[1:2,]) - 5, max(dev2[1:2]) + 10), xlim = c(1, 5.1), xlab = "number of parameters", ylab = "deviance", pch = 16, col = rangi2)
mtext(concat("N = ", N))
points(kseq + 0.1, dev2[2,])


for(i in kseq){
  pts_in <- dev2[1, i] + c(-1, +1) * dev2[3, i]
  pts_out <- dev2[2, i] + c(-1, +1) * dev2[4, i]
  lines(c(i, i), pts_in, col = rangi2)
  lines(c(i, i) + 0.1, pts_out)
}


# -->

