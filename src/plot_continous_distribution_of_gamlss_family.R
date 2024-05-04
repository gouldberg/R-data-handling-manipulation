setwd("//media//kswada//MyFiles//R_basics")

packages <- c("dplyr", "lattice", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Plot distributions of gamlss family:  continuous distributions
# ------------------------------------------------------------------------------

?gamlss.family



# ----------
# choose distribution and check basic info
# For example:  Gamma distribution
GA()



# ----------
mu <- 10
sigma <- 0.3
from <- 0.1
to <- 25


graphics.off()
ppp <- par(mfrow=c(2,2))

# PDF: probability density function
plot(function(y) dGA(y, mu = mu, sigma = sigma), from = from, to = to, ylab = "", xlab = "", main = "PDF")

# CDF
plot(function(y) pGA(y, mu = mu, sigma = sigma), from = from, to = to, ylab = "", xlab = "", main = "CDF")

# inverse cdf
plot(function(y) qGA(y, mu = mu, sigma = sigma), from = 0, to = 1, ylab = "", xlab = "", main = "Inverse CDF")

# randomly generated values
hist(rGA(100, mu = mu, sigma = sigma), ylab = "", xlab = "", main = "random samples")

par(ppp)



# ----------
# Alternatively
graphics.off()
ppp <- par(mfrow=c(2,2))

# PDF: probability density function
curve(dGA(x = x, mu = mu, sigma = sigma), from = from, to = to, ylab = "", xlab = "", main = "PDF")

# CDF
curve(pGA(q = x, mu = mu, sigma = sigma), from = from, to = to, ylab = "", xlab = "", main = "CDF")

# inverse cdf
curve(qGA(p = x, mu = mu, sigma = sigma), from = 0, to = 1, ylab = "", xlab = "", main = "Inverse CDF")

# randomly generated values
hist(rGA(100, mu = mu, sigma = sigma), ylab = "", xlab = "", main = "random samples")

par(ppp)




# ----------
# Show plots by various parameters
par(mfrow = c(2,2))
pdf.plot(family = GA, mu = mu, sigma = c(0.1, 0.5, 1, 1.5), min = 0.01, max = 20)




