setwd("//media//kswada//MyFiles//R_basics")

packages <- c("dplyr", "lattice", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Plot distributions of gamlss family:  discrete distributions
# ------------------------------------------------------------------------------

?gamlss.family



# ----------
# choose distribution and check basic info
# For example:  Negative Binomial Distribution
NBI()



# ----------
mu <- 10
sigma <- 0.5
from <- 0
to <- 40


graphics.off()
ppp <- par(mfrow=c(2,2))

# PDF: probability density function
plot(function(y) dNBI(y, mu = mu, sigma = sigma), from = from, to = to, n = to + 1, type = "h", ylab = "", xlab = "", main = "PDF")

# CDF
cdf <- stepfun(0:(to-1), c(0, pNBI(0:(to-1), mu = mu, sigma = sigma)), f = 0)
plot(cdf, ylab = "", xlab = "", main = "CDF", do.points = FALSE)

# inverse cdf
invcdf <- stepfun(seq(0.01, 0.99, length = to - 1), qNBI(seq(0.01, 0.99, length = to), mu = mu, sigma = sigma), f = 0)
plot(invcdf, ylab = "", xlab = "", main = "inverse CDF", do.points = FALSE)

# randomly generated values
tN <- table(Ni <- rNBI(1000, mu = mu, sigma = sigma))
r <- barplot(tN, col = "lightblue")

par(ppp)



# ----------
# Show plots by various parameters
par(mfrow = c(2,2))
pdf.plot(family = NBI, mu = mu, sigma = c(0.1, 0.5, 1, 1.5), min = from, max = to)




