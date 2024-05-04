packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Creating a log version of a TF family distribution from an existing continuous gamlss.family distribution on the real line
# ------------------------------------------------------------------------------

# We take a t family distribution, i.e. Z ~ TF(mu, sigma, nu)
# and apply an exponential transform Y = exp(Z) to give Y ~ logTF(mu, sigma, nu), i.e. we create a log-t family distribution on (0, inf).

gen.Family("TF", type = "log")



# ----------
# generate 200 observations with df = nu = 10 (and default mu = 0 and sigma = 1)

set.seed(134)

( Y <- rlogTF(200, nu = 10) )



# ----------
h1 <- histDist(Y, family = logTF, nbins = 30, ylim = c(0, .65), line.wd = 2.5)




# ------------------------------------------------------------------------------
# Truncating gamlss.family distributions (truncating t-family distributions)
# ------------------------------------------------------------------------------

library(gamlss.tr)

gen.trun(par = c(0, 100), family = "TF", name = "0to100", type = "both")



# ----------
( Y <- rTF0to100(1000, mu = 80, sigma = 20, nu = 5) )

h1 <- histDist(Y, family = TF0to100, nbins = 30, xlim = c(0, 100), line.col = "darkblue", line.wd = 2.5)




# ------------------------------------------------------------------------------
# Extend GAMLSS family distribution to generate an interval-censored Weibull distribution
# ------------------------------------------------------------------------------

library(gamlss.cens)


# ----------
# An interval-censored Weibull, WEI2(mu, sigma), distribution is now generated which allows interval- (as well as left- and right-) censored
# response values
gen.cens(WEI2, type = "interval")


WEI2ic()


