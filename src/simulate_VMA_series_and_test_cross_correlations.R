setwd("/media/kswada/MyFiles/R/R_basics")

packages <- c("dplyr", "MTS", "tseries", "forecast")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# simulate VMA(1) time series:  z(t) = a(t) - C * a(t-1)
# where a(t) are iid bivariate normal random variates with mean zero and Cov(a(t)) = S
# ------------------------------------------------------------------------------
( C <- matrix(c(0.8, -0.3, 0.4, 0.6), ncol = 2) )
( S <- matrix(c(2.0, 0.5, 0.5, 1.0), ncol = 2) )


n <- 200
m2 <- VARMAsim(300, malags = c(1), theta = C, sigma = S)

( zt <- m2$series )



# ----------
MTSplot(zt)



# ------------------------------------------------------------------------------
# Check acf and pacf (correlogram)
#   - The forecast package improves acf, pacf, ccf functions.  The main differences are that Acf does not plot a spike at lag 0
#     when type = "correlation" (which is redundant) and the horizontal axes show lags in time units rather than seasonal units.
#   - The Acf, Pacf and Ccf functions return objects of class "acf" as described in acf from the stats package. 
# ------------------------------------------------------------------------------

# tsdisplay shows origianl, acf and pacf at in one display for univariate time series
tsdisplay(zt[,1])

tsdisplay(zt[,2])



# ----------
graphics.off()
par(mfrow=c(2,1))
forecast::Acf(zt[,1])
forecast::Acf(zt[,2])


# ----------
# bivariate time series
par(mfrow=c(1,1))
forecast::Acf(zt)



# ------------------------------------------------------------------------------
# Assess cross-correlations
# ------------------------------------------------------------------------------

# obtain the first five lags of sample CCMs of z(t)
ccm(zt, lags = 5)


ccf(zt[,1], zt[,2], lag.max = 5)




# ------------------------------------------------------------------------------
# Testing zero cross-correlation
#   - multivariate Ljung-Box test (multivariate Portmanteau test)
#   - Under the null hypothesis that lag l cross-covariance matrix  = 0 for l > 0 and the condition that y(t) is normally distributed,
#     Qk(m) (the test statistic) is asymptotically distributed as X^2(mk^2), that is, a chisquare distribution with mk^2 degrees of freedom.
#     rho(l): lag l cross-correlation matrix
#     H0: rho1 = rho2 = ... = rho10 = ... = 0 versus H1: rho(i) <> 0 for some i
# ------------------------------------------------------------------------------

# Compute Q(m) statistics
MTS::mq(zt, lag = 5)


# -->
# the time plot of p-values of the Qk(m) statistic and dashed line of the plot denotes the type I error of 5%.
# all p-values are less than 5% (null hypothesis is rejected), confirming that the series has CCMs. (not zero CCMs)

# --> at lag = 1, there should be cross-correlation between these time series







