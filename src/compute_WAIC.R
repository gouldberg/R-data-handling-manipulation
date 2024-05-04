setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



data(cars, package = "datasets")

str(cars)


# ------------------------------------------------------------------------------
# Compute WAIC (Widely Applicable Information Criterion
#
#   - As same as DIC, WAIC calculate an estimate of out-of-sample deviance by taking averages of log-likelihood over the posterior distribution
#     But WAIC does not require a multivariate Gaussian posterior, and it is often more accurate than DIC.
#   - The distinguishing feature of WAIC is that it is pointwise. This means that uncertainty in prediction is considered case-by-case, or point-by-point, in the data.
#     It assesses flexibility of a model with respect to fitting each observation, and then sums up across all observations.
#
#   - WAIC = -2 * (lppd - pWAIC)
#   - lppd (log-pointwise-predictive-density) = sum(log(Pr(yi))),  Pr(Yi) as the average likelihood of obbservation i in the training sample.
#     This is just a pointwise analog of deviance, averaged over the posteriror distribution.
#     If multiplied by -2, it'd be similar to the deviance, in fact.
#   - pWAIC (effective number of parameters) = sum(V(yi)),  V(yi) as the variance in log-likelihood for observation i in the training sample
# ------------------------------------------------------------------------------

# To see how the WAIC calculations actually work, consider a simple regression fit with map

m <- map(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- a + b * speed,
    a ~ dnorm(0, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 30)
  ), data = cars
)


# ----------
# sampling from posterior distribution
post <- extract.samples(m, n = 1000)
post



# ----------
# We'll need the log-likehilood of each observation i at each sample s from the posterior
n_samples <- 1000

ll <- sapply(1:n_samples,
             function(s){
               mu <- post$a[s] + post$b[s] * cars$speed
               dnorm(cars$dist, mu, post$sigma[s], log = TRUE)
             }
            )

ll


# ----------
# Now to compute lppd, but to do this with precision, we need to do all of the averaging on the log scale.
# log_sum_exp() computes the log of a sum of exponentiated terms.
n_cases <- nrow(cars)

lppd <- sapply(1:n_cases, function(i) log_sum_exp(ll[i,]) - log(n_samples))
lppd


# ----------
# effective number of parameters: pWAIC
# Note that each individual observation has its own penalty term in the pWAIC vector
# This provides an interesting opportunity to study how different observations contributing to overfitting.
pWAIC <- sapply(1:n_cases, function(i) var(ll[i,]))
pWAIC


# ----------
# Compute WAIC
-2 * (sum(lppd) - sum(pWAIC))


# ----------
# Standard error by computing the square root of number of cases multiplied by the variance over the individual observation terms in WAIC
waic_vec <- -2 * (lppd - pWAIC)
sqrt(n_cases * var(waic_vec))



# ----------
# Compute WAIC by cheating
WAIC(m)

WAIC(m, pointwise = TRUE)

