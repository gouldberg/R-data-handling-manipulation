setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Only 2 sample data --> estimate parameter by using totally flat priors
# ------------------------------------------------------------------------------

# only 2 sample data
y <- c(-1, 1)


# use totally flat priors (by default)
mod <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha
  ),
  data = list(y = y),
  start = list(alpha = 0, sigma = 1), chains = 2, iter = 4000, warmup = 1000
)


# trace plot: this is wild chain
# The Markov chains seem to drift around and spike occasionally to extreme values, not stationary and not provide useful samples
plot(mod)


# We get crazy values and imploausibly wide foncidence intervals
precis(mod, digits = 3)



# ------------------------------------------------------------------------------
# Use weakly informative priors
# ------------------------------------------------------------------------------

# use weakly informative priors
mod.w <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha,
    alpha ~ dnorm(1, 10),
    sigma ~ dcauchy(0, 1)
  ),
  data = list(y = y),
  start = list(alpha = 0, sigma = 1), chains = 2, iter = 4000, warmup = 1000
)


# Much better estimation !!!
plot(mod.w)

precis(mod.w, digits = 3)