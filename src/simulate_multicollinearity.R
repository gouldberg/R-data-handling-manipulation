setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "MASS", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# The simulation example is predicting an individual's height using the length of his or her legs as predictor variables.
# Surely height is positively associated with leg length, or at least the simulation will assume it is.
# Nevertheless, once you put both leg lengths into the model, sometihng vexing will happen.
# ------------------------------------------------------------------------------

# number of individuals
N <- 100


# sim total height of each
height <- rnorm(N, 10, 2)


# leg as proportion of height
leg_prop <- runif(N, 0.4, 0.5)


# sim left and right leg as proportion + error
leg_left <- leg_prop * height + rnorm(N, 0, 0.02)
leg_right <- leg_prop * height + rnorm(N, 0, 0.02)


d <- data.frame(height, leg_left, leg_right)




# ----------
mod <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left + br * leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ), data = d
)


precis(mod, digits=3, corr=TRUE)


plot(precis(mod))



# -->
# Those posterior means and standard deviations look crazy.
# If both legs have almost identical lengths, and height is so strongly associated with leg length, but the posterior distribution so weird.
# But model fitting work correctly and the posterior distribution here is the right answer to the right but weird question:
#   - What is the value of knowing each leg's length, after already knowing the other leg's length ?
# The posteiror distribution is the answer to this question, considering every possible combination of the parameters and
# assigining relative plausibilities to every combinaion, conditional on thie model and these data.



# ----------
post <- extract.samples(mod)

plot(bl ~ br, post, col = col.alpha(rangi2, 0.1), pch = 16)


# -->
# Since both variables contain almost identical information, the posteriro is a narrow ridge of negatively correlated values.



# ----------
# The model produced a good estimate of the sum of bl and br.
sum_blbr <- post$bl + post$br
dens(sum_blbr, col = rangi2, lwd = 2, xlab = "sum of bl and br")
summary(sum_blbr)



# ----------
mod2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ), data = d
)


precis(mod2, digits=3, corr=TRUE)
summary(sum_blbr)


# -->
# the posterior mean is almost identical to the mean value of sum_blbr


par(mfrow=c(1,2))
plot(precis(mod))
plot(precis(mod2))




