setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "MASS", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Post-Treatment Bias:  mistaken inferrences arising from including variables that are consequences of other variables.
# The language "post-treatment" comes from thinking about experimental designs, but the problem also applies to observational studies.
#
# Example:
#   - Suppose that you are growing some plants in a greenhouse. You want to know the difference in growth under different antifungal soil treatments,
#     because fungus on the plants tends to reduce their growth.
#     Plants are initially seeded and sprout. Their heights are measured. Then different soil treatments are applied.
#     Final measures are the height of the plant and the presence of fungus.
#     If your goal is to make a causal inference about hte treatment, you should NOT inlcude the presence of fungus, because it is a POST-TREATMENT effect.
# ------------------------------------------------------------------------------

# number of plants
N <- 100


# sim initial heights
h0 <- rnorm(N, 10, 2)


# assign treatments and simulate fungus and growth
treatment <- rep(0:1, eahc = N/2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment * 0.4)
h1 <- h0 + rnorm(N, 5 - 3 * fungus)


d <- data.frame(h0 = h0, h1 = h1, treatment = treatment, fungus = fungus)
d



# ----------
mod <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh * h0 + bt * treatment + bf * fungus,
    a ~ dnorm(10, 100),
    c(bh, bt, bf) ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ), data = d
)


precis(mod, digits=3, corr=TRUE)

plot(precis(mod))



# -->
# The effect of treatment (bt) is very small, and fungus (bf) have imprtant effects.
# This model is answering to the question:  Once we already know whether or not a plant developed fungus, does soil treatment matter ?
# The answer is "no", because soil treatment has its effect on growth through reducing fungus.
# But we actually wanto to know, based on the design of the experiment, is the impact of treatment on growth.


# ----------
mod2 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh * h0 + bt * treatment,
    a ~ dnorm(10, 100),
    c(bh, bt) ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ), data = d
)


precis(mod2, digits=3, corr=TRUE)


par(mfrow=c(1,2))
plot(precis(mod))
plot(precis(mod2))


# -->
# Now the impact of treatment is strong and positive, as it should be.



# ----------
# BUT THE MODEL COMPARISON DOES NOT HELP:  since the model is answering the wrong questions !!!
compare(mod, mod2)


