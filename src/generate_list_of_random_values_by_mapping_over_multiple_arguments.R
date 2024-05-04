library(dplyr)
library(tidyverse)


# ---------------------------------------------------------------------------
# generate list of random values by mapping over multiple arguments
#
# map2() iterates over two vectors in parallel
# pmap() takes a list of multiple arguments
# ---------------------------------------------------------------------------
# Generate 5 random normals for each of mu = -5, 10, -3
mu <- list(5, 10, -3)
mu %>% map(rnorm, n=5) %>% str()



# ----------
# What if you also want to vary the standard deviation ?
sigma <- list(1, 5, 10)
seq_along(mu) %>% map(~rnorm(5, mu[[.]], sigma[[.]])) %>% str()



# ----------
# We could use map2(), which iterates over two vectors in parallel
# Note that the arguments that vary for each call come before the function
map2(mu, sigma, rnorm, n=5) %>% str()



# ----------
# pmap() takes a list of multiple arguments
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)
n <- list(10, 20, 100)

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% pmap(rnorm) %>% str()



# ----------
# Since the arguments are all the same length, it makes sense to store them in a data frame
params <- tribble(
~mean, ~sd, ~n,
5,1,1,
10,5,3,
-3,10,5
)

params %>% pmap(rnorm)


