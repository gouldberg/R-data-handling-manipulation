library(dplyr)
library(tidyverse)


# ---------------------------------------------------------------------------
# generate list of random values by map functions
# ---------------------------------------------------------------------------
# Generate 10 random normals for each of mu = -10, 0, 10, and 100
map(c(-10, 0, 10, 100), rnorm, n = 10)


# ----------
# Generate 5 random normals for each of mu = -3, -2, -1, 0, 1, 2, 3 
map(-3:3, rnorm, n = 5)


# ----------
# Generate 5 lists, each list has the result of runif(x)
map(1:5, runif)




