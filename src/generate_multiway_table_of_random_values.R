library(dplyr)



# ---------------------------------------------------------------------------
# generate multiway tables of random values
# ---------------------------------------------------------------------------
set.seed(1234)
dim <- c(3, 4, 5, 3)

tab <- array(rpois(prod(dim), 15), dim = dim)

tab