library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# Unknown Output Length
#
# convert list to vector
# ---------------------------------------------------------------------------
# You want to simulate some random vectors of random lengths
# You might be tempted to solve this problem by progressively growin the vector.
# This is "quadratic" (O(n^2)) behavior, which means that a loop with 3 times as many elements would take nine (3^2) time as long to run.
means <- c(0, 1, 4)

output <- double()

for(i in seq_along(means)){
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}

str(output)


# ----------
# Better solution is to save the results in a list, and then comobine into a single vector after the loop is done
means <- c(0, 1, 4)

output <- vector("list", length(means))

for(i in seq_along(means)){
  n <- sample(100, 1)
  output[[i]] <- rnorm(n, means[[i]])
}

str(output)


# flattern a list of vectors into a single vector
output_final <- unlist(output)
output_final


# stricter option is to use purrr::flatten_dbl(): throw an error if the input isn't a list of doubles
output_final <- purrr::flatten_dbl(output)
output_final