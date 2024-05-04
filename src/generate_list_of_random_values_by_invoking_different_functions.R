library(dplyr)
library(tidyverse)


# ---------------------------------------------------------------------------
# generate list of random values by invoking different functions
#
# incoke_map()
# ---------------------------------------------------------------------------
# Generate the list of randomly sampled values each from runif, rnorm and rpois
f <- c("runif", "rnorm", "rpois")

param <- list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)

invoke_map(f, param, n = 5) %>% str()



# ----------
# You can use tribble() to make creating these matching pairs a little easier
sim <- tribble(
  ~f, ~params,
  "runif", list(min=-1, max=1),
  "rnorm", list(sd=5),
  "rpois", list(lambda=10)
)


output <- sim %>% mutate(sim = invoke_map(f, params, n = 10))

output

output$sim


