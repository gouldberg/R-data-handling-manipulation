library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# Convert complex list to single data frame
#
# reduce(): takes a "binary" function (i.e., a function with two primary inputs),
#           and applies it repeatedly to a list untile there is only a single element left
# accumulate(): similar, but it keeps all the interim results
# ---------------------------------------------------------------------------
dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)


dfs


dfs %>% reduce(full_join)


# ----------
# accumulate() is similar but it keeps all the interim results
dfs %>% accumulate(full_join)