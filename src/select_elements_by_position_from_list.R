library(dplyr)
library(tidyverse)



# ---------------------------------------------------------------------------
# select elements by potition from list
#
# map_dbl() takes a vector as input, applies a function to each piece, and then returns a new "double" vector
# ---------------------------------------------------------------------------
x <- list(list(1,2,3,4,5), list(4,5,6), list(14,2,3,5,10))
y <- list(c(1,2,3,4,5), c(4,5,6), c(14,2,3,5,10))

x
y

# select only second elements from each list
# use integer to specify the position
x %>% map_dbl(2)

y %>% map_dbl(2)
