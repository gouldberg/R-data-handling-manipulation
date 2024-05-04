library(dplyr)
library(tidyverse)


# ---------------------------------------------------------------------------
# Apply function to each column (vector)
#
# map functions from purrr package:
#  - takes a vector as input, applies a function to each piece, and then returns a new vector
#    that's the same length (and has the same names) as the input
# 
# map(): makes a list
# map_dbl(): makes a double vector
# map_int(): makes an integer vector
# map_lgl(): makes a logical vector
# map_chr(): makes a character vector
#
# lapply() is basically identical to map(), except that map() is consistent with all the other functions in purrr, and you can use the shortcuts for .f.
# ---------------------------------------------------------------------------
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)


df


# ----------
map_dbl(df, mean)

map_dbl(df, median)

map_dbl(df, sd)



# ----------
# map_*() uses ... to pass along additional arguments to .f each time it's called
map_dbl(df, mean, trim = 0.5)



# ----------
# The map functions also preserve names
z <- list(x = 1:3, y = 4:5)

map_int(z, length)

map_int(df, length)
