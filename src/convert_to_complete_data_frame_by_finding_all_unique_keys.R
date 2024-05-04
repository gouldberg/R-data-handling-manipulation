library(dplyr)
library(tidyr)


# ---------------------------------------------------------------------------
# Convert data frame with missing key values and NAs to complete data frame
# by finding all unique keys and combinations automatically
#
# complete() makes missing values explicit in tidy data
# complete() takes a set of columns, and finds all unique combinations.
# It then ensures the original dataset contains all those values, filling in explicit NAs where necessary
# ---------------------------------------------------------------------------
stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c(1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)


stocks



# ----------
# complete() makes missing values explicit in tidy data
# complete() takes a set of columns, and finds all unique combinations.
# It then ensures the original dataset contains all those values, filling in explicit NAs where necessary
stocks
stocks %>% complete(year, qtr)

