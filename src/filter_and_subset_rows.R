library(nycflights13)
library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# filtering and subsetting
# ---------------------------------------------------------------------------
# remove duplicate rows
flights %>% distinct()



# ----------
# random sample, 0.01% sample size without replacement
flights %>% sample_frac(size = 0.0001, replace = FALSE)



# ----------
# random sample of 10 rows with replacement
flights %>% sample_n(size = 10, replace = TRUE)



# ----------
# select rows 1001-1010
flights %>% slice(1001:1010)



# ----------
# select top n entries
# In this case ranks variable dep_time and selects the rows with the top 10 values
flights %>% top_n(n = 10, wt = dep_time)