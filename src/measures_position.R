library(nycflights13)
library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# measures of position
# ---------------------------------------------------------------------------
not_cancelled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))



# ----------
# first() and last()
# You can find the first and last departure for each day
not_cancelled %>% group_by(year, month, day) %>% summarize(first_dep = first(dep_time), last_dep = last(dep_time))



# ----------
# nth(x, n)
# You can find the 10th departure for each day
# This let you see a default value if that position does not exist (such as only has 5 elements)
not_cancelled %>% group_by(year, month, day) %>% summarize(X10th = nth(dep_time, 10))



# ----------
# These functions are complementary to filtering on ranks
not_cancelled %>% group_by(year, month, day) %>% mutate(r = min_rank(desc(dep_time))) %>% filter(r %in% range(r))



