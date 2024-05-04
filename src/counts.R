library(nycflights13)
library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# count
# ---------------------------------------------------------------------------
x <- c(2,2,1,3,4,1,1,1,4,3,NA,NA)

table(x)



# ----------
str(x)
addNA(x)


# add table to NA
table(addNA(x))

table(x, useNA="always")




# ---------------------------------------------------------------------------
# count by tidyverse
# ---------------------------------------------------------------------------
not_cancelled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))



# ----------
# is.na()
# to count the number of non-missing values
sum(!is.na(not_cancelled$arr_time))



# ----------
# n()
flights %>% group_by(dest) %>% filter(n() > 365)

flights %>% group_by(carrier, dest) %>% summarize(count = n()) %>% arrange(desc(count))



# ----------
# n_distinct()
# to count the number of distinct (unique) values
not_cancelled %>% group_by(dest) %>% summarize(carriers = n_distinct(carrier)) %>% arrange(desc(carriers))



# ----------
# count()
# counting rows by dest
not_cancelled %>% count(dest)



# ----------
# count(, wt=)
# You could use weight variable to count (sum) thte total numver of miles a plan flew
not_cancelled %>% count(tailnum, wt = distance)



# ---------------------------------------------------------------------------
# row_number()
# ---------------------------------------------------------------------------
y <- c(1, 2, 2, NA, 3, 4)

row_number(y)



