library(nycflights13)
library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# ranking
# ---------------------------------------------------------------------------
y <- c(1, 2, 2, NA, 3, 4)


min_rank(y)


min_rank(desc(y))


row_number(y)


dense_rank(y)


percent_rank(y)


cume_dist(y)


ntile(y, 3)


