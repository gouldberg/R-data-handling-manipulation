library(nycflights13)
library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# rename
# ---------------------------------------------------------------------------
rename(flights, tail_num = tailnum)

colnames(flights)



# ----------
# select and rename
flights %>% dplyr::select(Yr = year)



# ----------
library(magrittr)

mtcars %>% set_colnames(paste("Col", 1:11, sep="")) %>% head



