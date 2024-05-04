library(dplyr)
library(stringr)



# ---------------------------------------------------------------------------
# Concatenate strings differenctly by condition
# ---------------------------------------------------------------------------
# stc_c() drops silently objects of length 0
# This is particulary useful in conjunction with if

name <- "Hadley"
time_of_day <- "morning"


# choose either TRUE or FALSE
# birthday <- FALSE
birthday <- TRUE
str_c("Good ", time_of_day, " ", name, if(birthday) " and HAPPY BIRTHDAY", ".")


