library(nycflights13)
library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# select
# ---------------------------------------------------------------------------
# select all columns that matches names that begin with "arr"
dplyr::select(flights, starts_with("arr"))


# ----------
# select all columns that matches names that end with "arr"
dplyr::select(flights, ends_with("time"))



# ----------
# select all columns between year and day (inclusive)
dplyr::select(flights, year:day)


# ----------
# select all columns except those from year to day (inclusive)
dplyr::select(flights, -(year:day))



# ----------
# select all columns containing "time"
dplyr::select(flights, contains("time"))



# ----------
# num_range("x", 1:3) matches x1, x2, x3


# -----------
# combination
dplyr::select(flights, year:day, ends_with("delay"), everything())



# ---------------------------------------------------------------------------
# select:  extract()
#  - do not need to specify "dplyr::select" but you have to specify by column number
# ---------------------------------------------------------------------------
extract(flights, 1:4)

extract(flights, "year")


# --> extract's aliase is '['



# ---------------------------------------------------------------------------
# select + everything()
#
#  - If you have a handful of variables, you'd like to move to the start of the data frame.
# ---------------------------------------------------------------------------
dplyr::select(flights, time_hour, air_time, everything())



# ---------------------------------------------------------------------------
# select + one_of()
#
#  - Matches variable names in a character vector
# ---------------------------------------------------------------------------
# you can use select by given vector
vars <- c("year", "month", "day", "dep_delay", "arr_delay")

dplyr::select(flights, one_of(vars))



# ---------------------------------------------------------------------------
# transmute
#  - If you only want to keep the new variables
# ---------------------------------------------------------------------------
transmute(flights, gain = arr_delay - dep_delay, hours = air_time / 60, gain_per_hour = gain / hours)



# ---------------------------------------------------------------------------
# de-select
# ---------------------------------------------------------------------------
# select all columns except those from year to day (inclusive)
dplyr::select(flights, -(year:day))



# ----------
# specify the column number
flights
flights[,-1]



# ----------
# specify the name of column
flights[,names(flights)!="year"]

