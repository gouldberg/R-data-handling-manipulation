library(lubridate)
library(tidyverse)



# ---------------------------------------------------------------------------
# Set and modify date-time components
# ---------------------------------------------------------------------------
datetime <- ymd_hms("2016-07-08 12:34:56")



# ----------
# modify date-time components
year(datetime) <- 2020
datetime



# ----------
# rather than modifying in place, you can create a new date-time with update().
update(datetime, year = 2000, month = 2, mday = 2, hour = 2)


# if values are too big, they will roll over
ymd("2015-02-01") %>% update(mday = 30)

ymd("2015-02-01") %>% update(hour = 400)



# ----------
x <- ymd("2016-07-08", "2017-08-09", "2018-09-10")

update(x, year = c(2015, 2014, 2013), month = c(10, 11, 12))



# ---------------------------------------------------------------------------
# Setting larger components of a date to a constant is a powerful technique
# that allows you to explore pattens in the smaller components
# ---------------------------------------------------------------------------
flights

make_datetime_100 <- function(year, month, day, time){
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(dep_time = make_datetime_100(year, month, day, dep_time),
         arr_time = make_datetime_100(year, month, day, arr_time),
         sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
         sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))


flights_dt



# ----------
# set all of yday of dep_time to 1 and visualize hourly pattern !!
flights_dt %>% mutate(dep_hour = update(dep_time, yday=1)) %>% ggplot(aes(dep_hour)) + geom_freqpoly(binwidth = 300)

