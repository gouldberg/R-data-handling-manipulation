library(nycflights13)
library(lubridate)
library(tidyverse)



# ---------------------------------------------------------------------------
# Durations
#  - represent an exact number of seconds
# ---------------------------------------------------------------------------
# How old are you ?
# a difftime class object records a time span of seconds, minutes, hours, days, or weeks.
( tmp_age <- today() - ymd(19690522) )

as.duration(tmp_age)



# ----------
# You can add and subtract durations to and from days
ymd(20190211) + ddays(300)

today() - ( dyears(1) + dweeks(12) + dhours(15) + dminutes(10) + dseconds(10) )



# ---------------------------------------------------------------------------
# Peirods
#  - work with human times, like days and months
# ---------------------------------------------------------------------------
# Compared to durations, periods are more likely to do what you expect
# Peridos can take into account leap year !!
ymd("2016-01-01") + dyears(1)

ymd("2016-01-01") + years(1)



# ----------
ymd("2016-01-01") + ( years(1) + months(2) + weeks(3) + days(2) + hours(10) + minutes(10) + seconds(15) )




# ---------------------------------------------------------------------------
# Fix by adding days(1) to the arrival time of each overnight flight
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
# Some planes appear to have arrived at their destination before they departed from New York City.
# These are overnight flights. These flights arrived on the followin day.
flights_dt %>% filter(arr_time < dep_time)


# We can fix by adding days(1) to the arrival time of each overnight flight
flights_dt <- flights_dt %>% mutate(overnight = arr_time < dep_time, 
                                    arr_time = arr_time + days(overnight * 1), 
                                    sched_arr_time = sched_arr_time + days(overnight * 1))


flights_dt %>% filter(overnight, arr_time < dep_time)






