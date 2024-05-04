library(nycflights13)
library(lubridate)
library(tidyverse)



# ---------------------------------------------------------------------------
# round the date to a nearby unit of time
#  - floor_date() to round down,  ceiling_date() to round up,  and round_date()
#  - valid base units are year, halfyear, season, quarter, bimonth, month, week, day, hour, minute, second
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
# application of rounding date time
# plot the number of flights per week
flights_dt %>% count(week = floor_date(dep_time, "week")) %>% ggplot(aes(week, n)) + geom_line()



# ----------
# floor_date()

idx <- 100

flights_dt$dep_time[idx]

floor_date(flights_dt$dep_time, "year")[idx]

floor_date(flights_dt$dep_time, "halfyear")[idx]

floor_date(flights_dt$dep_time, "season")[idx]

floor_date(flights_dt$dep_time, "quarter")[idx]

floor_date(flights_dt$dep_time, "bimonth")[idx]

floor_date(flights_dt$dep_time, "month")[idx]

floor_date(flights_dt$dep_time, "week")[idx]

floor_date(flights_dt$dep_time, "day")[idx]

floor_date(flights_dt$dep_time, "hour")[idx]

floor_date(flights_dt$dep_time, "minute")[idx]

floor_date(flights_dt$dep_time, "second")[idx]



# ----------
# ceiling_date()

idx <- 100

flights_dt$dep_time[idx]

ceiling_date(flights_dt$dep_time, "year")[idx]

ceiling_date(flights_dt$dep_time, "halfyear")[idx]

ceiling_date(flights_dt$dep_time, "season")[idx]

ceiling_date(flights_dt$dep_time, "quarter")[idx]

ceiling_date(flights_dt$dep_time, "bimonth")[idx]

ceiling_date(flights_dt$dep_time, "month")[idx]

ceiling_date(flights_dt$dep_time, "week")[idx]

ceiling_date(flights_dt$dep_time, "day")[idx]

ceiling_date(flights_dt$dep_time, "hour")[idx]

ceiling_date(flights_dt$dep_time, "minute")[idx]

ceiling_date(flights_dt$dep_time, "second")[idx]



# ----------
# round_date()

idx <- 100

flights_dt$dep_time[idx]

round_date(flights_dt$dep_time, "year")[idx]

round_date(flights_dt$dep_time, "halfyear")[idx]

round_date(flights_dt$dep_time, "season")[idx]

round_date(flights_dt$dep_time, "quarter")[idx]

round_date(flights_dt$dep_time, "bimonth")[idx]

round_date(flights_dt$dep_time, "month")[idx]

round_date(flights_dt$dep_time, "week")[idx]

round_date(flights_dt$dep_time, "day")[idx]

round_date(flights_dt$dep_time, "hour")[idx]

round_date(flights_dt$dep_time, "minute")[idx]

round_date(flights_dt$dep_time, "second")[idx]





