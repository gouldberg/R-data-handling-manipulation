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

as.difftime(tmp_age)

as.numeric(as.difftime(tmp_age), units="mins")

as.numeric(as.difftime(tmp_age), units="secs")



# ---------------------------------------------------------------------------
# Intervals
#  - duration with a starting and ending point
# ---------------------------------------------------------------------------
# how many weeks ?
tmp_age %/% dweeks(1)



# ----------
# If you want a more accurate measurement, you'll have to use an interval.
# Interval takes into account leap years
( ymd(19690522) %--% today() ) %/% weeks(1)


( ymd(19690522) %--% today() ) / days(1)


# different value
18160 / 365

( ymd(19690522) %--% today() ) / years(1)



# ----------
# Calculate interval in years considering leap year
( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2017-02-01")) ) / years(1)

( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2017-01-31") + 1) ) / years(1)

( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2017-02-10")) ) / years(1)


( ymd(as.Date("2013-02-01")) %--% ymd(as.Date("2013-02-10")) ) / years(1)
9 / 365

( ymd(as.Date("2016-01-01")) %--% ymd(as.Date("2016-01-10")) ) / years(1)
9 / 366

( ymd(as.Date("2015-12-30")) %--% ymd(as.Date("2016-01-10")) ) / years(1)
11 / 366

( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2016-12-31")) ) / years(1)
(28+31+30+31+30+31+31+30+31+30+31)/366

( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2017-1-10")) ) / years(1)
(28+31+30+31+30+31+31+30+31+30+31)/366 + (10)/366

( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2018-1-10")) ) / years(1)
(28+31+30+31+30+31+31+30+31+30+31)/366 + (32)/366 + (365-32)/365 + 10/365



# ----------
# Calculate interval in months considering leap year
( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2016-3-01")) ) / months(1)

( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2017-3-01")) ) / months(1)

( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2016-3-10")) ) / months(1)
1 + 9/31

( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2016-2-10")) ) / months(1)
9/29

( ymd(as.Date("2015-02-01")) %--% ymd(as.Date("2015-2-10")) ) / months(1)
9/28

( ymd(as.Date("2016-02-01")) %--% ymd(as.Date("2018-1-10")) ) / months(1)
12 + 12 + 9/31

