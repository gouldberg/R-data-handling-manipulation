library(lubridate)
library(tidyverse)



# ---------------------------------------------------------------------------
# Extract date-time components
# ---------------------------------------------------------------------------
( today_now <- Sys.time() )


year(today_now)

quarter(today_now)
quarters(today_now)

month(today_now)
months(today_now)

weekdays(today_now)

week(today_now)

day(today_now)
days(today_now)

date(today_now)

hour(today_now)

minute(today_now)

second(today_now)
seconds(today_now)



# ---------------------------------------------------------------------------
# Extract date-time components: format
# ---------------------------------------------------------------------------
format(today_now, "%a %b %d %X %Y %Z")


format(today_now, "%m/%d/%y %H:%M:%S")


# %Y-%m-%d
format(today_now, "%F")


# %m/%d/%y  (ISO C99)
format(today_now, "%D")


format(today_now, "%e")


format(today_now, "%a")
format(today_now, "%A")
format(today_now, "%b")
format(today_now, "%B")
format(today_now, "%c")


# ---------------------------------------------------------------------------
# Extract date-time components: lubridate
# ---------------------------------------------------------------------------
datetime <- ymd_hms("2016-07-08 12:34:56")


year(datetime)

month(datetime)
month(datetime, label=TRUE)


# day of the month
mday(datetime)


# day of the year
yday(datetime)


# day of the week
wday(datetime)
wday(datetime, label=TRUE, abbr=FALSE)


hour(datetime);  minute(datetime);  second(datetime)



