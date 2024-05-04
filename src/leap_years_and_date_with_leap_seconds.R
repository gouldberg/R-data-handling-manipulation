library(lubridate)
library(tidyverse)



# ---------------------------------------------------------------------------
# leap year or date with leap seconds ?
# ---------------------------------------------------------------------------
# return the date with leap seconds
as.Date(.leap.seconds)


print(.leap.seconds)



# ----------
# lubridate::leap_year()
# If x is a recognized date-time objet, leap_year will return whether x occurs during a leap year.
leap_year(as.Date("2009-08-02"))



# ----------
# lubridate::leap_year()
# If x is a number, leap_year returns whether it would be a leap year under the Gregorian calendar.
leap_year(2016)
