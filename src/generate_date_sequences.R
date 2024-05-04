library(lubridate)
library(tidyverse)



# ---------------------------------------------------------------------------
# Create date sequences, date object
# ---------------------------------------------------------------------------

seq(as.Date("2010-2-22"), as.Date("2012-2-2"), by="quarter")


seq(as.Date("2010-2-22"), as.Date("2012-2-2"), by="month")


seq(as.Date("2010-2-22"), as.Date("2012-2-2"), by="week")


seq(as.Date("2010-2-22"), as.Date("2012-2-2"), by="3 weeks")


seq(as.Date("2010-2-22"), as.Date("2012-2-2"), by="3 days")



# ---------------------------------------------------------------------------
# Create date sequences: date-time class object, lubridate
#  - Very similar with as.Date(), but the date object is POSIXct rather than just a Date object (as produced by as.Date)
# ---------------------------------------------------------------------------

seq(ymd("2010-2-22"), ymd("2012-2-2"), by="quarter")


seq(ymd("2010-2-22"), ymd("2012-2-2"), by="month")


seq(ymd("2010-2-22"), ymd("2012-2-2"), by="week")


seq(ymd("2010-2-22"), ymd("2012-2-2"), by="3 weeks")


seq(ymd("2010-2-22"), ymd("2012-2-2"), by="3 days")


seq(ymd_hm("2010-2-22 1:00"), ymd_hm("2012-2-2 12:00"), by="hours")



# ---------------------------------------------------------------------------
# Create date sequences: date-time class object, ISOdate
# ---------------------------------------------------------------------------

seq(ISOdate(2010,2,22), ISOdate(2012,2,2), by="3 days")


seq(ISOdate(2010,2,22), ISOdate(2012,2,2), by="3 weeks")


seq(ISOdate(2010,2,22), ISOdate(2012,2,2), by="3 hours")


