library(lubridate)



# ---------------------------------------------------------------------------
# Convert strings to date by as.Date()
# ---------------------------------------------------------------------------

# default date format is YYYY-MM-DD
x <- c("2015-07-01", "2015-08-01", "2015-09-01")

y <- c("07/01/2015", "08/01/2015", "09/01/2015")

z <- c("2015/7/1", "2015/8/1", "2015/9/1")


as.Date(x)

as.Date(y, format = "%m/%d/%Y")

as.Date(z)



# ---------------------------------------------------------------------------
# Convert strings to date by lubridate
#  - One of the many benefits of the lubridate packages is that
#    it automatically recognizes the common separators used when recording dates ("-", "/", ".")
# ---------------------------------------------------------------------------

# ymd()
ymd("2015.7.1")

ymd("2015-7-1")

dmy("7-1-2015")



# ----------
# hms(), ymd_hms()
hms("00.45.45")

ymd_hms("2015.7.1.12.45.45")



# ---------------------------------------------------------------------------
# Create dates by merging data by ISODate()
# ---------------------------------------------------------------------------

yr <- c("2012", "2013", "2014", "2015")
mo <- c("1", "5", "7", "2")
day <- c("02", "22", "15", "28")
hour <- c("02", "12", "15", "12")
minute <- c("02", "12", "15", "12")
second <- c("02", "12", "15", "12")


ISOdate(year = yr, month = mo, day = day, hour = hour, min = minute, sec= second, tz = "GMT")



# ---------
# truncate the unused time data by converting with as.Date()
ISOdate(year = yr, month = mo, day = day)

as.Date(ISOdate(year = yr, month = mo, day = day))



