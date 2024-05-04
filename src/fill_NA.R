library(dplyr)
library(tidyr)


# ---------------------------------------------------------------------------
# fill NA
# ---------------------------------------------------------------------------
expenses <- tbl_df(read.table(header=TRUE, text = "
Dept  Year  Month Day Cost
A     2015  01    01  $500.00
NA    NA    02    05  $90.00
NA    NA    02    22  $1,250.45
NA    NA    03    NA  $325.10
B     NA    01    02  $260.00
NA    NA    02    05  $90.00
"))


expenses



# ----------
# Often Excel reports will not repeat certain variables.
# When we read these reports in, the empty cells are typically filled in with NA.
# We can fll these values in with the previous entry using fill()
expenses %>% fill(Dept, Year)



# ----------
# You can replace missing (or NA) values with a specified value
library(magrittr)

expenses %>% replace_na(replace = list(Year = 2015, Day = "unknown"))
