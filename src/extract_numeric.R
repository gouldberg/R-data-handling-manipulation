library(dplyr)
library(tidyr)


# ---------------------------------------------------------------------------
# Extract numeric
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
# Sometimes accounting values in Excel spreadsheet get read in as a character value, which is the case for the Cost variable.
# We may wish to extract only the numeric part of this regular expression, which can be done with extract_numeric()
# Note that extract_numeric works on a single variable so when you pipe the expense data frame into the function
# you need to use %$% operator

library(magrittr)
expenses %$% extract_numeric(Cost)


# You can use this to convert and save the Cost column to a numeric variable
expenses$Cost <- expenses %$% extract_numeric(Cost)
expenses