library(dplyr)
library(tidyverse)
library(nycflights13)



# ---------------------------------------------------------------------------
# Coerce a data frame to a tibble
# ---------------------------------------------------------------------------
as_tibble(iris)



# ---------------------------------------------------------------------------
# Create a new tibble
#
# note that tibble() does much less than data.frame()
#  - it never changes the type of the inputs (e.g., it never converts strings to factors !)
#  - it never changes the names of variables, and it never creates row names
# ---------------------------------------------------------------------------
tb <- tibble(
  x  = 1:5,
  y = 1,
  z = x^2 + y
)

tb


# ----------
# It's possible for a tibble to have column names that are not valid R variable names aka nonsyntactic names.
# For example, they might not start with a letter, or they might contain unusual characters like a space.

tb <- tibble(
  ':)' = "smile",
  ' ' = "space",
  '2000' = "number"
)

tb



# ---------------------------------------------------------------------------
# Create a new tibble by tribble(), short for transposed tibble
#  - tribble() is customized for data entry in code: column headings are defined by formulas (i.e., they start with ~), 
#    and entries are separated by commas.
#  - This makes it possible to lay out small amounts of data in easy-to-read form.
# ---------------------------------------------------------------------------
tb <- tribble(
  ~x, ~y, ~z,
  #--/--/----
  "a",2,3.6,
  "b",1,8.5
)


tb


# ---------------------------------------------------------------------------
# Create a new tibble by tbl_df(read.table())
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



# ---------------------------------------------------------------------------
# print method
# ---------------------------------------------------------------------------
# width = Inf will display all columns
flights %>% print(n = 10, width = Inf)


# options(tibble.width = Inf) always print all columns, regardless of the width of the screen



# ---------------------------------------------------------------------------
# subsetting
# ---------------------------------------------------------------------------
flights$flight

flights[["flight"]]

flights[,c("flight", "year")]

flights[1]

flights[[1]]

flights %>% .$flight

flights %>% dplyr::select(flight) %>% .[[1]]



# ---------------------------------------------------------------------------
# enframe() converts named atomic vectors or lists to 2-column data frame
# ---------------------------------------------------------------------------
enframe(1:3)

enframe(c(a = 5, b = 7))



# ---------------------------------------------------------------------------
# deframe() converts 2-column data frames to a named vector or list, using the 1st column as name adn the 2nd column as value
# ---------------------------------------------------------------------------
( df <- enframe(c(a = 5, b = 7)) )

( vc <- deframe(df) )



# ---------------------------------------------------------------------------
# Compare with data frame operation
# ---------------------------------------------------------------------------
df <- data.frame(abc = 1, xyz = "a")
str(df)


# data frame does partial matching !! if you access by $
df$x


# data frame conver character column to factor automatically 
df[, "xyz"]


# data frame does not show type
df[, c("abc", "xyz")]


