library(dplyr)


# ---------------------------------------------------------------------------
# split data fram to list by category
# ---------------------------------------------------------------------------
# split a data frame into lists
( list_dat <- split(mtcars, mtcars$cyl) )



# binding lists to a data frame
bind_rows(list_dat)

