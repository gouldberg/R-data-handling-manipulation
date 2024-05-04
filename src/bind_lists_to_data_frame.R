library(dplyr)



# ---------------------------------------------------------------------------
# Bind two vectors to data frame
# ---------------------------------------------------------------------------
vec1 <- c(a = 1, b = 2)
vec2 <- c(a = 3, b = 4)


# ----------
# each vector is treated as a row
bind_rows(vec1, vec2)



# ---------------------------------------------------------------------------
# Bind two lists to data frame
# ---------------------------------------------------------------------------
( one <- list(mtcars[1:4, ]) )

( two <- list(mtcars[11:14, ]) )


# ----------
bind_rows(one, two)



# ---------------------------------------------------------------------------
# split --> bind
# ---------------------------------------------------------------------------
# split a data frame into lists
( list_dat <- split(mtcars, mtcars$cyl) )



# binding lists to a data frame
bind_rows(list_dat)



# ---------------------------------------------------------------------------
# Bind lists containing vectors to data frame
# ---------------------------------------------------------------------------
ll <- list(
  a = c(A = 1, B = 2),
  b = c(A = 3, B = 4)
)


ll


# ----------
# Note that for historical reasons, lists containing vectors are always treated as data frame.
# Thus their vectors are treated as columns rather than rows, and their inner names are ignored
bind_row(ll)



# ----------
# You can circumvent that behaviour with explicit splicing
bind_rows(!!!ll)


