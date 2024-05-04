library(dplyr)
library(tidyverse)


# ---------------------------------------------------------------------------
# enframe() converts named atomic vectors or lists to 2-column data frame
# ---------------------------------------------------------------------------
enframe(1:3)

enframe(c(a = 5, b = 7))



# ---------------------------------------------------------------------------
# deframe() converts 2-column data frames to a named vector or list, using the 1st column as name and the 2nd column as value
# ---------------------------------------------------------------------------
( df <- enframe(c(a = 5, b = 7)) )

( vc <- deframe(df) )

