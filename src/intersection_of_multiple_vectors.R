library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# intersection of multiple vectors
#
# reduce(): takes a "binary" function (i.e., a function with two primary inputs),
#           and applies it repeatedly to a list untile there is only a single element left
# accumulate(): similar, but it keeps all the interim results
# ---------------------------------------------------------------------------
a <- c(1,3,5,6,10)
b <- c(1,2,3,7,8,10)
c <- c(1,2,3,4,8,9,10)

vs <- list(a, b, c)

vs

vs %>% reduce(intersect)


# ----------
# for checking
vs %>% accumulate(intersect)