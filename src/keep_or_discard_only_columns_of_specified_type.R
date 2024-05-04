library(dplyr)
library(tidyverse)


# ---------------------------------------------------------------------------
# Keep or discard only columnes of specified type
# ---------------------------------------------------------------------------
# Keep only factor column, numeric column
str(iris)

iris %>% keep(is.factor) %>% str()
iris %>% keep(is.numeric) %>% str()



# ----------
# discard factor column
iris %>% discard(is.factor) %>% str()



