library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# Select a column and convert to unnamed vector
# ---------------------------------------------------------------------------
df <- data.frame(
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100)
)


str(df)


df %>% dplyr::select(a) %>% .[[1]]
