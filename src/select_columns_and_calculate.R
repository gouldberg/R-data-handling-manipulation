library(dplyr)
library(magrittr)



# ---------------------------------------------------------------------------
# select column and calculate
# ---------------------------------------------------------------------------
# multiply by
mtcars %>% extract(., "mpg") %>% multiply_by(5)
mtcars %>% extract(., "mpg") %>% '*'(5)

mtcars %>% extract(., c("mpg", "cyl")) %>% multiply_by(5)



# divide by
mtcars %>% extract(., "mpg") %>% divide_by(6)
mtcars %>% extract(., "mpg") %>% '/'(6)



# add
mtcars %>% extract(., "mpg") %>% add(6)
mtcars %>% extract(., "mpg") %>% '+'(6)



# raise to power
mtcars %>% extract(., "mpg") %>% raise_to_power(2)
mtcars %>% extract(., "mpg") %>% '^'(2)



# ----------
( good.times <-
  Sys.Date() %>%
  as.POSIXct %>%
  seq(by = "15 mins", length.out = 100) %>%
  data.frame(timestamp = .) )



( good.times$quarter <-
  good.times %>%
  use_series(timestamp) %>%
  format("%M") %>%
  as.numeric %>%
  divide_by_int(15) %>%
  add(1) )