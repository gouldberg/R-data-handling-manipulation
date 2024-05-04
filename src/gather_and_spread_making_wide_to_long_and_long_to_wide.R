library(dplyr)
library(tidyr)
library(tidyverse)



# ---------------------------------------------------------------------------
# Making wide data to long data
# ---------------------------------------------------------------------------
wide <- tbl_df(read.table(header=TRUE, text = "
Group Year  Qtr.1 Qtr.2 Qtr.3 Qtr.4
1 2006  15  16  19  17
1 2007  12  13  27  23
1 2008  22  22  24  20
1 2009  10  14  20  16
2 2006  12  13  19  17
2 2007  16  14  27  23
2 2008  13  22  24  20
2 2009  23  11  20  16
3 2006  11  20  22  16
3 2007  13  11  27  21
3 2008  17  12  23  19
3 2009  14  9  31  24
"))


wide



# Making wide data to long data
long <- wide %>% gather(Quarter, Revenue, Qtr.1:Qtr.4)
long



# ----------
# These all produce the same resutls
( long <- wide %>% gather(Quarter, Revenue, -Group, -Year) )
( long <- wide %>% gather(Quarter, Revenue, 3:6) )
( long <- wide %>% gather(Quarter, Revenue, Qtr.1, Qtr.2, Qtr.3, Qtr.4) )



# ---------------------------------------------------------------------------
# Making long data to wide
# ---------------------------------------------------------------------------
( back2wide <- long %>% spread(Quarter, Revenue) )



# ---------------------------------------------------------------------------
# Note:  spread() + gather() not perfectly symmetrical
# ---------------------------------------------------------------------------
stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)


stocks


# Note that spread() + gather() not perfectly symmetrical
stocks %>% spread(key = year, value = return)

stocks %>% spread(key = year, value = return) %>% gather(key = year, value = return, '2015':'2016')



# ---------------------------------------------------------------------------
# handling missing values
# ---------------------------------------------------------------------------
stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c(1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)


stocks



# ----------
# complete() makes missing values explicit in tidy data
# complete() takes a set of columns, and finds all unique combinations.
# It then ensures the original dataset contains all those values, filling in explicit NAs where necessary
stocks
stocks %>% complete(year, qtr)


# ----------
# implicit missing value explicit by putting years in the columns
stocks %>% spread(key = year, value = return)



# ----------
# na.rm = TRUE in gather() to turn explicit missing values implicit
stocks %>% spread(key = year, value = return) %>% gather(key = year, value = return, '2015':'2016', na.rm=TRUE)






