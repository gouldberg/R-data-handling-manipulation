library(dplyr)
library(tidyr)
library(tidyverse)



# ---------------------------------------------------------------------------
# Splitting a single column to multiple columns:  separate()
# ---------------------------------------------------------------------------
messy_df <- tbl_df(read.table(header=TRUE, text = "
Grp_Ind Yr_Mo     City_State      Extra_variable
1.a     2006_Jan  Dayton(OH)      XX01person_1
1.b     2006_Feb  GrandForks(ND)  XX02person_2
1.c     2006_Mar  Fargo(ND)       XX03person_3
2.a     2007_Jan  Rochester(MN)   XX04person_4
"))


messy_df



# sparate Grp_Ind column into two variables named "Grp" and "Ind"
messy_df %>% tidyr::separate(col = Grp_Ind, into = c("Grp", "Ind"))



# ----------
# default separater is any non alpha-numeric character but you can specify the specific character to separate at
messy_df %>% separate(col = Extra_variable, into = c("X", "Y"), sep = "_")



# ----------
# You can keep the original column that you are separating
messy_df %>% separate(col = Grp_Ind, into = c("Grp", "Ind"), remove = FALSE)



# ----------
# by default, separate() will split values wherever it sees a non-alphanumeric character
table3
table3 %>% separate(rate, into = c("cases", "population"))



# ----------
# but we need not character type but integer type.
# we can ask separate() to try and convert to better types using convert = TRUE
table3 %>% separate(rate, into = c("cases", "population"), convert=TRUE)



# ----------
# You can also pass a vector of integers to sep. separte() will interpret the integers as positions to split at
# Positive values start at 1 on the far left of the strings, negative values start at -1 on the far right of the strings
table3 %>% separate(year, into = c("century", "year"), sep=2)



# ---------------------------------------------------------------------------
# Combining multiple columns into a single column:  unite()
# ---------------------------------------------------------------------------
expenses <- tbl_df(read.table(header=TRUE, text = "
Year  Month Day Expense
2015  01  01  500
2015  02  05  90
2015  02  22  250
2015  03  10  325
"))


expenses



# ----------
# default separater when uniting is "_"
expenses %>% unite(col = "Date", c(Year, Month, Day))



# ----------
# specify sep argment to change separator
expenses %>% unite(col = "Date", c(Year, Month, Day), sep = "-")



# ----------
# The default will place an underscore(_)
table5

table5 %>% unite(new, century, year)
table5 %>% unite(new, century, year, sep="")



# ---------------------------------------------------------------------------
# Combining multiple columns into a single column:  interaction
# ---------------------------------------------------------------------------
set.seed(1234)
dim <- c(3, 2, 2, 2)

tab <- array(rpois(prod(dim), 15), dim = dim)

dimnames(tab) <- list(Pet = c("dog", "cat", "bird"),
                      Age = c("young", "old"),
                      Color = c("black", "white"),
                      Sex = c("male", "female"))

tab


# data in frequency data frame to matrix
tab.df <- as.data.frame(as.table(tab))



# ----------
# interaction() combines the values of multiple columns
tab.df <- within(tab.df,
                 { Pet.Age = interaction(Pet, Age)
                 Color.Sex = interaction(Color, Sex) })

tab.df
