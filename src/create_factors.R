library(dplyr)
library(forcats)



# ---------------------------------------------------------------------------
# Create factors
# ---------------------------------------------------------------------------
# Give levels (n), repeat times for each level (k)
( x1 <- gl(n = 2, k = 8, labels=c("C", "T"), ordered=TRUE) )
( x2 <- gl(n = 2, k = 8, length = 100, labels=c("C", "T"), ordered=TRUE) )

nlevels(x1)



# ---------------------------------------------------------------------------
# Create factors with interaction
# ---------------------------------------------------------------------------
( a <- gl(n = 2, k = 4) )
( b <- gl(n = 2, k = 2, labels = c("c", "t")) )


interaction(a, b, sep="_")



# ---------------------------------------------------------------------------
# Create factors with same prefix name
# ---------------------------------------------------------------------------
factor(letters[1:20], label="LT")




# ---------------------------------------------------------------------------
# Create factors
# ---------------------------------------------------------------------------
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")


# ----------
# If you omit levels, factor is created taken from the data in alphabetical order
factor(x1)



# ----------
# Sometimes you'd prefer that the order of the levels match the order of the first appearance in the data
# You can do that when creating the factor by setting levels to unique(), or after the fact, fct_inorder()
f1 <- factor(x1, levels = unique(x1))

f1


f2 <- x1 %>% factor() %>% fct_inorder()

f2


levels(f2)



# ---------------------------------------------------------------------------
# create factors with specified levels
# ---------------------------------------------------------------------------
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

y1 <- factor(x1, levels = month_levels)

y1


# factored value is sorted by the level
sort(y1)



# ----------
# Any values not in the set will be silently converted to NA
y2 <- factor(x2, levels = month_levels)

y2



# ----------
# If you want an error, you can use readr::parse_factor()
y2 <- parse_factor(x2, levels = month_levels)







