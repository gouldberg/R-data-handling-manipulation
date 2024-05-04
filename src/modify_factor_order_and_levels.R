library(dplyr)



# ---------------------------------------------------------------------------
# Create factor with ordered levels
# ---------------------------------------------------------------------------
gender <- factor(c("male", "female", "female", "male", "female"), levels = c("male", "female"))

gender



# ---------------------------------------------------------------------------
# Create factor with ordinal factors in which a specific order is desired by using the ordered = TRUE
# ---------------------------------------------------------------------------
x <- c(1,2,3,2,1,3,2,1,2,3,2,1,1,1,2,3,3,2,2,1)
z <- c("low", "middle", "high")

( ses <- z[match(x,1:3)] )

( ses <- factor(ses, levels = z, ordered=TRUE) )



# ----------
# You can also reverse the order of levels if desired by rev(levels())
( ses <- factor(ses, levels = rev(levels(ses))) )



# ---------------------------------------------------------------------------
# Revalue levels by plyr::revalue()
# ---------------------------------------------------------------------------
( ses <- factor(ses, levels = rev(levels(ses))) )
plyr::revalue(ses, c("low"="small", "middle"="medium", "high"="large"))



# ---------------------------------------------------------------------------
# Dropping levels
# ---------------------------------------------------------------------------
# dataframe without "middle"
ses2 <- ses[ses != "middle"]
summary(ses2)



# droplevels drop the level with no record automatically
ses3 <- droplevels(ses2)
ses3



# drop=TRUE does same thing
ses4 <- ses2[,drop=TRUE]
ses4



# ---------------------------------------------------------------------------
# replace factors
# ---------------------------------------------------------------------------
( ff <- factor(substring("statistics", 1:10, 1:10), levels=letters) )



# replace
ff[1:3] <- c("f", "a", "n")
ff


# drop unused levels
ff[,drop=TRUE]



# ---------------------------------------------------------------------------
# reorder factors by calculated statistics
# ---------------------------------------------------------------------------
str(InsectSprays)

InsectSprays$spray


# calculate median of "count" for each factor "spray" and reorder factors by the calculated median 
with(InsectSprays, reorder(spray, count, median))



