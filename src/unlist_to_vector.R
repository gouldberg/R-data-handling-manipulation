
# ---------------------------------------------------------------------------
# The list with numeric value is converted to numeric vector by as.numeric() or by unlist()
# ---------------------------------------------------------------------------
x <- 1:10

( x_list <- as.list(x) )


# convert this list to numeric vector by as.numeric(), or by unlist()
as.numeric(x_list)

unlist(x_list)



# ----------
# the list with character values is converted to character vector by as.character(), or by unlist()

y_list <- as.list(c("I", "love", "R"))

as.character(y_list)

unlist(y_list)



# ---------------------------------------------------------------------------
# unlist() convert nested list to vector
# ---------------------------------------------------------------------------
( l.ex <- list(a = list(1:2, LETTERS[1:3], b = "Z", c = NA)) )



# ----------
# convert nested list to vector with name
l.ex %>% unlist(., recursive=TRUE)


# ----------
# convert to vector with name
l.ex %>% unlist()


# ----------
# convert to vector with no name
l.ex %>% unlist(., use.names=FALSE)




