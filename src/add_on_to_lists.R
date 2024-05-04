
# ---------------------------------------------------------------------------
# Adding on to lists
#  - list()
#  - append()
#  - c()
# ---------------------------------------------------------------------------
l1 <- list(1:3, "a", c(TRUE, FALSE, TRUE))

str(l1)



# ----------
# If we add the new elements with list(), it will create a list of two components,
#  - component 1 will be a nested list of the original list
#  - component 2 will be the new elements added
l2 <- list(l1, c(2.5,4.2))

str(l2)



# ----------
# simply add a fourth list component without creating nested lists by append()
l3 <- append(l1, list(c(2.5, 4.2)))

str(l3)


# simply concatenate two lists by c()
l3_2 <- c(l1, list(c(2.5, 4.2)))

str(l3_2)



# alternatively, we can also add a new list component by utilizing the $ sign and naming the new item
l1$item4 <- c(2.5, 4.2)

str(l1)



# ---------------------------------------------------------------------------
# Adding on to lists a individual element by [[]] <- c()
# ---------------------------------------------------------------------------
l1[[1]] <- c(l1[[1]], 4:6)
l1[[2]] <- c(l1[[2]], c("dding", "to a", "list"))

str(l1)


