
# ---------------------------------------------------------------------------
# Get positions in aother vector by match()
# ---------------------------------------------------------------------------
x <- c(1, 2, 3, 2, 1)


# For x, get positions in c(2,3)
match(x, c(2,3))



# For x, get positions in c(4,3,2,1)
match(x, 4:1)



# ---------------------------------------------------------------------------
# By using match(), you can convert a value of a vector by looking up other vector
# ---------------------------------------------------------------------------
x <- c(1, 2, 3, 2, 1, 4, 5, 1, 2)

z <- letters[1:5]


# convert the value of x by looking up z
z[match(x,1:5)]



