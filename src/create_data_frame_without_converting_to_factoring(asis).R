
# ---------------------------------------------------------------------------
# Create data frame without converting to factor (asis) by I()
# ---------------------------------------------------------------------------
x1 <- data.frame(a = 1:1000, b = letters[1:1000])

x2 <- data.frame(a = 1:1000, b = I(letters[1:1000]))



# ----------
# AsIs attribute applied to x2
str(x1)
str(x2)



# ----------
# the memory size of x1 (factored) is less than that of x2 (As-Is)
c(object.size(x1), object.size(x2))


