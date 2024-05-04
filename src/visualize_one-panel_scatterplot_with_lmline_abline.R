library(dplyr)
library(ggplot2)
libarary(lattice)



# ---------------------------------------------------------------------------
# Visualize scatter plot with abline
# ---------------------------------------------------------------------------
mpg


# ----------
# scatter plot with abline (reference line)
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point() + geom_abline()

ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point() + geom_abline() + coord_fixed()


# ----------
# you can specify intercept and slope
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point() + geom_abline(intercept = 6, slope = 1) + coord_fixed()



# ---------------------------------------------------------------------------
# Visualize scatter plot with lm line
# ---------------------------------------------------------------------------
p <- ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point(mapping = aes(colour = class))

p + geom_smooth(method = "lm", se = TRUE)



# ----------
# xyplot()
# type = "r"
xyplot(hwy ~ displ, data = mpg, type = c("g", "p", "r"), col.line = "black", pch = 20)
