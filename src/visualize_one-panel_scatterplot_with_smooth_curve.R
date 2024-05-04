library(dplyr)
library(ggplot2)
library(lattice)



# ---------------------------------------------------------------------------
# Visualize scatter plot with smooth curve
# ---------------------------------------------------------------------------
mpg


# ----------
# scatter plot with smooth curve
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + geom_smooth()



# ----------
# scatter plot by category and smooth curve
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(colour = class)) + geom_smooth()



# ----------
# xyplot()
# type = "smooth"
xyplot(hwy ~ displ, data = mpg, type = c("g", "p", "smooth"), col.line = "black", pch = 20)



# ----------
# car::scaterplot()
# by default, adding a fitted regression line (OLS line), and adding boxplots for x and y
car::scatterplot(wt ~ mpg, data = mtcars)
