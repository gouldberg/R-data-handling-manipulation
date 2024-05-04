library(dplyr)
library(ggplot2)
library(lattice)



# ---------------------------------------------------------------------------
# Visualize scatter plot by category in one display
# ---------------------------------------------------------------------------
mpg


# ----------
# color by class
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))



# ----------
# shape by class
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))



# ----------
# color by class
# pch --> changes shape
xyplot(hwy ~ displ, data = mpg, groups = class, pch = 20)




# ---------------------------------------------------------------------------
# groups are created by "cut"
# ---------------------------------------------------------------------------

xyplot(lat ~ long, data = quakes, aspect = "iso",
       groups = cut(depth, breaks = quantile(depth, ppoints(4, 1))),
       auto.key = list(columns = 3, title = "Depth"), xlab = "Longitude", ylab = "Latitude")



# ---------------------------------------------------------------------------
# Continuous gradation for continuous variable
# ---------------------------------------------------------------------------

depth.col <- grey.colors(100)[cut(quakes$depth, 100, label = FALSE)]


depth.ord <- rev(order(quakes$depth))


xyplot(lat ~ long, data = quakes[depth.ord,], aspect = "iso", type = c("p", "g"), col = "black", pch = 21,
       fill = depth.col[depth.ord], cex = 2, xlab = "Longitude", ylab = "Latitude")

