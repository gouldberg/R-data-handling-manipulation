library(dplyr)
library(ggplot2)
library(lattice)


mpg



# ---------------------------------------------------------------------------
# Visualize scatter plot by category in multiple displays
# ---------------------------------------------------------------------------
# facet by class: facet_wrap()
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ class, nrow=2)



# ----------
# facet by class: facet_grid()
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(. ~ class)



# ----------
# panel by class
xyplot(hwy ~ displ | class, data = mpg, pch = 20)



# ---------------------------------------------------------------------------
# Visualize scatter plot by more than two categories
# ---------------------------------------------------------------------------
# facet by category:  category is not only one but two category (drv and cyl)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ cyl)


xyplot(hwy ~ displ | drv + cyl, data = mpg, pch = 20)



# ---------------------------------------------------------------------------
# Categories are automatically created by "cut" for continuous variable
# ---------------------------------------------------------------------------

xyplot(lat ~ long | cut(depth, 3), data = quakes, pch = ".",
       aspect = "iso", cex = 2, type = c("p", "g"),
       xlab = "Longitude", ylab = "Latitude",
       strip = strip.custom(strip.names = TRUE, var.name = "Depth"))



# ---------------------------------------------------------------------------
# Continuous gradation for continuous variable
# ---------------------------------------------------------------------------


# shingles admitting overlaps
quakes$Magnitude <- equal.count(quakes$mag, 4)


summary(quakes$Magnitude)


quakes$color <- depth.col <- grey.colors(100)[cut(quakes$depth, 100, label = FALSE)]


depth.ord <- rev(order(quakes$depth))


quakes.ordered <- quakes[depth.ord,]


xyplot(lat ~ long | Magnitude, data = quakes.ordered, aspect = "iso", col = "black", cex = 2,
       fill.color = quakes.ordered$color,
       panel = function(x, y, fill.color, ..., subscripts){
         fill <- fill.color[subscripts]
         panel.grid(h = -1, v = -1)
         panel.xyplot(x, y, pch = 21, fill = fill, ...)
       },
       xlab = "Longitude", ylab = "Latitude")



# ----------
depth.breaks <- do.breaks(range(quakes.ordered$depth), 50)


# level.colors():  calculate false colors from a numeric variable (including factors, using their numecir codes) given a color scheme and breakpoints
quakes.ordered$color <- level.colors(quakes.ordered$depth, at = depth.breaks, col.regions = grey.colors)


xyplot(lat ~ long | Magnitude, data = quakes.ordered, aspect = "iso", col = "black", cex = 2,
       groups = color,
       panel = function(x, y, groups, ..., subscripts){
         fill <- groups[subscripts]
         panel.grid(h = -1, v = -1)
         panel.xyplot(x, y, pch = 21, fill = fill, ...)
       },
       legend = list(right = list(fun = draw.colorkey,
                                  args = list(key = list(col = grey.colors, at = depth.breaks), draw = FALSE))),
       xlab = "Longitude", ylab = "Latitude")



