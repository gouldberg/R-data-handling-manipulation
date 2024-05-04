library(sp)


# ---------------------------------------------------------------------------
# A straight Trellis example
#  - plotting of 2 interpolation scenrios for the zinc variable in the meuse data set, obtained on the direct scale and on the log scale.
# ---------------------------------------------------------------------------
library(gstat)
library(sp)

data(meuse)
coordinates(meuse) <- ~ x + y

data(meuse.grid)
coordinates(meuse.grid) <- ~ x + y 



# ----------
gridded(meuse.grid) <- T
zn <- krige(zinc ~ 1, meuse, meuse.grid)
zn$direct <- zn$var1.pred
zn$log <- exp(krige(log(zinc) ~ 1, meuse, meuse.grid)$var1.pred)

zn



# ----------
library(lattice)

# helper function spmap.to.lev converts the SpatialPixelsDataFrame object to special format by replicating the coordinates, stacking the attribute variables and adding a factor to distinguish the two maps
print(levelplot(z ~ x + y | name, spmap.to.lev(zn[c("direct", "log")]), asp = "iso", cuts=4, col.regions= brewer.pal(5, "Reds")), split = c(1,1,1,2), more = TRUE)
print(spplot(zn[c("direct", "log")], cuts=4, col.regions= brewer.pal(5, "Reds")), split = c(1,2,1,2))



# ----------
# spplot
spplot(zn[c("direct", "log")])



# ---------------------------------------------------------------------------
# Plotting points
# ---------------------------------------------------------------------------
# Soil measurements for 4 heavy metals in the Meuse data set
cuts=c(0,20,50,200,500,2000)
grys <- brewer.pal(7, "Reds")
print(spplot(meuse[1:4], main = "ppm", cuts=cuts, cex=.5, col.regions=grys, key.space="right"), split=c(1,1,1,2), more=TRUE)



# each variable scaled to mean zero and unit standard variance
meuse$lead.st = as.vector(scale(meuse$lead))
meuse$zinc.st = as.vector(scale(meuse$zinc))
meuse$copper.st = as.vector(scale(meuse$copper))
meuse$cadmium.st = as.vector(scale(meuse$cadmium))
cuts=c(-1.2,0,1,2,3,5)
print(spplot(meuse, c("cadmium.st", "copper.st", "lead.st", "zinc.st"), key.space="right", main = "standardised", cex = .5, cuts = cuts, col.regions=grys), split=c(1,2,1,2))



# ---------------------------------------------------------------------------
# Plotting contourlines, grid plot of a numerical variables and factor variable
# ---------------------------------------------------------------------------
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")


meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")


# contour lines for distance to river Meuse, levels represented by grey tones
cl <- ContourLines2SLDF(contourLines(as.image.SpatialGridDataFrame(meuse.grid["dist"])))
print(spplot(cl, colorkey=list(height=0.8, width=0.6), col.regions=grys), split = c(1,1,3,1), more=TRUE)


# grid plot af a numerical variable
grys <- brewer.pal(6, "Reds")
cuts = (0:5)/5
print(spplot(meuse.grid, "dist", at=cuts, colorkey=list(labels=list(at=cuts), at=cuts), col.regions=grys), split = c(2,1,3,1), more = TRUE)


# plot of the factor variable flood frequency
meuse.grid$f <- factor(meuse.grid$ffreq, labels = c("annual", "2-5 yrs", "> 5 yrs"))
print(spplot(meuse.grid, "f", colorkey=list(height=0.4, width=0.6), col.regions= brewer.pal(3, "Reds")), split = c(3,1,3,1), more=FALSE)



# ---------------------------------------------------------------------------
# Adding reference and layout elements to plots  (scale and north arrow)
# ---------------------------------------------------------------------------
river <- list("sp.polygons", meuse.pol)
north <- list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(178750,332500), scale = 400)
scale <- list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(180200, 329800), scale = 1000, fill=c("transparent","black"))
txt1 <- list("sp.text", c(180200, 329950), "0")
txt2 <- list("sp.text", c(181200, 329950), "1 km")
pts <- list("sp.points", meuse, pch = 3, col = "black")

meuse.layout <- list(river, north, scale, txt1, txt2, pts)

grys <- brewer.pal(7, "Reds")


# Method spplot takes a single argument, sp.layout, to annotate plots with lines, points, grids, polygons, text, or combinations of these.
# The order of items in the sp.layout argument matters; in principle objects are drawn in the order they appear.
# By default, when the object of spplot has points or lines, sp.layout items are drawn before the points to allow grids and polygons drawn as a background.
# For grids and polygons, sp.layout items are drawn afterwards.
# For grids, adding a list element first = TRUE ensures that the item is drawn before the grid is drawn.
print(spplot(zn["log"], sp.layout = meuse.layout, cuts=5, col.regions=grys))



# ---------------------------------------------------------------------------
# latticeExtra
#   - As objects returned by spplot are trellis objects, they can be combined by "+" operator
# ---------------------------------------------------------------------------
library(latticeExtra)

data(meuse.riv)
( meuse.lst <- list(Polygons(list(Polygon(meuse.riv)), "meuse.riv")) )
( meuse.pol <- SpatialPolygons(meuse.lst) )

reds <- brewer.pal(7, "Reds")
p <- spplot(meuse["zinc"], cuts = 5, col.regions = reds)
m <- SpatialPolygonsDataFrame(meuse.pol, data.frame(col=1), match.ID = FALSE)

l <- spplot(m, cuts = 5, col.regions = reds)


print(l + p, more = TRUE, split = c(1,1,2,1))

print(p + l, split = c(2,1,2,1))



# ---------------------------------------------------------------------------
# Interactive plots:  identify points
# ---------------------------------------------------------------------------
# This will show the points selected and return the selected points' row numbers.
ids <- spplot(meuse, "zinc", identify = TRUE)



# ----------
# This is the same
library(lattice)
trellis.focus("panel", column = 1, row = 1)
ids <- panel.identify()
trellis.unfocus()



# ----------
# grid.locator():  identify only one point
spplot(meuse, "zinc")

library(grid)
trellis.focus("panel", column = 1, row = 1)
as.numeric(grid.locator())
trellis.unfocus()



# ----------
# spplot.locator():  identify multiple points
# It returns a two-column matrix with spatial coordinates
library(grid)
trellis.focus("panel", column = 1, row = 1)
as.numeric(spplot.locator())
trellis.unfocus()



# ---------------------------------------------------------------------------
# Colour paletts and class intervals
# empirical cumulative distribution and classification by class intervals
# ---------------------------------------------------------------------------
rw.colors <- colorRampPalette(c("red", "white"))

image(meuse.grid["dist"], col=rw.colors(10))



# ----------
# Check brewer.pal
library(RColorBrewer)
example(brewer.pal)



# ----------
# the empirical cumulative distribution with classfification (style = "quantile")
library(classInt)
pal <- brewer.pal(5, "Reds")
q5 <- classIntervals(meuse$zinc, n=5, style="quantile")

q5

diff(q5$brks)

plot(q5, pal=pal)


# -->
# While the number of sites in each class is equal by definition,
# the observed values are far from uniformly distributed.
# Examining the widths of the classes using diff on the class breaks shows that many sites with moderate zinc values will be assigned to the 
# darkest colour class.



# ----------
# the empirical cumulative distribution with classfification (style = "fisher": five-class Fisher-Jenks classification)
# This 'natural breaks' set of class intervals is based on minimising the within-class variance
fj5 <- classIntervals(meuse$zinc, n=5, style="fisher")

fj5

diff(fj5$brks)

plot(fj5, pal=pal)



# ----------
# Once we are satisfied with the chosen class intervals and palette, we can go on to plot the data
oopar <- par(mar=c(1,1,3,1)+0.1, mfrow=c(1,2))
q5Colours <- findColours(q5, pal)
plot(meuse, col=q5Colours, pch=19)
points(meuse, pch=1)
box()
title(main="Quantile")
legend("topleft", fill=attr(q5Colours, "palette"), legend=names(attr(q5Colours,"table")), bty="n", cex=0.8, y.intersp=0.8)

fj5Colours <- findColours(fj5, pal)
plot(meuse, col=fj5Colours, pch=19)
points(meuse, pch=1)
box()
title(main="Fisher-Jenks")
legend("topleft", fill=attr(fj5Colours, "palette"), legend=names(attr(fj5Colours, "table")), bty="n", cex=0.8, y.intersp=0.8)
par(oopar)


# -->
# Using quantile-based class intervals, it appears that almost all th eriver bank sites are equally polluted,
# while the natural breaks intervals discriminate better.



