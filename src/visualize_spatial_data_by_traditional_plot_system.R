library(sp)


data(meuse)
data(meuse.riv)
data(meuse.grid)


# ----------
# This is only ordinary data.frame
str(meuse)


# This is matrix 
str(meuse.riv)
meuse.riv


# This is the data frame
str(meuse.grid)



# ---------------------------------------------------------------------------
# plotting points, lines, polygons and grid
# ---------------------------------------------------------------------------
# plotting points
# the plot method shows the points with the default symbol
coordinates(meuse) <- c("x", "y")

# This is SpatialPointsDataFrame
str(meuse)

plot(meuse)
title("points")



# ----------
# plotting lines
# SpatialLines object is made by joining up the points in sequence, and plot draws the resulting zig-zags
cc <- coordinates(meuse)
cc

( m.sl <- SpatialLines(list(Lines(list(Line(cc)), "line1"))) )

plot(m.sl)
title("lines")



# ----------
# polygons outlining the banks of the River Meuse
( meuse.lst <- list(Polygons(list(Polygon(meuse.riv)), "meuse.riv")) )

( meuse.pol <- SpatialPolygons(meuse.lst) )

plot(meuse.pol, col = "grey")
title("polygons")



# ----------
# plotting grid
coordinates(meuse.grid) <- c("x", "y")

( meuse.grid <- as(meuse.grid, "SpatialPixels") )

image(meuse.grid, col = "grey")
title("grid")



# ---------------------------------------------------------------------------
# combining several elements
# ---------------------------------------------------------------------------
image(meuse.grid, col = "lightgrey")
plot(meuse.pol, col = "grey", add = TRUE)
plot(meuse, add = TRUE)



# ---------------------------------------------------------------------------
# change aspect ratio
# ---------------------------------------------------------------------------
# On each map, one unit in the x-direction equals one unit in the y-direction, 
# this is the default when the coordinate reference system is not longlat or is unknown.
# For unprojected data in geographical coordinates (longitude/latitude), the default aspect ratio depends on the (mean) latitude of the area plotted.

par(mfrow=c(1,3))
image(meuse.grid, col = "grey", asp = 0.8)
title("grid, asp = 0.8")
image(meuse.grid, col = "grey", asp = 1.0)
title("grid, asp = 1.0")
image(meuse.grid, col = "grey", asp = 1.2)
title("grid, asp = 1.2")



# ---------------------------------------------------------------------------
# Axes and layout elements
# ---------------------------------------------------------------------------
# basics: axes and layout
layout(matrix(c(1,2),1,2))
plot(meuse.pol, axes = TRUE)
plot(meuse.pol, axes = FALSE)
axis(1, at = c(17800 + 0:2 * 2000), cex.axis = 0.7)
axis(2, at = c(32600 + 0:3 * 4000), cex.axis = 0.7)
box()



# ---------------------------------------------------------------------------
# Scale bar and a north arrow
# ---------------------------------------------------------------------------
layout(matrix(c(1,1),1,1))
plot(meuse)
plot(meuse.pol, add = TRUE, col = "grey")

SpatialPolygonsRescale(layout.north.arrow(), offset = locator(1), scale = 400, plot.gird = FALSE)

SpatialPolygonsRescale(layout.scale.bar(), offset = locator(1), scale = 1000, fill = c("transparent", "black"), plot.grid = FALSE)
text(locator(1), "0")
text(locator(1), "1 km")




# ---------------------------------------------------------------------------
# degrees in axes labels and reference grid
# ---------------------------------------------------------------------------

library(maptools)
library(maps)

wrld <- map("world", interior = FALSE, xlim = c(-179, 179), ylim = c(-89, 89), plot = FALSE)
wrld_p <- pruneMap(wrld, xlim = c(-179, 179))

llCRS <- CRS("+proj=longlat +ellps=WGS84")
wrld_sp <- map2SpatialLines(wrld_p, proj4string = llCRS)
prj_new <- CRS("+proj=moll +ellps=WGS84")

wrld_sp



# ----------
# plotting world
library(rgdal)
wrld_proj <- spTransform(wrld_sp, prj_new)
plot(wrld_proj, col = "grey60")



# ----------
# gridlines draws line generate an object of class SpatialLines
# by default it draws lines withing the bounding box of the object at values where the default axes lables are drawn.
# grid lines may be latitude/longitude grids, and these are non-straight lines
wrld_grd <- gridlines(wrld_sp, easts = c(-179, seq(-150, 150, 50), 179.5), norths = seq(-75, 75, 15), ndiscr = 100)
wrld_grd_proj <- spTransform(wrld_grd, prj_new)

plot(wrld_grd_proj, add = TRUE, lty = 3, col = "grey70")



# ----------
# gridat returns object to draw the labels for these gridded curves
at_sp <- gridat(wrld_sp, easts = 0, norths = seq(-75, 75, 15), offset = 0.3)
at_proj <- spTransform(at_sp, prj_new)
text(coordinates(at_proj), pos = at_proj$pos, offset = at_proj$offset, labels = parse(text = as.character(at_proj$labels)), cex = 0.6)



# ---------------------------------------------------------------------------
# plotting attributes and map legends
# ---------------------------------------------------------------------------
gridded(meuse.grid) <- TRUE

library(gstat)
zn.idw <- krige(log(zinc) ~ 1, meuse, meuse.grid)


# -----------
library(RColorBrewer)
cols <- brewer.pal(4, "Reds")

image(zn.idw, col = cols, breaks=log(c(100,200,400,800,1800)))

plot(meuse.pol, add = TRUE)
plot(meuse, pch = 1, cex = sqrt(meuse$zinc)/20, add = TRUE)

legVals <- c(100, 200, 500, 1000, 2000)
legend("left", legend=legVals, pch = 1, pt.cex = sqrt(legVals)/20, bty = "n", title="measured, ppm", cex=0.8, y.inter=0.9)
legend("topleft", fill = cols, legend=c("100-200","200-400","400-800", "800-1800"), bty = "n", title = "interpolated, ppm", cex=0.8, y.inter=0.9)
title("measured and interpolated zinc")



# ---------------------------------------------------------------------------
# Interactive plots:  individual identification of points
# ---------------------------------------------------------------------------
plot(meuse)

# after this line of script, identify on the graph the points and click finish
meuse.id <- identify(coordinates(meuse))



# ---------------------------------------------------------------------------
# Interactive plots:  digitizing a region
# ---------------------------------------------------------------------------
plot(meuse)


# Note that we manually close the polygon by adding the first point to the set of points digitized.
region <- locator(type="o")


n <- length(region$x)
p <- Polygon(cbind(region$x,region$y)[c(1:n,1),], hole=FALSE)
ps <- Polygons(list(p), ID = "region")
sps <- SpatialPolygons(list(ps))

plot(meuse[sps,], pch=16, cex=.5, add=TRUE)




# ---------------------------------------------------------------------------
# Identify particular polygons
# ---------------------------------------------------------------------------
library(maptools)

prj <- CRS("+proj=longlat +datum=NAD27")
nc_shp <- system.file("shapes/sids.shp", package="maptools")[1]
nc <- readShapePoly(nc_shp, proj4string=prj)


plot(nc)
pt <- locator(type="p")


print(pt)
pt.sp = SpatialPoints(cbind(pt$x,pt$y),proj4string=prj)
over(pt.sp, nc)



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


