library(dplyr)
library(sp)



# ---------------------------------------------------------------------------
# SpatialPolygons class
# ---------------------------------------------------------------------------

# Polygon class extends the Line class by adding slots needed for polygons and checking that the first and last coordinates are identical.
# The extra slots are a label point, taken as the centroid of the polygon, the area of the polygon in the metric of the coordinates,
# whether the polygon is declared as ahole or not
getClass("Polygon")

getClass("Polygons")


getClass("SpatialPolygons")



# ---------------------------------------------------------------------------
# SpatialPolygons
# ---------------------------------------------------------------------------
# read shoreline data in 'Mapgen' format from the National Geophysical Data Center costline extractor
# into a SpatialLines object
llCRS <- CRS("+proj=longlat +ellps=WGS84")

auck_shore <- MapGen2SL("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//cm_bundle//auckland_mapgen.dat", llCRS)

summary(auck_shore)



# ----------
( lns <- slot(auck_shore, "lines") )


# identify the closed lines:  the first point is the same as the last point
( islands_auck <- sapply(lns, function(x) {
  crds <- slot(slot(x, "Lines")[[1]], "coords")
  identical(crds[1,], crds[nrow(crds),])
}) )


( islands_sl <- auck_shore[islands_auck] )


( list_of_Lines <- slot(islands_sl, "lines") )



# ----------
# convert to SpatialPolygons class
islands_sp <- SpatialPolygons(lapply(list_of_Lines, function(x) {
  Polygons(list(Polygon(slot(slot(x, "Lines")[[1]], "coords"))), ID=slot(x, "ID"))
}), proj4string=CRS("+proj=longlat +ellps=WGS84"))


summary(islands_sp)



# ----------
par(mfrow=c(1,2))
plot(auck_shore)
legend("bottomleft", legend="a)", bty="n")

plot(auck_shore)
plot(islands_sp, add=TRUE, col="grey")
legend("bottomleft", legend="b)", bty="n")



# ----------
lns <- slot(auck_shore, "lines")

table(sapply(lns, function(x) length(slot(x, "Lines"))))

islands_auck <- sapply(lns, function(x) {
  crds <- slot(slot(x, "Lines")[[1]], "coords")
  identical(crds[1,], crds[nrow(crds),])
})

table(islands_auck)



slot(islands_sp, "plotOrder")

order(sapply(slot(islands_sp, "polygons"),
             function(x) slot(x, "area")), decreasing=TRUE)



# ---------------------------------------------------------------------------
# convert the boundaries to a SpatialPolygons object
# ---------------------------------------------------------------------------
library(maps)

state.map <- map("state", plot=FALSE, fill=TRUE)

str(state.map)



# ----------
# convert to SpatialPolygons object
library(maptools)
IDs <- sapply(strsplit(state.map$names, ":"), function(x) x[1])
state.sp <- map2SpatialPolygons(state.map, IDs=IDs,
                                proj4string=CRS("+proj=longlat +ellps=WGS84"))


summary(state.sp)



# ---------------------------------------------------------------------------
# convert to SpatialPolygonsDataFrame
# ---------------------------------------------------------------------------
sat <- read.table("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//cm_bundle//state.sat.data_mod.txt", row.names=5, header=TRUE)

str(sat)



# ----------
# We can use identifying tag matching to suit the rows of the data frame to the SpatilaPolygons.
# Here we subset to the matched rows of the data frame, to ensure that one row corresponds to each Polygons object, to achieve one-to-one matching.
id <- match(row.names(sat), row.names(state.sp))

row.names(sat)[is.na(id)]

sat1 <- sat[!is.na(id),]



# ----------
# convert to SpatialPolygonsDataFrame
state.spdf <- SpatialPolygonsDataFrame(state.sp, sat1)

str(slot(state.spdf, "data"))

str(state.spdf, max.level=2)



# ----------
# If we modify the row name of "arizona' in the data frame to 'Arizona', there is no longer a match with a polygon identifying tag,
# and error is signalled.
rownames(sat1)[2] <- "Arizona"

try(SpatialPolygonsDataFrame(state.sp, sat1))



#library(maptools)
#high <- Rgshhs("/home/rsb/tmp/gshhs/GSHHS220/gshhs/gshhs_h.b", xlim=c(277,278), ylim=c(45.7,46.2))
#save(high, file="../Data/high.RData")


###################################################
### code chunk number 80: cm.Rnw:1597-1601
###################################################
load("high.RData")
manitoulin_sp <- high$SP


###################################################
### code chunk number 81: cm.Rnw:1603-1606
###################################################
length(slot(manitoulin_sp, "polygons"))
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "hole"))
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "ringDir"))


###################################################
### code chunk number 82: cm.Rnw:1643-1644
###################################################
library(rgeos)


###################################################
### code chunk number 83: cm.Rnw:1646-1648
###################################################
manitoulin_sp <- createSPComment(manitoulin_sp)
sapply(slot(manitoulin_sp, "polygons"), comment)


###################################################
### code chunk number 84: cm.Rnw:1674-1687
###################################################
plot(manitoulin_sp, pbg="lightsteelblue2", col="khaki2", usePolypath=FALSE)
text(t(sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "labpt")))[-c(1,2),], label=high$polydata$level[-c(1,2)], col="black", font=2)
cmt <- unlist(strsplit(sapply(slot(manitoulin_sp, "polygons"), comment), " "))
plot(manitoulin_sp, pbg="lightsteelblue2", col="khaki2", usePolypath=FALSE)
text(t(sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"), function(x) slot(x, "labpt")))[-c(1,2),], label=cmt[-c(1,2)], col="black", font=2)


###################################################
### code chunk number 85: cm.Rnw:1719-1720
###################################################
getClass("GridTopology")


###################################################
### code chunk number 86: cm.Rnw:1732-1739
###################################################
bb <- bbox(manitoulin_sp)
bb
cs <- c(0.01, 0.01)
cc <- bb[,1]+(cs/2)
cd <- ceiling(diff(t(bb))/cs)
manitoulin_grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
manitoulin_grd


###################################################
### code chunk number 87: cm.Rnw:1754-1755 
###################################################
getClass("SpatialGrid")


###################################################
### code chunk number 89: cm.Rnw:1772-1775
###################################################
p4s <- CRS(proj4string(manitoulin_sp))
manitoulin_SG <- SpatialGrid(manitoulin_grd, proj4string=p4s)
summary(manitoulin_SG)


###################################################
### code chunk number 90: cm.Rnw:1781-1794
###################################################
#library(rgdal)
#auck_el1 <- readGDAL("../Data/70042108.tif")
#save(auck_el1, file="../Data/auck_el1.RData")
#library(maptools)
#auck2 <- Rgshhs("/home/rsb/tmp/gshhs/GSHHS220/gshhs/gshhs_f.b", xlim=c(174.2,175.3), ylim=c(-37.5,-36.5), level=1)
#auck_gshhs <- auck2$SP
#auck_gshhs <- createSPComment(auck_gshhs)
#save(auck_gshhs, file="../Data/auck_gshhs.RData")
load("auck_el1.RData")
is.na(auck_el1$band1) <- auck_el1$band1 <= 0
load("auck_gshhs.RData")


###################################################
### code chunk number 91: cm.Rnw:1818-1823
###################################################
class(auck_el1)
slot(auck_el1, "grid")
slot(auck_el1, "bbox")
object.size(auck_el1)
object.size(slot(auck_el1, "data"))


###################################################
### code chunk number 92: cm.Rnw:1833-1835
###################################################
is.na(auck_el1$band1) <- auck_el1$band1 <= 0
summary(auck_el1$band1)


###################################################
### code chunk number 93: cm.Rnw:1870-1875
###################################################
#auck_el2 <- as(auck_el1, "SpatialPixelsDataFrame")
#save(auck_el2, file="../Data/auck_el2.RData")
load("auck_el2.RData")


###################################################
### code chunk number 95: cm.Rnw:1880-1885
###################################################
object.size(auck_el2)
object.size(slot(auck_el2, "grid.index"))
object.size(slot(auck_el2, "coords"))
sum(is.na(auck_el1$band1)) + nrow(slot(auck_el2, "coords"))
prod(slot(slot(auck_el2, "grid"), "cells.dim"))


###################################################
### code chunk number 96: cm.Rnw:1909-1912
###################################################
auck_el_500 <- auck_el2[auck_el2$band1 > 500,]
summary(auck_el_500)
object.size(auck_el_500)


###################################################
### code chunk number 97: cm.Rnw:1932-1938
###################################################
data(meuse.grid)
mg_SP <- SpatialPoints(cbind(meuse.grid$x, meuse.grid$y))
summary(mg_SP)
mg_SPix0 <- SpatialPixels(mg_SP)
summary(mg_SPix0)
prod(slot(slot(mg_SPix0, "grid"), "cells.dim"))


###################################################
### code chunk number 98: cm.Rnw:1955-1957
###################################################
mg_SPix1 <- as(mg_SP, "SpatialPixels")
summary(mg_SPix1)


###################################################
### code chunk number 100: cm.Rnw:1983-1984
###################################################
library(raster)


###################################################
### code chunk number 101: cm.Rnw:1986-1987
###################################################
r <- raster("70042108.tif")


###################################################
### code chunk number 103: cm.Rnw:1992-1998
###################################################
class(r)
inMemory(r)
object.size(r)
cellStats(r, max)
cellStats(r, min)
inMemory(r)


###################################################
### code chunk number 104: cm.Rnw:2012-2024
###################################################
out <- raster(r)
bs <- blockSize(out)
out <- writeStart(out, filename=tempfile(), overwrite=TRUE)
for (i in 1:bs$n) {
  v <- getValues(r, row=bs$row[i], nrows=bs$nrows[i])
  v[v <= 0] <- NA
  writeValues(out, v, bs$row[i])
}
out <- writeStop(out)
cellStats(out, min)
cellStats(out, max)
inMemory(out)


###################################################
### code chunk number 106: cm.Rnw:2044-2052
###################################################
plot(out, col=terrain.colors(100))
plot(auck_gshhs, add=TRUE)


###################################################
### code chunk number 107: cm.Rnw:2069-2073
###################################################
r1 <- as(out, "SpatialGridDataFrame")
summary(r1)
r2 <- as(r1, "RasterLayer")
summary(r2)



