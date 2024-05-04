library(dplyr)
library(sp)



# ---------------------------------------------------------------------------
# SpatialLines class
# ---------------------------------------------------------------------------
# Neither Line nor Lines objects inherit from the Spatial class.
# It is the SpatialLines object that contains the bounding box and projection information for the list of Lines objects stored in its lines slot.
getClass("Line")

getClass("Lines")

getClass("SpatialLines")



# ---------------------------------------------------------------------------
# convert to SpatialLines class
# ---------------------------------------------------------------------------
library(maps)
japan <- map("world", "japan", plot=FALSE)

str(japan)



# ----------
# convert to a SpatialLines object using the map2SpatialLines
library(maptools)
p4s <- CRS("+proj=longlat +ellps=WGS84")
SLjapan <- map2SpatialLines(japan, proj4string=p4s)


# We can see that the lines slot of the object is a list of 34 components, 
# each of which must be a Lines object in a valid SpatialLines object
str(SLjapan, max.level=2)



# ----------
# return the length of the Lines slot
Lines_len <- sapply(slot(SLjapan, "lines"), function(x) length(slot(x, "Lines")))
Lines_len

table(Lines_len)



# ---------------------------------------------------------------------------
# convert to SpatialLinesDataFrame object
# ---------------------------------------------------------------------------
# convert data returned by the base graphics function contourLines into a SpatialLinesDataFrame object
volcano_sl <- ContourLines2SLDF(contourLines(volcano))

volcano_sl

# we can see that there are 10 separate contour level labels in the variable in the data slot
t(slot(volcano_sl, "data"))



# ---------------------------------------------------------------------------
# read shoreline data in 'Mapgen' format from the National Geophysical Data Center costline extractor
# into a SpatialLines object
# ---------------------------------------------------------------------------
llCRS <- CRS("+proj=longlat +ellps=WGS84")

auck_shore <- MapGen2SL("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//cm_bundle//auckland_mapgen.dat", llCRS)

summary(auck_shore)

