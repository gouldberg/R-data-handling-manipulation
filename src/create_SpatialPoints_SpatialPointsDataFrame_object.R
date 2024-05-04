library(dplyr)
library(sp)



# ---------------------------------------------------------------------------
# Spatial class
#   - Spatial class has just two slots:
#       1.  bounding box, a matrix of numerical coordinates with column names c('min', 'max'),
#           amd at least two rows, with the 1st row eastings (x-axis) and the second northings (y-axis)
#       2.  CRS class object defining the coordinate reference system
# ---------------------------------------------------------------------------
# we can getClass to return the complete definition of a class, including its slot names and the types of their contents
getClass("Spatial")


# -->
# getClass also returns known subclases, showing the classes that include the Spatial class in their definitions.


getClass("CRS")



# ---------------------------------------------------------------------------
#  build a simple Spatial object from a bounding box matrix and a missing coordinate reference system
# ---------------------------------------------------------------------------
# bounding box
m <- matrix(c(0,0,1,1), ncol=2, dimnames=list(NULL, c("min", "max")))

# CRS
crs <- CRS(projargs=as.character(NA))


# ----------
S <- Spatial(bbox=m, proj4string=crs)

S


# ----------
bb <- matrix(c(350, 85, 370, 95), ncol=2, dimnames=list(NULL, c("min", "max")))

res <- Spatial(bb, proj4string=CRS("+proj=longlat +ellps=WGS84"))
# res <- try(Spatial(bb, proj4string=CRS("+proj=longlat +ellps=WGS84")))

res



# ---------------------------------------------------------------------------
# SpatialPoints
#   - Classes in sp are not atomic: there is no SpatialPoint (not SpatialPoints) class that is extended by SpatialPoints
# ---------------------------------------------------------------------------
# data file with the positions of CRAN mirrors across the world in 2005.
CRAN_df <- read.table("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//cm_bundle//CRAN051001a.txt", header=TRUE)

str(CRAN_df)


# ----------
CRAN_mat <- cbind(CRAN_df$long, CRAN_df$lat)

row.names(CRAN_mat) <- 1:nrow(CRAN_mat)

str(CRAN_mat)

CRAN_mat



# ----------
getClass("SpatialPoints")

llCRS <- CRS("+proj=longlat +ellps=WGS84")

CRAN_sp <- SpatialPoints(CRAN_mat, proj4string=llCRS)



# ----------
# summary method shows the bounding box, whether the object is projected, and the number of rows of coordinates
# here FALSE, because the string "longlat" is included in the projection description
summary(CRAN_sp)


# Note that SpatialPoints objects may have more than two dimensions,
# but plot methods for the class use only the first two.


# ----------
# bbox method returns the bounding box of the object
# 1st row:  west-east range   2nd row: the sourth-north direction
bbox(CRAN_sp)



# ----------
# the basic method reports the projection string contained as a CRS object in the proj4string slot of the object, 
# and it also has an assignment form
proj4string(CRAN_sp)


proj4string(CRAN_sp) <- CRS(as.character(NA))
proj4string(CRAN_sp)
proj4string(CRAN_sp) <- llCRS



# ----------
# Extract the coordinates from a SpatialPoints object as a numeric matrix
brazil <- which(CRAN_df$loc == "Brazil")
brazil

coordinates(CRAN_sp)[brazil,]


# SpatialPoints object can also be accessed by index
# The "[" operator also works for negative indices, which remove those coordinates from the object.
summary(CRAN_sp[brazil,])

south_of_equator <- which(coordinates(CRAN_sp)[,2] < 0)
summary(CRAN_sp[-south_of_equator,])



# ---------------------------------------------------------------------------
# Data Frames for Spatial Point Data (SpatialPointsDataFrame)
# ---------------------------------------------------------------------------
# If the matrix of point coordinates has row names and the match.ID argument is set to its default value of TRUE,
# then the matrix row names are checked against the row names of the data frame.
# If they match, but are not in the same order, the data frame rows are re-ordered to suit the points

CRAN_mat

head(CRAN_df)
str(CRAN_df)


# ----------
# create SpatialPointsDataFrame
CRAN_spdf1 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df, proj4string=llCRS, match.ID=TRUE)

str(CRAN_spdf1)

head(CRAN_spdf1)

CRAN_spdf1[4,]



# ----------
# If we re-order the data frame at random sampling, we still get the same result, 
# because the data frame is re-ordered to match the row names of the points
s <- sample(nrow(CRAN_df))
CRAN_spdf2 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df[s,], proj4string=llCRS, match.ID=TRUE)
all.equal(CRAN_spdf2, CRAN_spdf1)

CRAN_spdf2[4,]



# ----------
# But if we have non-matching ID values, created by pasting pairs of letters together and sampling an appropriate number of them
# the result is an error:
CRAN_df1 <- CRAN_df
row.names(CRAN_df1) <- sample(c(outer(letters, letters, paste, sep="")), nrow(CRAN_df1))
try(CRAN_spdf3 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df1, proj4string=llCRS, match.ID=TRUE))



# ---------------------------------------------------------------------------
# Create Data Frames for Spatial Point Data (SpatialPointsDataFrame) from SpatialPoints object
# ---------------------------------------------------------------------------
# We can also construct SpatialPontsDataFrame from SpatialPoints object
CRAN_spdf4 <- SpatialPointsDataFrame(CRAN_sp, CRAN_df)
all.equal(CRAN_spdf4, CRAN_spdf2)



# ----------
# We can also assign coordinates to a data frame
CRAN_df0 <- CRAN_df
coordinates(CRAN_df0) <- CRAN_mat
proj4string(CRAN_df0) <- llCRS
all.equal(CRAN_df0, CRAN_spdf2)


# objects created in this way differ slightly from those we have seen before,
# because the coords.nrs slot is now used, and the coordinates are moved from the data slot toe the coords slot, 
# but the objects are otherwise the same.
str(CRAN_sp, max.level=2)
str(CRAN_df0, max.level=2)


CRAN_df1 <- CRAN_df
names(CRAN_df1)
coordinates(CRAN_df1) <- c("long", "lat")
proj4string(CRAN_df1) <- llCRS

str(CRAN_df1, max.level=2)




# ---------------------------------------------------------------------------
# Transect and tracking data may also be represented as points,
# because the observation at each point contributes information that is associated with the point itself,
# rather than a whole.
# Sequence numbers can be entered into the data frame to make it possible to trace the points in order
# ---------------------------------------------------------------------------
turtle_df <- read.csv("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//cm_bundle//seamap105_mod.csv")

str(turtle_df)

summary(turtle_df)

head(turtle_df)


# ----------
# Before creating a SpatialPointsDataFrame, we will timestamp the observations, and re-order the input data frame by timestamp to make it easier to add months
# to show progress westwards across the Pacific
timestamp <- as.POSIXlt(strptime(as.character(turtle_df$obs_date), "%m/%d/%Y %H:%M:%S"), "GMT")
turtle_df1 <- data.frame(turtle_df, timestamp=timestamp)
turtle_df1$lon <- ifelse(turtle_df1$lon < 0, turtle_df1$lon+360, turtle_df1$lon)
turtle_sp <- turtle_df1[order(turtle_df1$timestamp),]

coordinates(turtle_sp) <- c("lon", "lat")
proj4string(turtle_sp) <- CRS("+proj=longlat +ellps=WGS84")


head(turtle_sp)

