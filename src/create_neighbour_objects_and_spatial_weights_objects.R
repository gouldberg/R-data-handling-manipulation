library(dplyr)
library(sp)



# ---------------------------------------------------------------------------
# Neighbour list object
# ---------------------------------------------------------------------------
library(rgdal)
NY8 <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY8_utm18.shp", "NY8_utm18")
TCE <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//TCE.shp", "TCE")

library(spdep)
NY_nb <- read.gal("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY_nb.gal", region.id = row.names(NY8))
cities <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY8cities.shp", "NY8cities")



# ----------
# in the spdep package, neighbour relationships between n observations are represented by an object of class nb;
# the class is an old-style class.
# It is a list of length n with the index numbers of neighbours of each component recorded as an integer vector
# If any observation has no neighbours, the component contains an integer zero.
# It also contains attributes, typically a vector of character region identifiers, and a logical value indicating whether the relationships are symmetric.
# The region identifiers can be used to check for integrity between the data themselves and the neighbour object.
str(NY_nb)



# ----------
# summary method presents a table of the link number distribution
# reporting asymmetry and the presence of no-neighbour observations
# asymmetry is present when i is an neighbour of j but j is not a neighbour of i.
summary(NY_nb)
print(NY_nb)



# ----------
# card function returns the cardinality of the neighbour set for each object, that is the number of neighbours.
# it differs from the application of length() to the list components because no-neighbour entries are coded as a single element integer vector with the value of zero
card(NY_nb)

length(NY_nb)



# ---------------------------------------------------------------------------
# For reference:  check NY8 (not NY_nb)
# ---------------------------------------------------------------------------
plot(NY8, border="grey60", axes=TRUE)
text(coordinates(cities), labels=as.character(cities$names), font=2, cex=0.9)
text(bbox(NY8)[1,1], bbox(NY8)[2,2], labels="a)", cex=0.8)


plot(NY8, border="grey60", axes=TRUE)
points(TCE, pch=1, cex=0.7)
points(TCE, pch=3, cex=0.7)
text(coordinates(TCE), labels=as.character(TCE$name), cex=0.7, font=1, pos=c(4,1,4,1,4,4,4,2,3,4,2), offset=0.3)
text(bbox(NY8)[1,1], bbox(NY8)[2,2], labels="b)", cex=0.8)



# ---------------------------------------------------------------------------
# plot NY_nb on map
# ---------------------------------------------------------------------------
plot(NY8, border="grey60", axes=TRUE)
plot(NY_nb, coordinates(NY8), pch=19, cex=0.6, add=TRUE)



# ---------------------------------------------------------------------------
# subsetting neighbour object
# ---------------------------------------------------------------------------
Syracuse <- NY8[NY8$AREANAME == "Syracuse city",]
Sy0_nb <- subset(NY_nb, NY8$AREANAME == "Syracuse city")

summary(Sy0_nb)



# ---------------------------------------------------------------------------
# Create neighbour objects
# by k nearest neighbour objects, taking account of differences in the densities of areal entities
#
#   - Naturaly, in the overwhelming majority of cases, it leads to asymmetric neighbours, but will ensure that all areas have k neighbours.
#   - The k = 1 object is also useful in finding the minimum distance at which all areas have a distance-based neighbour.
# ---------------------------------------------------------------------------
coords <- coordinates(Syracuse)

IDs <- row.names(Syracuse)


# ----------
Sy8_nb <- knn2nb(knearneigh(coords, k=1), row.names=IDs)
Sy9_nb <- knn2nb(knearneigh(coords, k=2), row.names=IDs)
Sy10_nb <- knn2nb(knearneigh(coords, k=4), row.names=IDs)


Sy8_nb



# ----------
# ndists() calculate a list of vectors of distances corresponding to the neighbour object, here for first nearest neighbours
( dsts <- unlist(nbdists(Sy8_nb, coords)) )

( Sy11_nb <- dnearneigh(coords, d1=0, d2=0.75*max(dsts), row.names=IDs) )



# ---------------------------------------------------------------------------
# Create Spatial Weights Objects
# ---------------------------------------------------------------------------
# nb2listw() takes a neighbours list object and converts it into a weights object.
# The default conversion style is W, where the weights for each areal entity are standardised to sum to unity;
# this is also often called row standardisation.

( Sy0_lw_W <- nb2listw(Sy0_nb) )


print(Sy0_lw_W)



# ----------
# The neighbours component of the object is the underlying nb object, which gives the indexing of the weights component.
names(Sy0_lw_W)

names(attributes(Sy0_lw_W))


Sy0_lw_W$neighbours
Sy0_lw_W$weights



# ----------
# For style = "W", the weights vary between unity divided by the largest and smallest numbers of neighbours,
# and the sums of weights for each areal entity are unity.
# The weights for links originating at areas with few neighbours are larger than those originating at areas with many neighbours,
# perhaps boosting areal entities on the edge of the study area unintentionally.
1/rev(range(card(Sy0_lw_W$neighbours)))
summary(unlist(Sy0_lw_W$weights))
summary(sapply(Sy0_lw_W$weights, sum))



# ----------
# Setting style = "B" (binary) retains a weight of unity for each neighbour relationship,
# but in this case, the sums of weights for areas differ according to the numbers of neighbour areas have.
( Sy0_lw_B <- nb2listw(Sy0_nb, style="B") )

summary(unlist(Sy0_lw_B$weights))
summary(sapply(Sy0_lw_B$weights, sum))



# ----------
# The glist argument can be used to pass a list of vectors of general weights corresponding to the neighbour relationships to nb2listw.
# We could set the weights to be proportional to the inverse distance between points representing the areas.
dsts <- nbdists(Sy0_nb, coordinates(Syracuse))
idw <- lapply(dsts, function(x) 1/(x/1000))
Sy0_lw_idwB <- nb2listw(Sy0_nb, glist=idw, style="B")

summary(unlist(Sy0_lw_idwB$weights))
summary(sapply(Sy0_lw_idwB$weights, sum))



# ----------
# compare:  style = "W", style = "B", and style = "B" with glist = idw

library(RColorBrewer)
pal <- brewer.pal(9, "Reds")
oopar <- par(mfrow=c(1,3), mar=c(1,1,3,1)+0.1)

z <- t(listw2mat(Sy0_lw_W))
brks <- c(0,0.1,0.143,0.167,0.2,0.5,1)
nbr3 <- length(brks)-3
image(1:63, 1:63, z[,ncol(z):1], breaks=brks, col=pal[c(1,(9-nbr3):9)], main="W style", axes=FALSE)
box()

z <- t(listw2mat(Sy0_lw_B))
image(1:63, 1:63, z[,ncol(z):1], col=pal[c(1,9)], main="B style", axes=FALSE)
box()

z <- t(listw2mat(Sy0_lw_idwB))
brks <- c(0,0.35,0.73,0.93,1.2,2.6)
nbr3 <- length(brks)-3
image(1:63, 1:63, z[,ncol(z):1], breaks=brks, col=pal[c(1,(9-nbr3):9)], main="IDW B style", axes=FALSE)
box()

par(oopar)


# -->
# The style = "W" is evidently asymmetric, with darker colours showing larger weights for areas with few neighbours.

# The other two panels are symmetric, but express different assumptions about the strengths of neighbour relationships.



# ----------
# If there is empty neighbour sets are found, error is raised.
try(Sy0_lw_D1 <- nb2listw(Sy11_nb, style="B"))


# Setting the argument to TRUE permits the creaation of the spatial weights object, with zero weights.
Sy0_lw_D1 <- nb2listw(Sy11_nb, style="B", zero.policy=TRUE)

print(Sy0_lw_D1, zero.policy=TRUE)



# ---------------------------------------------------------------------------
# Read GAL-format, GWT-format file 
# ---------------------------------------------------------------------------
# GAL-format files contain only neighbour information
Sy14_nb <- read.gal("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//Sy_GeoDa1.GAL")

Sy14_nb

isTRUE(all.equal(Sy0_nb, Sy14_nb, check.attributes=FALSE))



# ----------
# GWT-format file also contain distance information for the link between the areas, and are stored in a 3-column sparse representation.
# They can be read using read.gwt2nd, here for a 4-nearest-neighbour scheme, and only using the neighbour links
Sy16_nb <- read.gwt2nb("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//Sy_GeoDa4.GWT")

Sy16_nb

isTRUE(all.equal(Sy10_nb, Sy16_nb, check.attributes=FALSE))



