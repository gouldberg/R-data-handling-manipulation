library(dplyr)
library(sp)



# ---------------------------------------------------------------------------
# parepare
# ---------------------------------------------------------------------------
library(rgdal)
NY8 <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY8_utm18.shp", "NY8_utm18")


# ----------
# read neighbour list object
library(spdep)
NY_nb <- read.gal("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY_nb.gal", region.id = row.names(NY8))


str(NY_nb)

summary(NY_nb)
print(NY_nb)


card(NY_nb)
length(NY_nb)



# ----------
# plot NY_nb on map
plot(NY8, border="grey60", axes=TRUE)
plot(NY_nb, coordinates(NY8), pch=19, cex=0.6, add=TRUE)



# ----------
# subsetting neighbour object
Syracuse <- NY8[NY8$AREANAME == "Syracuse city",]
Sy0_nb <- subset(NY_nb, NY8$AREANAME == "Syracuse city")

summary(Sy0_nb)



# ---------------------------------------------------------------------------
# Simulate spatial autocorrelation using weights
# ---------------------------------------------------------------------------
( Sy0_lw_W <- nb2listw(Sy0_nb) )


# Starting with a vector of random numbers corresponding to the number of census tracs in Syracuse,
# we use the row-standardised contiguity weights to introduce autocorrelation.
n <- length(Sy0_nb)


set.seed(987654)
uncorr_x <- rnorm(n)

rho <- 0.5
( autocorr_x <- invIrW(Sy0_lw_W, rho) %*% uncorr_x )



# ----------
# stats::lag() method for listw objects creates 'spatial lag' values: 
# lag(y(i)) = sum(w(ij) * y(k)), j in N(i), for observed values y(i)
#   - N(i): the set of neighbours of i

# If the weights object style is row-standardisation, the lag(y(i)) values will be averages over the sets of neighbours for each i,
# rather like a moving window defined by N(i) and incuding values weighted by w(ij)
stats::lag(Sy0_lw_W, uncorr_x)
stats::lag(Sy0_lw_W, autocorr_x)


oopar <- par(mfrow=c(1,2), mar=c(4,4,3,2)+0.1)
plot(uncorr_x, stats::lag(Sy0_lw_W, uncorr_x), xlab="", cex.lab=0.8, ylab="spatial lag", main="Uncorrelated random variable", cex.main=0.8)
lines(lowess(uncorr_x, stats::lag(Sy0_lw_W, uncorr_x)), lty=2, lwd=2)
plot(autocorr_x, stats::lag(Sy0_lw_W, autocorr_x), xlab="", ylab="", main="Autocorrelated random variable", cex.main=0.8, cex.lab=0.8)
lines(lowess(autocorr_x, stats::lag(Sy0_lw_W, autocorr_x)), lty=2, lwd=2)
par(oopar)

