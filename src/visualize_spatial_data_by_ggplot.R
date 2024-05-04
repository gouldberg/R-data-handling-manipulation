library(ggplot2)


# ---------------------------------------------------------------------------
# ggplot
#  - ggplot takes an object and will try to convert it to a data.frame using method fortify
# ---------------------------------------------------------------------------
# data frame version
data(meuse)
ggplot(m, aes(x,y)) + geom_point() + coord_equal()



# ----------
# Check the fortify methods exist for the class
methods(fortify)


library(gstat)
library(sp)


# convert to SpatialPointsDataFrame
coordinates(meuse) <- ~ x + y
str(meuse)


# convert back to data frame by fortify method
m <- as(meuse, "data.frame")


# geom_point(): the plot should be a scatter plot
# coord_equal(): makes sure that units along the x-axis equal those along the y-axis (data are projected)
ggplot(m, aes(x,y)) + geom_point() + coord_equal()


