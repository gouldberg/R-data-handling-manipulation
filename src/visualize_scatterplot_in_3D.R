library(dplyr)
library(ggplot2)
library(lattice)
library(rgl)
library(car)



# ---------------------------------------------------------------------------
# Visualize scatter plot in 3D
# ---------------------------------------------------------------------------

lung <- read.csv("//media//kswada//MyFiles//R//lung//data.csv", header = T)

str(lung)



# ----------
# only males data

dm <- subset(lung, sex == 1)

dim(dm)



# ----------
# scatter plot in 3D
lattice::cloud(fev ~ height * age, data = dm)


rgl::plot3d(dm$height, dm$age, dm$fev)


car::scatter3d(dm$height, dm$age, dm$fev, xlab = "height", ylab = "age", zlab = "fev", ticktype = "detailed")


