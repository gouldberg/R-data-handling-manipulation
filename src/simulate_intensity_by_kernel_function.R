setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
# simulate observed spatial points (on 1 dimension)
set.seed(7)
nx <- 10
x <- runif(nx)



# ----------
# bandwidth
# In the context of spatial analysis, it is not clear how to choose an optimal value for the bandwidth in the general case.
# It seems reasonable to use several values depending on the process under consideration, and choose a value that seems plausible.
bw <- .1



# ----------
# quartic kernel (aka biweight) = 3 / pi * (1 - |u|^2)^2  (if u in (-1, 1)), otherwise 0
# |u|^2 : the squared norm of point u = (u1, u2) equal to u1^2 + u2^2
k <- density(x, bw=bw, kernel="biweight")

k

k$x
k$y


k$y <- k$y * nx


plot(k, ylab="Intensity", main="")
points(x, rep(0, nx), pch=20)
for(i in 1:length(x)) lines(density(x[i], bw=bw, kernel="biweight"), lty=2)

legend(x=14, y=0.6, legend=c("Intensity", "Kernel"), lty=c(1,2))
