setwd("//media//kswada//MyFiles//R_basics")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ethanol
#   - data from ethanol fuel burned in a single-cylinder engine. The emissions of nitrogen oxides should be considered as the response
#     and engine compression and equivalence ratio as the predictors

#   - NOx:  Concentration of nitrogen oxides (NO and NO2) in micrograms/J.
#   - C:  Compression ratio of the engine.
#   - E:  Equivalence ratio–a measure of the richness of the air and ethanol fuel mixture.
# ------------------------------------------------------------------------------

data(ethanol, package="lattice")

str(ethanol)



# ------------------------------------------------------------------------------
# correlation between variables
# ------------------------------------------------------------------------------

pairs(ethanol)

psych::pairs.panels(ethanol)



# ------------------------------------------------------------------------------
# investigate relationship between compression ratio and NOx (response) by equivalence ratio (by equal sample size)
# ------------------------------------------------------------------------------
etha_l <- loess(NOx ~ C, ethanol)

par(mfrow=c(1,1))
i <- order(ethanol$C)
plot(NOx ~ C, ethanol)
lines(etha_l$fitted[i] ~ etha_l$x[i])



# ----------
# equal.count converts x to a shingle using the equal count algorithm. This is essentially a wrapper around
EE <- equal.count(ethanol$E, number=9, overlap=1/4)

xyplot(NOx ~ C | EE, data = ethanol,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       xlab = "Compression ratio", ylab = "NOx (micrograms/J)",
       panel = function(x, y) {
         panel.grid(h=-1, v= 2)
         panel.xyplot(x, y)
         panel.loess(x,y, span=1)
       },
       aspect = "xy")
