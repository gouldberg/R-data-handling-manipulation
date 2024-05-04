setwd("//media//kswada//MyFiles//R_basics")

packages <- c("dplyr", "datasets", "popbio")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Fit a nonparametric density:  gamlss::histSmo,  MASS::truehist
# ------------------------------------------------------------------------------
data(acidity, package = "gamlss.data")

str(acidity)


# ----------
h1 <- histSmo(acidity$y, plot = TRUE)


h1$hist

h1$density



# ----------
MASS::truehist(acidity$y, col = "grey")

lines(histSmo(acidity$y), lty = 1, lwd = 2)

lines(histSmo(acidity$y, df = 4), lty = 2, lwd = 2)

legend("topleft", legend = c("local ML", "fixed df"), lty = 1:2, cex = 1)





