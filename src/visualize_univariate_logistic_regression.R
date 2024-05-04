setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "datasets", "popbio")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Univariate logistic regression by popbio
# ------------------------------------------------------------------------------
data("VictimsOfCrime", package = "gamlss.data")


str(VictimsOfCrime)



# ----------
# Univariate logistic regression

popbio::logi.hist.plot(VictimsOfCrime$age, VictimsOfCrime$reported, col = "skyblue",
                       type =" hist", boxp = TRUE,  rug = TRUE, counts = FALSE)
