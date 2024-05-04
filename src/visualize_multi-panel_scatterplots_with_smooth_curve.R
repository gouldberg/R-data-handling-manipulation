setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ---------------------------------------------------------------------------
# Target variable vs. continuous variable (covariate) with smooth curve by xyplot
# ---------------------------------------------------------------------------
# ----------
# xyplot()
# type = "smooth"
xyplot(hwy ~ displ | class, data = mpg, type = c("g", "p", "smooth"), pch = 20)



# ------------------------------------------------------------------------------
# Target variable vs. continuous variable (covariate) with smooth curve by featurePlot 
# ------------------------------------------------------------------------------
data("solubility", package = "AppliedPredictiveModeling")

dim(solTrainX)

names(solTrainX)

head(solTestY)

str(solTrainX)


fingerprints <- grep("FP", names(solTrainXtrans))
length(fingerprints)

library(caret)

featurePlot(solTrainXtrans[, -fingerprints], solTrainY,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))



# ------------------------------------------------------------------------------
# Target variable vs. continuous variable (covariate) with smooth curve by xyplot
# ------------------------------------------------------------------------------
HVS <- read.table(file = "//media//kswada//MyFiles//references//ZuurBeginnersGuideToGeneralizedAdditiveModelsWithR//HVS.txt", header = TRUE, dec=".")

str(HVS)

# The variable names are long, we will rename some of them
# We also define categorical variables as factors, to avoid mistakes.
HVS$OrbitalV    <- HVS$MeanOrbitalVolume
HVS$fPopulation <- factor(HVS$Population)
HVS$LatAbs      <- HVS$AbsoluteLatitude
HVS$CC          <- HVS$CranialCapacity
HVS$FM          <- HVS$FMarea_intercondyle
HVS$Illuminance <- HVS$Minimum_Illuminance
HVS$Temperature <- HVS$Minimum_Temperature_celsius
HVS$fGender     <- factor(HVS$Gender)


# There is one missing value
colSums(is.na(HVS))


# we remove it
# The function na.exvlude removes all rows that contain a missing value.
HVS2 <- na.exclude(HVS)

Myxyplot <- function(Z, MyV, NameY1, MyXlab = "", MyYlab="") {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  
  
  library(mgcv)
  library(lattice)
  
  P <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(MyXlab, cex = 1.5),
              #ylab = list("Response variable", cex = 1.5),
              #ylab = list("Pearson residuals", cex = 1.5),
              ylab = list(MyYlab, cex = 1.5),
              #layout = c(2,2),   #Modify
              strip = function(bg='white', ...)
                strip.default(bg='white', ...),
              scales = list(alternating = T,
                            x = list(relation = "free"),
                            y = list(relation = "same")),
              panel=function(x, y){
                panel.grid(h=-1, v= 2)
                panel.points(x, y, col = 1)
                panel.loess(x, y, span = 0.8,col = 1, lwd = 2)
              })
  
  print(P)
}

MyVar2 <- c("LatAbs", "CC", "FM", "Illuminance", "Temperature")

Myxyplot(HVS2, MyVar2, "OrbitalV")



# ---------------------------------------------------------------------------
# Target variable vs. continuous variable (covariate) with smooth curve by ggplot
# ---------------------------------------------------------------------------
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + geom_smooth() + facet_wrap(~ class, nrow=2)

