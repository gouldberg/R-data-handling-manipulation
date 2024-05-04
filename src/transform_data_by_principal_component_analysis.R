library(dplyr)
library(AppliedPredictiveModeling)
library(caret)


data(segmentationOriginal)
data <- segmentationOriginal

car::some(data)


# ----------
data2 <- subset(data, Case == "Train")

cellID <- data2$Cell
class <- data2$Class
case <- data2$Case

data2 <- data2[, -c(1:3)]

statusColNum <- grep("Status", names(data2))
statusColNum

data2 <- data2[, -statusColNum]



# ---------------------------------------------------------------------------
# The base R fucntion prcomp
# ---------------------------------------------------------------------------
pcaObject <- prcomp(data2, center = TRUE, scale. = TRUE)

pcaObject



# ----------
# variance percent
round(pcaObject$sd^2 / sum(pcaObject$sd^2) * 100, digits = 2)



# ----------
# transformed values
head(pcaObject$x[, 1:5])



# ----------
# variable loading, where rows correspondend to predictor variables and columns are associated with the components
head(pcaObject$rotation[, 1:3])
