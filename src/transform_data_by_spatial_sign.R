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
# Transform data by spatial sign
#
#  - 
# ---------------------------------------------------------------------------

