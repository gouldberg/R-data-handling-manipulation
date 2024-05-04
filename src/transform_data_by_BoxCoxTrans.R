library(dplyr)
library(AppliedPredictiveModeling)
library(caret)
library(e1071)


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
# Determine type of data transformation by caret::BoxCoxTrans
#
#  - Box and Cox (1964) propose a family of transformations that are indexed by a parameter, denoted as lambda
#     - Box-Cox transformed data = ( x^lambda - 1 ) / lambda  (if lambda != 0),   log(x)  (if lambda = 0) 
#
#  - In addition to the log transformation, this family can identify sqaure transformation (lambda = 2), square root (lambda = 0.5), inverse (lambda = -1), 
#    and others in-between.
#  - Box and Cox (1964) show how to use maximum likelihood estimation to determine the transformation parameter. This procedure would be applied independently
#    to each predictor data that contain values greater than zero.
# ---------------------------------------------------------------------------
# check skewness by e1071::skewness
# A general rule of thumb to consider is that skewed data whose ratio of the highest value to the lowest value is greater than 20 have significant skewness
# Since all the predictors are numeric columns, the apply function can be used to compute the skewness across columns
apply(data2, MARGIN = 2, FUN = skewness) %>% head()


hist(data2$AreaCh1, breaks=seq(0, 3000, 100))



# ----------
# Estimate lambda for Box-Cox transformation
Ch1AreaTrans <- caret::BoxCoxTrans(data2$AreaCh1)

Ch1AreaTrans



# ----------
# Transform data by estimated lambda
head( ( data2$AreaCh1 ^ (Ch1AreaTrans$lambda) - 1 ) / Ch1AreaTrans$lambda )


# Transform by predict(lambda, )
( tmp <- predict(Ch1AreaTrans, data2$AreaCh1) )



# ----------
hist(tmp, breaks=seq(1.095, 1.115, 0.001))


