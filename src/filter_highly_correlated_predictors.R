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


# Good reason to avoid data with highly correlated predictors:
#   - redundant predictors frequently add more complexity to the model than information they provide to the model
#   - Using highly correlated predictors in techniques like linear regression can result in highly unstable models,
#     numerical errors, and degraded predictive performance.


# ---------------------------------------------------------------------------
# Filter highly correlated predictors
#
#  - Remove the minimum number of predictors to ensure that all pairwise correlations are below a certain threshold.
#    While this method only identify collinearities in two dimensions, it can have a significantly positive effect on the performance of some models.
#
#  - The algorithm is as follows:
#      - Calculate the correlation matrix of the predictors
#      - Determine the two predictors associated with the largest absolute pairwise correlation (call them predictors A and B)
#      - Determine the average correlation between A and the other variables.  Do the same for predictor B.
#      - If A has a larger average correlation, remove it;  otherwise, remove predictor B.
#      - Repeat above steps until no absolute correlations are avove the threshold.
#
#  - When we apply a threshold of 0.75, this means that we want to eliminate the minimum number of the predictors to achieve
#    all pairwise correlations less than 0.75
# ---------------------------------------------------------------------------
highCorr <- findCorrelation(cor(data2), cutoff = 0.75)

length(highCorr)



# ----------
only_highCorr <- data2[, highCorr]
exclude_highCorr <- data2[, -highCorr]


library(corrplot)

corrplot(cor(only_highCorr), order = "hclust")

corrplot(cor(exclude_highCorr), order = "hclust")



# ---------------------------------------------------------------------------
# Filter highly correlated predictors:  VIF (variance inflation factor)
#  - 
# ---------------------------------------------------------------------------
# When predictors should be removed, a vector of integers is returned that indicates which columns should be removed.

