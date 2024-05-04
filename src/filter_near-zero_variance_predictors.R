library(dplyr)
library(AppliedPredictiveModeling)
library(caret)



# ---------------------------------------------------------------------------
# Filter near-zero variance predictors by nearZeroVar()
#
#  - a rule of thumb for detecting near-zero variance predictors is:
#      - The fraction of unique values over the sample size is low (say 10%)
#      - The ratio of the frequency of the most prevalent value to the frequency of the second most prevalentvalue is large (say around 20)
# ---------------------------------------------------------------------------
data(segmentationOriginal)

data <- segmentationOriginal

car::some(data)



# ----------
# When predictors should be removed, a vector of integers is returned that indicates which columns should be removed.
idx <- nearZeroVar(data)

names(data)[idx]

for(i in seq_along(idx)) print(table(data[,idx[i]]))


