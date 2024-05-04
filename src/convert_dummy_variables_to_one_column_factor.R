library(caret)
library(dplyr)


data("cars") 

carSubset <- subset(cars) 

head(carSubset) 



# ---------------------------------------------------------------------------
# Convert dummy variables to one column factor
# ---------------------------------------------------------------------------
type <- c("convertible","coupe", "hatchback", "sedan", "wagon") 

carSubset$Type <- factor(apply(carSubset[, 14:18], 1, function(x) type[which(x == 1)]))


head(carSubset)

