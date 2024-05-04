library(caret)
library(dplyr)


data("cars") 

carSubset <- subset(cars) 

head(carSubset) 


# into 1 column named Type, with the corresponding column name 
type <- c("convertible","coupe", "hatchback", "sedan", "wagon") 
carSubset$Type <- factor(apply(carSubset[, 14:18], 1, function(x) type[which(x == 1)]))
head(carSubset)

carSubset <- carSubset %>% dplyr::select(-(convertible:wagon))



# ---------------------------------------------------------------------------
# Create dummy variables by dummyVars + predict
# ---------------------------------------------------------------------------
# levelsOnly = TRUE remove the variable name from the column name ..?
simpleMod <- dummyVars(~ Mileage + Type, data = carSubset, levelsOnly = TRUE)
simpleMod


# THe type field is expanded into five variables for 5 factor levels
head(predict(simpleMod, carSubset))



# ----------
# Joint effect of mileage and car type:  interaction
withInteraction <- dummyVars(~ Mileage + Type + Mileage:Type, data = carSubset, levelsOnly = TRUE)
withInteraction


head(predict(withInteraction, carSubset))

