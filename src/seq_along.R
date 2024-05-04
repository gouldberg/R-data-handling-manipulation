library(tidyverse)
library(dplyr)



# ---------------------------------------------------------------------------
# sequence i in seq_along(df)
#  - This determines what to loop over: each run of the for loop will assign i to a different value from seq_along(df).
#  - It is a safe version of the familiar 1:length(l), with an important difference; if you have zero-length vector, seq_along() does the right thing
# ---------------------------------------------------------------------------
# Normal case
df <- mtcars

output <- vector("double", ncol(df))
output

seq_along(df)

for(i in seq_along(df)){
  output[[i]] <- median(df[[i]])
}

output



# ----------
# df has no columns
df <- data.frame()

output <- vector("double", ncol(df))
output

seq_along(df)

for(i in seq_along(df)){
  output[[i]] <- median(df[[i]])
}

output



# ----------
# df has no columns and use "i in 1:ncol(df)"
df <- data.frame()

output <- vector("double", ncol(df))
output

for(i in 1:ncol(df)){
  output[[i]] <- median(df[[i]])
}

output
