library(dplyr)

data("GermanCredit", package = "caret")

data <- GermanCredit

dim(data)

str(data)



# ---------------------------------------------------------------------------
# select columns of specified type
# ---------------------------------------------------------------------------
# select only columns with integer type by lapply + unlist
col_int <- unlist(lapply(data[,names(data)!="Class"], is.integer)) 

data[,col_int]



# ----------
# select only columns with integer type by purrr::map_lgl
data[ , purrr::map_lgl(data, is.integer)]



# ----------
dplyr::select_if(data, is.integer)



# ----------
# select ordered factor variables
data(ticdata)

isOrdered <- unlist(lapply(ticdata, function(x) any(class(x) == "ordered")))
isOrdered
