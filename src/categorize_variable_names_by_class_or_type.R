packages <- c("dplyr", "Hmisc", "rms")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ---------------------------------------------------------------------------
# data
# ---------------------------------------------------------------------------
getHdata(prostate)

str(prostate)

car::some(prostate)



# ------------------------------------------------------------------------------
# Categorize variable names by class or type
# ------------------------------------------------------------------------------

var_fac <- sapply(1:ncol(data), function(x) is.factor(data[,x]))
( var_fac <- colnames(data)[var_fac] )


var_char <- sapply(1:ncol(data), function(x) is.character(data[,x]))
( var_char <- colnames(data)[var_char] )


var_num <- sapply(1:ncol(data), function(x) is.numeric(data[,x]))
( var_num <- colnames(data)[var_num] )


var_int <- sapply(1:ncol(data), function(x) is.integer(data[,x]))
( var_int <- colnames(data)[var_int] )


var_bin <- c("stage", "bm", "hx", "ekg.norm")


var_cat <- c("sg", "dbp", "sbp", "rx", "pf", "status", "ekg")


