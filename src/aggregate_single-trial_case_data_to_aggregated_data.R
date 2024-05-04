library(dplyr)
library(rethinking)


# ---------------------------------------------------------------------------
# aggregate single-trial case data to aggregated data
# ---------------------------------------------------------------------------
data("chimpanzees", package = "rethinking")


# This is the data of single-trial case
d <- chimpanzees

car::some(d)


# ----------
# Aggregate "pulled_left" by "prosoc_left", "condition", and "actor" 
d.aggregated <- aggregate(d$pulled_left, list(prosoc_left = d$prosoc_left, condition = d$condition, actor = d$actor), sum)

head(d.aggregated)