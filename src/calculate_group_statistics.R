library(dplyr)
library(rethinking)



# ---------------------------------------------------------------------------
# calculate group statistics
# ---------------------------------------------------------------------------
data("chimpanzees", package = "rethinking")


# This is the data of single-trial case
d <- chimpanzees

car::some(d)


# ----------
# The by() command can split up data and automatically apply functions such as mean() and summary()
p <- by(d$pulled_left, list(d$prosoc_left, d$condition, d$actor), mean)

str(p)


# show only actor == 1
as.vector(p[,,1])




# ---------------------------------------------------------------------------
# calculate group statistics
# ---------------------------------------------------------------------------

seg.df <- read.csv("//media//kswada//MyFiles//R//consumer_segment//rintro-chapter5.csv", header = TRUE)



# ----------
by(seg.df$income, seg.df$Segment, mean)


# aggregate() understands formula models and produces a reusable, indexable object with its results
aggregate(seg.df$income, list(seg.df$Segment), mean)


boxplot(income ~ Segment, data = seg.df, yaxt = "n", ylab = "Income ($k)")
ax.seq <- seq(from = 0, to = 120000, by = 20000)
axis(side = 2, at = ax.seq, labels = paste(ax.seq/1000, "k", sep = ""), las = 1)



# ----------
# list() is used to give multiple indices
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)


# aggregate() understands formula model
aggregate(income ~ Segment + ownHome, data = seg.df, mean)


aggregate(income ~ Segment + ownHome + subscribe, data = seg.df, mean)



# ---------------------------------------------------------------------------
# report mean by group
# ---------------------------------------------------------------------------

seg.summ <- function(data, groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


seg.summ(seg.df, seg.df$Segment)

