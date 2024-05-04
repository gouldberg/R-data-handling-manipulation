library(dplyr)



# ---------------------------------------------------------------------------
# data:  consumer segment data
# ---------------------------------------------------------------------------

seg.df <- read.csv("//media//kswada//MyFiles//R//consumer_segment//rintro-chapter5.csv", header = TRUE)



# ---------------------------------------------------------------------------
# Testing group frequencies
# ---------------------------------------------------------------------------
( tab <- table(seg.df$subscribe, seg.df$ownHome) )



# ----------
# chisq.test():  for 2 * 2 tables, chisq.test() defaults to using Yates' correction, which adjusts the chi-square statistic in light of the fact that
# the assumption of continuous data is imperfect when data comes from a lumpy binomial distribution.
chisq.test(tab)



# ----------
# chisq.test() can calculate confidence intervals using a simulation method, where it compares the observed table to thousands of simulated tables
# with the same marginal counts
chisq.test(tab, sim = TRUE, B = 10000)



# ---------------------------------------------------------------------------
# Testing group means
# ---------------------------------------------------------------------------
by(seg.df$income, seg.df$ownHome, mean)


# aggregate() understands formula models and produces a reusable, indexable object with its results
aggregate(seg.df$income, list(seg.df$ownHome), mean)


boxplot(income ~ ownHome, data = seg.df, yaxt = "n", ylab = "Income ($k)")
ax.seq <- seq(from = 0, to = 120000, by = 20000)
axis(side = 2, at = ax.seq, labels = paste(ax.seq/1000, "k", sep = ""), las = 1)



# ----------
# Testing group means
t.test(income ~ ownHome, data = seg.df)


t.test(income ~ ownHome, data = subset(seg.df, Segment == "Travelers"))


# -->
# The null hypothesis of no difference in income by home ownership is rejected. The data suggests that people who own their homes have higher income.
# But There is not a significant difference in mean income among those Travelers in our data who own homes and who don't.



# ---------------------------------------------------------------------------
# Testing Multiple Group Means: ANOVA
# ---------------------------------------------------------------------------

( seg.aov.own <- aov(income ~ ownHome, data = seg.df) )
( seg.aov.seg <- aov(income ~ Segment, data = seg.df) )
( seg.aov.seg2 <- aov(income ~ Segment + ownHome, data = seg.df) )


anova(seg.aov.own)
anova(seg.aov.seg)
anova(seg.aov.seg2)



# -->
# Segment is a significant predictor but home ownership is not a significant predictor.
# Segment and homeownership are not independent, and the effect is captured sufficiently by segment membership alone.
# Home ownership accounts for little more over and aboe what can be be explained by Segment.



# ----------
anova(aov(income ~ Segment * ownHome, data = seg.df))


# -->
# Could it be that home ownership is related to income in some segments but not in others?
# Again segment is a significant predictor, while home ownership and the interaction of segment with home ownership are not significant.
# In other words, segment membership is again the best predictor on its own.



# ---------------------------------------------------------------------------
# Visualizing Group Confidece Intervals
# ---------------------------------------------------------------------------
library(multcomp)

glht(seg.aov.seg)



# -->
# The default aov() model has an intercept term (correspinding to the Moving up segment) and all other segments are relative to that.
# This may be difficult for decision makers or clients to understand, so we find it preferable to remove the intercept by adding "-1" to the model formula.


# ----------
( seg.aov.seg_rev <- aov(income ~ -1 + Segment, data = seg.df) )


# With the intercept removed, glht() gives us the mean value for each segment.
( g <- glht(seg.aov.seg_rev) )


par(mar = c(6, 10, 2, 2))

plot(g, xlab = "Income", main = "Average Income by Segment (95% CI)")

