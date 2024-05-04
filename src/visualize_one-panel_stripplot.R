library(dplyr)
library(ggplot2)
library(lattice)



# ---------------------------------------------------------------------------
# Visualize One-panle stripplot
# ---------------------------------------------------------------------------

xyplot(depth ~ factor(mag), data = quakes, 
       jitter.data = TRUE, alpha = 0.6, xlab = "Magnitude (Richter)", ylab = "Depath (km)")



# For stripplot, the one of axis is assumed to be categorical
stripplot(depth ~ factor(mag), data = quakes, 
          jitter.data = TRUE, alpha = 0.6, xlab = "Magnitude (Richter)", ylab = "Depath (km)")


