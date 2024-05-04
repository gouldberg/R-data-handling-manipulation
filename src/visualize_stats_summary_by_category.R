library(dplyr)
library(ggplot2)



# ---------------------------------------------------------------------------
# Visualize stats summary by category
# ---------------------------------------------------------------------------
diamonds


# min, max, and median for "depth" by "cut"
ggplot(data = diamonds) + stat_summary(
  mapping = aes(x = cut, y = depth),
  fun.ymin = min,
  fun.ymax = max,
  fun.y = median
)



# ----------
# customized range for "depth" by "cut"
ggplot(data = diamonds, aes(x = cut, y = depth)) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    fun.y = mean
  )
  

