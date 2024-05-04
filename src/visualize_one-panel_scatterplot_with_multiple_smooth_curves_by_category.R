library(dplyr)
library(ggplot2)



# ---------------------------------------------------------------------------
# Visualize scatter plot with smooth curve by category in one panel
# ---------------------------------------------------------------------------
mpg


# ----------
# scatter plot with smooth curve by category (se=FALSE):  colour by category
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(colour = class)) + 
  geom_smooth(mapping = aes(colour = class), se=FALSE)



# ----------
# scatter plot with smooth curve by category (se=FALSE):  linetype by category
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(colour = class)) + 
  geom_smooth(mapping = aes(linetype = class), se=FALSE)



# ----------
# xyplot()
# type = "smooth"
xyplot(hwy ~ displ, data = mpg, type = c("g", "p", "smooth"), groups = class, pch = 20)




# ---------------------------------------------------------------------------
# Visualize scatter plot with smooth curve for filtered category
# ---------------------------------------------------------------------------
# smooth curve is shown for all and only for filtered category
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(colour = class)) +
  geom_smooth(
    data = filter(mpg, class == "subcompact"),
    se=FALSE
  ) +
  geom_smooth(colour="black")
