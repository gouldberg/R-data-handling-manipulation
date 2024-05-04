library(dplyr)
library(ggplot2)
library(lattice)



# ---------------------------------------------------------------------------
# Visualize boxplot by category
# ---------------------------------------------------------------------------
mpg


# boxplot for "hwy" by "class"
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot()



# coordinate flip
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot() + coord_flip()



# boxplot by lattice bwplot()
bwplot(hwy ~ factor(class), data = mpg)



# boxplot by lattice bwplot() with coordinate flip
bwplot(factor(class) ~ hwy, data = mpg)



# ----------
# boxplot by lattice bwplot() + varwidth = TRUE (varwidth is adjusted to proportional to sample sizes)
bwplot(hwy ~ factor(class), data = mpg, varwidth = TRUE)
