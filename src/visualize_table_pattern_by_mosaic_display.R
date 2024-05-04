setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

data <- HairEyeColor

data


# collapse table to 2-way table (Hair(1) and Eye(2))
( haireye <- margin.table(data, 1:2) )



# ------------------------------------------------------------------------------
# Two-way tables
# ------------------------------------------------------------------------------
vcd::mosaic(haireye, shade=TRUE, suppress=0, labeling=labeling_residuals, gp_text=gpar(fontface=2))

vcd::mosaic(haireye, shade = TRUE, labeling = labeling_residuals)



# ------------------------------------------------------------------------------
# Two-way tables
# Marimekko chart (or a "poor-man's" mosaic display)
# ------------------------------------------------------------------------------
# This is essentially a divided bar chart where the eye colors within each horizontal bar for the hair color group are all given the same color
# color by hair color
fill_colors <- c("brown4", "#acba72", "green", "lightblue")
( fill_colors_mat <- t(matrix(rep(fill_colors, 4), ncol = 4)) )

vcd::mosaic(haireye2, gp = gpar(fill = fill_colors_mat, col = 0))



# ----------
# Alternatively, we could have used function shading_Marimekko()
vcd::mosaic(haireye2, gp = shading_Marimekko(haireye2))



# -->
# Note that because the hair colors and eye colors are both ordered, this shows the decreasing prevalence of light hair color amongst those with brown eyaes
# and the increasing prevalence of light hair with blue eyes.



# ------------------------------------------------------------------------------
# Two-way tables:  Toeplitz-based colors
# toeplitz designs: use color to highlight the pattern of diagonal cells, and the off-diagonals
# ------------------------------------------------------------------------------
# colorspace generates better colors than palette()
library(colorspace)


# ----------
# toeplitz() returns such a patterned matrix
toeplitz(1 : 4)


( fill_colors <- colorspace::rainbow_hcl(8)[1 + toeplitz(1 : 4)] )



# ----------
# toeplitz-based colors reflecting the diagonal strips in a square table.
vcd::mosaic(haireye2, gp = gpar(fill = fill_colors, col = 0))



# ----------
# Alternatively, we could have used function shading_diagonal()
vcd::mosaic(haireye2, gp = shading_diagonal(haireye2))



# ------------------------------------------------------------------------------
# Two-way tables
# More simply, to shade a mosaic according to the levels of one variable (typically a response variable), 
# you can use the highlighting arguments
# ------------------------------------------------------------------------------
vcd::mosaic(haireye2, highlighting = "Eye", highlighting_fill = fill_colors)

vcd::mosaic(Eye ~ Hair, data = haireye2, highlighting_fill = fill_colors)



# ------------------------------------------------------------------------------
# Two-way tables
# Residual-based shading:  Interpolation options (more shading levels)
# ------------------------------------------------------------------------------
# interpolation option:  more shading levels
# a numeric vector is passed as interpolate = 1:4, defining the boundaries of a step function mapping the absolute values of residuals
# to saturation levels in the HCL color scheme.
vcd::mosaic(haireye2, shade = TRUE, gp_args = list(interpolate = 1 : 4))



# continuous shading
# a user-defined function, interp(), is created which maps the absolute residuals to saturation values
# in a continuous way (up to a maximum of 6)
interp <- function(x) pmin(x / 6, 1)

vcd::mosaic(haireye2, shade = TRUE, gp_args = list(interpolate = interp))



# -->
# In practice, the default discrete interpolation, using cutoffs of +-2, +-4 usually works quite well.



# ------------------------------------------------------------------------------
# Two-way tables
# Shading functions:  shading_Friendly
# ------------------------------------------------------------------------------
# classical Friendly shading scheme:
# with HSV colors of blue and red and default cutoffs for absolute residuals, +-2, +-4, corresponding to interpolate = c(2,4)
# the border line type (lty) distinguishes positive and negative residuals, which is useful if a mosaic plot is printed in black and white.
vcd::mosaic(haireye2, gp = shading_Friendly, legend = legend_fixed)

vcd::mosaic(haireye2, gp = shading_Friendly2, legend = legend_fixed)


# ----------
# Shading functions shading_max
# Instead of using the cutoffs 2 and 4, it employs the critical values M(alpha) for the maximum absolute Pearson residual statistic, 
# by default at alpha = 0.10 and 0.01.
# Only those residuals with |r(ij)| > M(alpha) are colored in the plot, using 2 levels for Value ("lightness") in HSV color space.
# This function uses a permutation-based test to determine significance of residuals
set.seed(1234)

vcd::mosaic(haireye2, gp = shading_max)




# ------------------------------------------------------------------------------
# 3-way and larger tables
# Mosaic plot
# ------------------------------------------------------------------------------
HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"),]
vcd::mosaic(HEC, rot_labels = c(right = -45))


# --> There is no systematic association between sex and the combinations of hair and eye color.
# The proportion of male-female students is roughly the same in almost all hair/eye color groups.
# Yet, among blue-eyed blonds, there seems to be an overabundance of females, and the proportion of blue-eyed males with brown in hair also looks suspicious.



# ------------------------------------------------------------------------------
# mosaic plot:  highlighitng
# ------------------------------------------------------------------------------
vcd::mosaic(margin.table(Titanic, c(1,2)), highlighting = "Sex")
vcd::mosaic(margin.table(Titanic, c(1,2)), highlighting = "Class")



# ------------------------------------------------------------------------------
# mosaic plot:  customize labelling
# ------------------------------------------------------------------------------
# default
mosaic(Titanic)


# Note that the last two levels of the survived variable do overlap, as well as some adult and child labels of the age Variable.
# This issue can be addressed in several ways. 



# ----------
# clipping:  The “brute force” method is to enable clipping for these dimensions
mosaic(Titanic, labeling_args = list(clip = c(Survived = TRUE, Age = TRUE)))



# ----------
# abbreviate the levels
# The abbreviate argument takes a vector of integers indicating the number of significant characters
# the levels should be abbreviated to (TRUE is interpreted as 1, obviously)
mosaic(Titanic, labeling_args = list(abbreviate_labs = c(Survived = TRUE, Age = 3)))



# ----------
# rotate the levels
mosaic(Titanic, 
       labeling_args = list(rot_labels = c(bottom = 90, right = 0), 
                            offset_varnames = c(right = 1), offset_labels = c(right = 0.3)),
       margins = c(right = 4, bottom = 3))



# ----------
# inhibit the output of repeated levels
mosaic(Titanic, labeling_args = list(rep = c(Survived = FALSE, Age = FALSE)))



# ----------
# position all labels and variables left-aligned
mosaic(Titanic, labeling_args = list(pos_varnames = "left", pos_labels = "left", just_labels = "left", rep = FALSE))
mosaic(Titanic, labeling = labeling_left)


