setwd("//media//kswada//MyFiles//R//R_basic")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Mosaic matrices
#   - The vcd packages extends pairs() generic function to mosaic matrices with methods for "table" and "structable" objects
# ------------------------------------------------------------------------------
data("PreSex", package = "vcd")

data <- PreSex

data


# The mosaic matrix show all 2-way marginal relations
# The diagonal panels show the labels for the category levels as well as the one-way marginal totals.
pairs(data, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))




# ------------------------------------------------------------------------------
# Mosaic matrices
#   - The vcd packages extends pairs() generic function to mosaic matrices with methods for "table" and "structable" objects
# ------------------------------------------------------------------------------
data("UCBAdmissions", package = "datasets")

data <- UCBAdmissions

data



# The mosaic matrix show all 2-way marginal relations
# Some additional arguments are used to control the details of labels for the diagonal and off-diagonal panels.

largs <- list(labeling = labeling_border(varnames = FALSE, labels = c(T, T, F, T), alternate_labels = FALSE))
dargs <- list(gp_varnames = gpar(fontsize = 20), offset_varnames = -1, labeling = labeling_border(alternate_labels = FALSE))

pairs(data, shade = TRUE, space = 0.25, diag_panel_args = dargs, upper_panel_args = largs, lower_panel_args = largs)



# --> 
# The panel (1,2) shows that Admission and Gender are strongly associated marginally.

# The panel (1,3) shows that Department A and B have the greatest overall admission rate, departments E and F the least.

# The panel (2,3) shows that men and women apply differentially to the various departments

# It can be seen that men apply in much greater numbers to departments A and B, with higher admission rates,
# while women apply in greater numbers to the departments C-F, with the lowest overall rate of admission



# ------------------------------------------------------------------------------
# Generalized mosaic matrices
#  - type argument can be used to plot mosaics showing various kinds of independence relations
#      - "pairwise":  bivariate marginal relations, collapsed over all other variables
#      - "total":  mosaic plots for mututal independence
#      - "conditional":  mosaic plots for conditional independence given all other variables
#      - "joint":  mosaic plots for joint independence of all pairs of variable from the others
# ------------------------------------------------------------------------------
data("UCBAdmissions", package = "datasets")

data <- UCBAdmissions

data


# The observed frequencies are the same in all these cells
# However, in the lower panels, the tiles are shaded according to models of joint independence, 
# while in the upper panels, they are shaded according to models of mutual independence.

pairs(data, space = 0.2, lower_panel = pairs_mosaic(type = "joint"), upper_panel = pairs_mosaic(type = "total"))



# ----------
# For this data, more useful to fit and display the models of conditional independence for each pair of row, column variables given the remaining one
pairs(data, space = 0.2, type = "conditional")


# -->
# The shading in the (1,2) and (2,1) panels shows the fit of the model [Admin, Dept] [Gender, Dept],
# which asserts that Admission and Gender are independent, given (controlling for) Department.

# Except for Department A, this model fits quite well, again indicating lack of gender bias

# The panel (1,3) and (3,1) shows the relation between admission and department controlling for gender,
# highlighting the differential admission rates across departments.

