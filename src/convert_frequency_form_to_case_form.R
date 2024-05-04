library(dplyr)


data("HairEyeColor", package = "datasets")

HairEyeColor


# Calculate expected frequencies
haireye <- margin.table(HairEyeColor, 1:2)

haireye


# frequency form data frame
( dat_freq <- as.data.frame(haireye) )



# ---------------------------------------------------------------------------
# Convert frequency form to case form by expand.dft()
# ---------------------------------------------------------------------------
dat_case <- expand.dft(dat_freq)

dat_case