library(dplyr)



# ---------------------------------------------------------------------------
# Writing pipeable functions
#
# There are two main types of pipeable functions: transformation and side-effect
#  - In transformation functions, there's a clear "primary" object that is passed ina as the first argument, 
#    and a modifed version is returned by the function
#  - Side-effect functions are primarily called to perform an action, like drawing a plot or saving a file, not transforming an object.
#    These functions should "invisible" return the first argument, so they're not printed by default, but can still be used in a pipeline
# ---------------------------------------------------------------------------
show_missings <- function(df){
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
}


# This function "invisible" return the first argument
x <- show_missings(mtcars)
x



# ----------
# This function can be used in a pipeline !!!
mtcars %>% show_missings() %>% mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% show_missings()


