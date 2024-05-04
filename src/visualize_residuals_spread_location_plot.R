setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Residual spread-location plot
#   - Check the unusual pattern in residual variation
# ------------------------------------------------------------------------------

stripplot(sqrt(abs(residuals(lm(yield ~ variety + year + site)))) ~ site,
          data = barley, groups = year,
          jitter.data = TRUE,
          auto.key = list(points = TRUE, lines = TRUE, columns = 2),
          type = c("p", "a"), fun = median, ylab = expression(abs("Residual Barley Yield") ^ {1/2}))
