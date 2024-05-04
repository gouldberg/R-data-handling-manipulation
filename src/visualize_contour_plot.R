setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Ridge and Lasso regression
#  --> The way the least squares coefficients are shurunk towards zero for different value of p
# ------------------------------------------------------------------------------

p <- 2

beta1 <- beta2 <- seq(-1, 1, len = 100)

( r <- (outer(abs(beta1)^p, abs(beta2)^p, "+")) )

image(beta1, beta2, r, main = paste("p=", p, sep = ""), ylab = expression(beta[2]), xlab = expression(beta[1]))

contour(beta1, beta2, r, add = T)



# ----------
normPlot <- function(p = 2){
  beta1 <- beta2 <- seq(-1, 1, len = 100)
  
  r <- (outer(abs(beta1)^p, abs(beta2)^p, "+"))
  
  image(beta1, beta2, r, main = paste("p=", p, sep = ""), ylab = expression(beta[2]), xlab = expression(beta[1]))
  
  contour(beta1, beta2, r, add = T)
}



p <- c(10, 5, 2, 1.5, 1.2, 1, 0.8, 0.5, 0.001)

sapply(seq_along(p), function(x) normPlot(p[x]))