setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Kernel
# ------------------------------------------------------------------------------
# Gaussian Kernel or Radian Basis Function (RBF)
kernel_func <- function(theta1, theta2, x1, x2){
  return(theta1 * exp(- sum((x1 - x2)^2) / theta2))
}

# parameter for kernel_func
theta1 <- 1
theta2 <- 1



# ----------
# lnear kernel
kernel_func <- function(theta1, theta2, x1, x2){
  return(t(x1) * x2)
}

theta1 <- 1
theta2 <- 1



# ----------
# exponential kernel
kernel_func <- function(theta1, theta2, x1, x2){
  return(exp(- sum(abs(x1 - x2)) / theta2))
}

theta1 <- 1
theta2 <- 1



# ----------
# periodict kernel
kernel_func <- function(theta1, theta2, x1, x2){
  return(exp(theta1 * cos(abs(x1 - x2) / theta2)))
}

theta1 <- 1
theta2 <- 1



# ------------------------------------------------------------------------------
# Simulate Gaussian Process
# ------------------------------------------------------------------------------
x_1 <- seq(-5, 5, by = 1)
x_2 <- seq(-5, 5, by = 0.2)
x_3 <- seq(-5, 5, by = 0.1)


x_list <- list(x_1 = x_1, x_2 = x_2, x_3 = x_3)


# ----------
par(mfrow=c(length(x_list),2))

for(i in 1:length(x_list)){

  x <- x_list[[i]]
  # K_mat:  covariance matrix
  K_mat <- matrix(rep(0, length(x) * length(x)), nrow = length(x), ncol = length(x))
  for(i in 1:length(x)){
    for(j in 1:length(x)){
      K_mat[i,j] <- kernel_func(theta1=theta1, theta2=theta2, x1 = x[i], x2 = x[j])
    }
  }
  
  K_mat
  
  
  
  # ----------
  n <- 10
  ( y <- mvrnorm(n = n, mu = rep(0, length(x)), Sigma = K_mat) )
  
  
  
  # ----------
  color_vec <- c("black", "red", "blue", "gray", "navy", "orange", "green", "black", "red", "blue")
  for(i in 1:n){
    if(i == 1){ plot(x = x, y = y[i,], type = "l", lty = i, col = color_vec[i], xlab = "X", ylab = "Y", ylim = c(min(y), max(y))) }
    else { par(new=T); plot(x = x, y = y[i,], type = "l", lty = i, col = color_vec[i], xlab = "", ylab = "", xaxt = "n", yaxt = "n") }
  }
  
  range_y <- max(y) - min(y)
  corrplot::corrplot(K_mat/range_y, method = "shade", title = "")
}


