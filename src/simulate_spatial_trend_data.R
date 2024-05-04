setwd("//media//kswada//MyFiles//R_basics")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Simulation of the 3 component model (deterministic + correlated + random)
# ------------------------------------------------------------------------------
library(ggplot2)
library(spdep)


# ----------
# parameters
sigma.eta <- 1.0  # Variance of autocorrelated component
lambda <- 0.4     # Autocorrelation coefficient
sigma.eps <- 0.4 # Magnitude of random component


# ----------
# generate neighbours list for grid cells

set.seed(123)
n <- 20

( nlist <- cell2nb(n, n) )



# ----------
# spdep::invlrM(): Computes the matrix used for generating simultaneous autoregressive random variables, 
# for a given value of rho, a neighbours list object or a matrix, a chosen coding scheme style, and optionally a list of general weights corresponding to neighbours.
( e <- rnorm(n ^ 2) )

( eta <- sigma.eta * invIrM(neighbours = nlist, rho = lambda) %*% e )



# ----------
# Random data
( eps <- sigma.eps * rnorm(n ^ 2) )



# ----------
# Create the correlated component plus uncorrelated component
Y.df <- data.frame(eta)  # Start with autocorr. comp.
Y.df$eps <- eps  # Add uncorrelated component



# ----------
# Add deterministic logistic trend
a <- 2
b.eps <- 0.5  # Variance factor for uncorr. component
b.eta <- 0.15 # Variance factor for autocorr. component
c <- 1
f2 <- n / 2
xymax <- n - 0.5
coords.xy <- expand.grid(x = 0.5:xymax, y = xymax:0.5)

x <- coords.xy[,1]
y <- coords.xy[,2]
Y.trend <- a * exp(c * (x - f2)) / (1 + exp(c * (x - f2)))
Y <- Y.trend + b.eps * eps + b.eta* eta


Y.df$trend <- Y.trend
Y.df$Y <- Y
Y.df$x <- x
Y.df$y <- y



# ------------------------------------------------------------------------------
# plotting simulated 3 component model
# ------------------------------------------------------------------------------
Y.plot <- Y.df[(Y.df$y == n / 2 + 0.5),]
Yt <- Y.plot$trend
Yeps <- b.eps * Y.plot$eps
Yeta <- b.eta * Y.plot$eta
Y <- Yt + Yeps + Yeta
x <- Y.plot$x
par(mai = c(1,1,1,1))
plot(x, Y, type = "l", lwd = 2, cex.lab = 1.5, ylim = c(-0.5,2.5), xlab = expression(italic(x)), ylab = expression(italic(Y)), col = "red")
lines(x, Yt, lwd = 2, col = "blue")
lines(x, Yt + Yeta, lwd = 2, col = "dark green")
text(x = 12.5, y = 0.25, "T(x,y)", pos = 4)
lines(c(10,12.5),c(0.25,0.25),lwd = 2, col = "blue")
text(x = 12.5, y = 0.0, expression("T(x,y)"~+~eta*"(x,y)"), pos = 4)
lines(c(10,12.5),c(0,0),lwd = 2, col = "dark green")
text(x = 12.5, y = -0.25, pos = 4, expression("T(x,y)"~+~eta*"(x,y)"~+~epsilon*"(x,y)"))
lines(c(10,12.5),c(-0.25,-0.25),lwd = 2, col = "red")



# ------------------------------------------------------------------------------
# plotting simulated 3 component model by ggplot()
# ------------------------------------------------------------------------------
Y.ggplot <- data.frame(x)
Y.ggplot$Yt <- Y.plot$trend
Y.ggplot$eps <- b.eps * Y.plot$eps
Y.ggplot$eta <- b.eta * Y.plot$eta
Y.ggplot$Yeta <- with(Y.ggplot, Yt + eta)
Y.ggplot$Y <- with(Y.ggplot, Yt + eps + eta)
ggplot(data = Y.ggplot) +
  geom_line(aes(x = x, y = Yt, color = "T(x,y)"), size = 1) +
  geom_line(aes(x = x, y = Yeta, color = "T(x,y)+n(x,y)"), size = 1) +
  geom_line(aes(x = x, y = Y, color = "T(x,y)+n(x,y)+e(x,y)"), size = 1) +
  theme(legend.position = c(0.75, 0.25)) +
  theme(legend.title = element_blank())



# ------------------------------------------------------------------------------
# Perspective plots
# Plot using the function persp
# ------------------------------------------------------------------------------
x <- (1:n) + 1.5
y <- x



# ----------
# Original data
( Y.persp <- matrix(4 * Y.df$Y, nrow = n) )

persp(x, y, Y.persp, theta = 225, phi = 15, scale = FALSE, zlab = "Y")



# ----------
# Trend T(x,y)
Trend <- matrix(4 * Y.df$trend, nrow = n)
persp(x, y, Trend, theta = 225, phi = 15, scale = FALSE, zlab = "T(x,y)")



# ------------------------------------------------------------------------------
# Fit linear trend to logistic model
# ------------------------------------------------------------------------------
model.lin <- lm(Y ~ x + y, data = Y.df)
coef(model.lin)
trend.lin <- predict(model.lin)
Y.lin <- matrix(trend.lin, nrow = n)
Fit <- 4 * Y.lin
persp(x, y, Fit, theta = 225, phi = 15, scale = FALSE)



# ------------------------------------------------------------------------------
# Median polish trend
#   - nonparametric method, where no assumptions are made about the distribution of the error terms
# ------------------------------------------------------------------------------
Y.trend <- matrix(Y.df$Y, nrow = n)
model.mp <- medpolish(Y.trend)
Y.mp <- Y.trend - model.mp$residuals
Fit <- 4 * Y.mp
persp(x, y, Fit, theta = 225, phi = 15, scale = FALSE)


# -->
# The median polish fit more accurately represents the trend surface,
# and because of the logistic shape of this surface, median polish would probably still be more accurate even if higher order powers
# of x and y were included in the regression.

