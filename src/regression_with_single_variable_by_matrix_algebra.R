packages <- c("dplyr", "GLMsData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ---------------------------------------------------------------------------
# data: gestation
#   - The mean birthweight y (in kg) and gestational ages x (in weeks) of 1513 infants boarn to Caucasian mothers at St George's hospital,
#     London, between August 1982 and March 1984 were recorded from volunteers.
# ---------------------------------------------------------------------------
data(gestation)

str(gestation)

summary(gestation)



# ----------
plot(Weight ~ Age, data = gestation, las = 1, pch = ifelse(Births < 20, 1, 19))



# ---------------------------------------------------------------------------
# Least-Squares Estimation:  Coefficient Estimation
#   - mu = beta0 + beta1 * xi
#   - var[y] = sigma^2 / wi
# ---------------------------------------------------------------------------

y <- gestation$Weight
x <- gestation$Age
wts <- gestation$Births


xbar <- weighted.mean(x, w = wts)
SSx <- sum(wts * (x - xbar)^2)
ybar <- weighted.mean(y, w = wts)
SSxy <- sum(wts * (x - xbar) * y)


beta1 <- SSxy / SSx
beta0 <- ybar - beta1 * xbar
mu <- beta0 + beta1 * x
RSS <- sum(wts * (y - mu)^2)


c(beta0 = beta0, beta1 = beta1, RSS = RSS)



# ----------
mod_lm <- lm(Weight ~ Age, weights = Births, data = gestation)

coef(mod_lm)



# ---------------------------------------------------------------------------
# Least-Squares Estimation:  Estimating the variance sigma^2
# ---------------------------------------------------------------------------

df <- length(y) - 2

s2 <- RSS / df

c(df = df, s = sqrt(s2), s2 = s2)

mod_lm$residuals



# ---------------------------------------------------------------------------
# Least-Squares Estimation:  Standard Errors of the Coefficients
# ---------------------------------------------------------------------------

var.b0 <- s2 * ( 1 / sum(wts) + xbar ^2 / SSx )

var.b1 <- s2 / SSx

sqrt(c(beta0 = var.b0, beta1 = var.b1))

summary(mod_lm)



# ---------------------------------------------------------------------------
# Least-Squares Estimation:  Standard Errors of Fitted Values
# ---------------------------------------------------------------------------

x.g <- 30

mu.g <- beta0 + x.g * beta1

var.mu.g <- s2 * (1 / sum(wts) + (x.g - xbar)^2 / SSx)

se.mu.g <- sqrt(var.mu.g)

c(mu = mu.g, se = sqrt(var.mu.g))
















