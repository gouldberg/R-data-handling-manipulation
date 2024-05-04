packages <- c("dplyr", "GLMsData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ---------------------------------------------------------------------------
# data: lungcap
# ---------------------------------------------------------------------------
data(lungcap)

str(lungcap)

summary(lungcap)



# ----------
scatter.smooth(lungcap$Ht, lungcap$FEV, las = 1, col = "grey", ylim = c(0, 6), xlim = c(45, 75), main = "FEV", xlab = "Height (in inches)", ylab = "FEV (in L)")

scatter.smooth(lungcap$Ht, log(lungcap$FEV), las = 1, col = "grey", ylim = c(-0.5, 2), xlim = c(45, 75), main = "FEV", xlab = "Height (in inches)", ylab = "log of FEV (in L)")



# ---------------------------------------------------------------------------
# Coefficient Estimates
# ---------------------------------------------------------------------------

lungcap$Smoke <- factor(lungcap$Smoke, levels = c(0,1), labels = c("Non-smoker", "Smoker"))

Xmat <- model.matrix(~ Age + Ht + factor(Gender) + factor(Smoke), data = lungcap)

head(Xmat)


y <- log(lungcap$FEV)

( beta <- solve(t(Xmat) %*% Xmat) %*% (t(Xmat) %*% y) )

drop(beta)  # drops any unnecessary dimensions, in this case it reduces a singl-column matrix to a vector



# ----------
# slightly more efficient code

XtX <- t(Xmat) %*% Xmat

XtY <- t(Xmat) %*% y

( beta <- solve(XtX, XtY) )



# ----------
# Even more efficient approach would have been to use the QR-decomposition

( QR <- qr(Xmat) )

beta <- qr.coef(QR, y)

beta



# ---------------------------------------------------------------------------
# Estimating the variance simga^2
# ---------------------------------------------------------------------------

mu <- Xmat %*% beta

RSS <- sum((y - mu)^2)

s2 <- RSS / (length(y) - length(beta))

c(s = sqrt(s2), s2 = s2)



# ---------------------------------------------------------------------------
# Estimating the variance of beta-hat
# ---------------------------------------------------------------------------

( var.matrix <- s2 * solve(XtX) )

var.betaj <- diag(var.matrix)

sqrt(var.betaj)



# ---------------------------------------------------------------------------
# Estimating the variance of fitted values
# ---------------------------------------------------------------------------

xg.vec <- matrix(c(1, 18, 66, 0, 1), nrow = 1)

mu.g <- xg.vec %*% beta

var.mu.g <- sqrt(xg.vec %*% (solve(t(Xmat) %*% Xmat)) %*% t(xg.vec) * s2)

c(mu.g, var.mu.g)

drop(sqrt(var.mu.g))









