library(dplyr)



library("mvtnorm")

( sigma <- matrix(c(2, 0.8, 0, 0.8, 0.5, 0, 0, 0, 0.1), ncol = 3) )

set.seed(123)
( baguette <- rmvnorm(1000, mean = c(0, 0, 0), sigma = sigma) )



# ---------------------------------------------------------------------------
# singular value decomposition
# ---------------------------------------------------------------------------
( svdb <- svd(baguette) )

names(svdb)



# left singular vectors
svdb$u

( U <- svdb$u )


# singular values
svdb$d

( D <- diag(svdb$d) )



# right singular vectors
svdb$v

( V <- svdb$v )



# ----------
X <- U %*% D %*% t(V)

head(X)

head(baguette)

identical(round(X, 7), round(baguette, 7))



# ----------
# V teslls us something about the orientation of the baguette in the 3D space.
# More technically, it defines three orthogonal vectors (v1, v2, v3), meaning that they are perpendicular to each other.

# All inner product is zero:  orthogonal
round(V[,1] %*% V[,2], 7)
round(V[,1] %*% V[,3], 7)
round(V[,2] %*% V[,3], 7)


# Eucledean norm (length) is all 1
sqrt(sum(V[,1]^2))
sqrt(sum(V[,2]^2))
sqrt(sum(V[,3]^2))



# ----------
library("rgl")

plot3d(baguette, col = "gray", xlim = c(-4, 4), ylim = c(-4, 4), zlim = c(-4, 4), aspect = 1, size = 2)
arrow3d(c(0,0,0), V[,1], col = "red")
arrow3d(c(0,0,0), V[,2], col = "red")
arrow3d(c(0,0,0), V[,3], col = "red")



# ----------
# D's singular values tell us something about the variance of the points along V's axes
# Note that singular values are alwyas in descending order, that is, the first one is always the largest and the last one always the smallest.
# The first singular value is by far the largest since it tells us something about the stretch of the data on the first axis ("long" baguette direction)
# The remaining two singular values are of approximately the same length since they describe the round-ish slice.

svdb$d



# ---------------------------------------------------------------------------
# eigenvalue decomposition
#   - This decomposition is restricted to square matrices.
# ---------------------------------------------------------------------------

# Note that we use covariance matrix (it is square matrix !!)
( covb <- cov(baguette) )

( eigend <- eigen(covb) )



# eigenvalues
L <- diag(eigend$values)

round(L, 3)



# eigenvectors 
U2 <- eigend$vectors

round(U2, 3)



# ----------
X <- U2 %*% L %*% t(U)

identical(round(X, 7), round(covb, 7))



# ----------
# All inner product is zero:  orthogonal
round(U2[,1] %*% U2[,2], 7)
round(U2[,1] %*% U2[,3], 7)
round(U2[,2] %*% U2[,3], 7)


# Eucledean norm (length) is all 1
sqrt(sum(U2[,1]^2))
sqrt(sum(U2[,2]^2))
sqrt(sum(U2[,3]^2))



# ---------------------------------------------------------------------------
# Eigenvalues from SVD
# ---------------------------------------------------------------------------
# eignevalu is the square of a singular value
# eigenvalues = { singular values / sqrt(n-1) }^2
# SVD is a generalization of an eigenvalue decomposition in terms of input matrices with unequal number of rows and columns


l <- ( svdb$d / sqrt(nrow(baguette) - 1) )^2

round(l, 3)
round(eigend$values, 3)

