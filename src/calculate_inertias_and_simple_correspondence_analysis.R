library(dplyr)


superfan <- as.table(matrix(c(9, 12, 8, 1, 13, 1, 6, 20, 15, 4, 23, 18), ncol = 3))


attr(superfan, "dimnames") <- list(Band = c("Slayer", "Iron Maiden", "Metallica", "Judas Priest"), 
                                   Fan = c("Horst", "Helga", "Klaus"))


superfan



# ---------------------------------------------------------------------------
# Chi-square test of independence
# ---------------------------------------------------------------------------

# Pearson's X^2-test
fit_chisq <- chisq.test(superfan)

fit_chisq



# ----------
# standardized residuals ("Pearson residuals")

S <- fit_chisq$residuals

round(S, 3)



# ----------
# Mosaic plot to display the residual information (as deviation from independence)

library(vcd)

mosaic(superfan, shade = TRUE)



# -->
# We see that the cell related to Horst and Iron Maiden shows the largest deviation from independence.



# ---------------------------------------------------------------------------
# Row profiles and column profiles (conditional relative frequencies)
# ---------------------------------------------------------------------------

# table with relative frequencies
P <- prop.table(superfan)

round(P, 3)
sum(P)



# ----------
# row masses and column masses
( r_mass <- margin.table(P, 1) )
( c_mass <- margin.table(P, 2) )



# ----------
# row profiles and column profiles (= conditional relative frequencies for the rows and the columns)
( r_profile <- prop.table(P, 1) )
( c_profile <- prop.table(P, 2) )



# ----------
# average row profile and average column profile (= row centroid and column centroid)
# These average profiles are matched with row masses and column masses
( ar_profile <- t(r_profile) %*% r_mass )
c_mass

( ac_profile <- c_profile %*% c_mass )
r_mass



# ---------------------------------------------------------------------------
# Visualize Row profiles
#   - Since we have three fans only, their row profiles can be plotted in a 3D space.
#     Each fan gets its own axis.  The band ponts lie on a plane spanned by the column (i.e., fan) vertices.
#     They lie on a triangle in a 3D space.  ("simplex structure")
# ---------------------------------------------------------------------------

library(plot3D)

tc <- r_profile

scatter3D(x = tc[,1], y = tc[,2], z = tc[,3], xlab = "Horst", ylab = "Helga", zlab = "Klaus", colkey = FALSE, 
          col = 1, pch = 20, xlim = c(0,1), ylim = c(0,1), zlim = c(0,1), ticktype = "simple", type = "h", 
          phi = 40, theta = 50, main = "Row Profiles", bty = "g")

points3D(x = c(0,0,1), y = c(0,1,0), z = c(1,0,0), col = "red", add = TRUE, pch = 20)

lines3D(x = c(0,0,1,0), y = c(0,1,0,0), z = c(1,0,0,1), col = "red", add = TRUE)

text3D(x = tc[,1], y = tc[,2], z = tc[,3], labels = rownames(tc), pos = 3, add = TRUE, cex = 0.8, adj = -0.1) 



# ----------
# ternary plot:  2D plot where the axes are determined by the simplex.
library(ggtern)

tf <- as.data.frame.matrix(superfan)

c_mass <- as.vector(c_mass)

gt <- ggtern(data = tf, aes(Horst, Helga, Klaus))

( lines <- data.frame(x = c(c_mass[1], 1 - c_mass[3], 0),
                    y = c(1 - c_mass[1], 0, c_mass[2]),
                    z = c(0, c_mass[3], 1 - c_mass[2]),
                    xend = c(c_mass[1], c_mass[1], c_mass[1]),
                    yend = c(c_mass[2], c_mass[2], c_mass[2]),
                    zend = c(c_mass[3], c_mass[3], c_mass[3]), row.names = NULL) )

gt + geom_point() + theme_rgbw() + geom_text(label = rownames(tf), vjust = -0.5) +
  geom_point(aes(x = c_mass[1], y = c_mass[2], z = c_mass[3]), colour = "red", size = 4) +
  geom_segment(data = lines, aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), 
               color = 'red', size = 0.5) + labs(title = "Ternary Plot")


# -->
# The red dot represents the band centroid (i.e., average row profile) with the corresponding projections on the three fan axes.
# Slayer's profile is very close to the centroid.
# Iron Maiden scores high on Horst's axies, but low on Helga's and Klaus' axes, as suggested by vcd::mosaic()



# ---------------------------------------------------------------------------
# Quantify the distances between two bands or betwen a band and the average row profile
#   - We can use the X^2-distance. It weights each term in the Euclidean distance by the expected profile lemenet (element in the average row profile)
# ---------------------------------------------------------------------------

# The distance between two bands
# Slayer and Iron Maiden, Slayer and Judas Priest
sqrt( sum( (r_profile["Slayer",] - r_profile["Iron Maiden",])^2 / ar_profile ) )
sqrt( sum( (r_profile["Slayer",] - r_profile["Judas Priest",])^2 / ar_profile ) )



# The distance between a band and the average band profile.
sqrt( sum( (r_profile["Slayer",] - ar_profile)^2 / ar_profile ) )
sqrt( sum( (r_profile["Iron Maiden",] - ar_profile)^2 / ar_profile ) )



# ---------------------------------------------------------------------------
# calculate inertias
# ---------------------------------------------------------------------------

# row inertia = row masses * { sum of row profile distances to the centroid }^2

# row inertia for each band --> ??
( Slayer_inertia <- r_profile["Slayer",] * sum( (r_profile["Slayer",] - ar_profile)^2 / ar_profile ) )
( Iron_inertia <- r_profile["Iron Maiden",] * sum( (r_profile["Iron Maiden",] - ar_profile)^2 / ar_profile ) )
( Metallica_inertia <- r_profile["Metallica",] * sum( (r_profile["Metallica",] - ar_profile)^2 / ar_profile ) )
( Judas_inertia <- r_profile["Judas Priest",] * sum( (r_profile["Judas Priest",] - ar_profile)^2 / ar_profile ) )



# ----------
# total inertia --> it does not match ... why ??
# total inertia describes total amount of dispersion of the profiles around the centroid.
Slayer_inertia + Iron_inertia + Metallica_inertia + Judas_inertia

as.vector(fit_chisq$statistic) / sum(superfan)



# ---------------------------------------------------------------------------
# Simple Correspondence Analysis
#   - anacor package provides some useful features such as the computation of confidence ellipsoids.
#   - In the case of simple CA, we perform an singular value decomposition on the standardized residual matrix S = U * D * t(V)
#     D contains singular values (eigenvalues = singular values ^2)
#   - Mapping the principal coordinates U(p) and V(p) into the same plot gives us a symmetric map
#     p: the number of dimension
#     R: diag(row_masses)
#     C: diag(column masses)
#     U(p) = R^(-0.5) * U * D    V(p) = C^(-0.5) * V * D
#
#   - Be careful in a symmetric map that CA defines no distances between row and column categories.
#     In fact, the row and column profiles are computed separately.
#     But Greenacre (2007) discusses in great detail the practical implication of this distortion induced by using symmetic map as if they were asymmetric maps
#     and concludes that in cases where the square roots of the principal inertias are not heavily different, row-column relationships in a symmetric map
#     can be interpreted with reasonable assurance.
# ---------------------------------------------------------------------------

library(anacor)

ca_fans <- anacor(superfan, ellipse = TRUE)

ca_fans


# -->
# Note taht in simple CA, the maximum number of dimensions we can fit is min(I - 1, J - 1).
# In our simple example, we can maximally reduce to 2 dimensions wince we have only 3 fans.

# eigenvalues reflect the dispersion of profiles around the centroid on the dimension,
# which is the contribution to the total inertia associated with that dimension.

# Total inersia = sum of eigenvalues = 0.304 is decomposed into 2 principal inertias

# We see that the first dimension explains 84% of the dispersion in the row/column profiles.
# and 2nd dimension takes care of the remaining 16%.
# The anacor() prints out an explicit test on whether a dimension is needed  --> the p-value <= 0.05



# ----------
# plot the principal row and column coordinates in the same plot
# including confidence  ellisoids.
plot(ca_fans, main = "Symmetric CA Map")


# -->
# Slayer is close to the origin since thier row profile is close to the average row profile.
# Iron Maiden is the most extreme band since their row profile deviates heavily from the average one.
# The distances among the bands in the plot can be interpreted as X^2-distances.


