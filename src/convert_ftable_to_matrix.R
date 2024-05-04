library(dplyr)



# ---------------------------------------------------------------------------
# Convert ftable to matrix
# ---------------------------------------------------------------------------
set.seed(1234)
dim <- c(3, 2, 2, 2)

tab <- array(rpois(prod(dim), 15), dim = dim)

dimnames(tab) <- list(Pet = c("dog", "cat", "bird"),
                      Age = c("young", "old"),
                      Color = c("black", "white"),
                      Sex = c("male", "female"))

tab


# ----------
( ftab <- ftable(Pet + Age ~ Color + Sex, tab) )



# ----------
# as.matrix() creates a matrix with the levels of the stacked variables combined with some separator character
( pet.mat <- as.matrix(ftab, sep = ".") )




# ----------
# For reference:  
# data in frequency data frame to matrix
tab.df <- as.data.frame(as.table(tab))

tab.df <- within(tab.df,
                 { Pet.Age = interaction(Pet, Age)
                 Color.Sex = interaction(Color, Sex) })

tab.df

xtabs(Freq ~ Color.Sex + Pet.Age, data = tab.df)