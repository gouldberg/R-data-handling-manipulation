
# ---------------------------------------------------------------------------
# convert or generate vector to other mode
# ---------------------------------------------------------------------------

# vector with 0 values
vector("numeric", 5)



# vector with empty character
vector("character", 5)



# empty list
vector("list", 2)



# convert 1:5 to character 1,2,3,4,5
as.vector(1:5, "character")



# only 0 is FALSE
as.vector((-1):5, "logical")

as.numeric(as.vector((-1):5, "logical"))

