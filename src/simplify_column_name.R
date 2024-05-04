
# ---------------------------------------------------------------------------
# Simplify column name by abbreviate()
# ---------------------------------------------------------------------------
head(iris)

names(iris) <- abbreviate(names(iris))

str(iris)


