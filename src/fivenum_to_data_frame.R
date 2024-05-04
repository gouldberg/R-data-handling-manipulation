
# ---------------------------------------------------------------------------
# fivenum to vector
# ---------------------------------------------------------------------------
df <- data.frame(
  a = rnorm(100),
  b = rpois(10, 100),
  c = rbinom(10, prob=0.1, n = 100)
)


head(df)



# ----------
# summary of five numbers
# --> not tidy data
( tmp <- summary(df) )
tmp %>% unname()



# ----------
# summary of five numbers:  Min, 1st Qu., Median, 3rd Qu., Max
# Note that MEAN is not included in five numbers
summary(df)
apply(df, MARGIN=2, FUN=fivenum)



# ----------
# Does same thing
sapply(df, fivenum)

do.call(cbind, lapply(df, fivenum))



# ----------
# vapply can specify output format by FUN.VALUE
# FUN.VALUE = c(0,0,0,0,0) means the output is 5 length vector
( output <- vapply(df, fivenum, FUN.VALUE = c(0,0,0,0,0)) )

row.names(output) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")


# You can set names to the each row of the output
vapply(df, fivenum, FUN.VALUE = c(Min.=0, "1st Qu."=0, Median=0, "3rd Qu."=0, Max.=0))



