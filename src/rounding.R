

# ---------------------------------------------------------------------------
# round()
# ---------------------------------------------------------------------------
# Note that round() is based on IEEE754 (bank) !!!
round(122.5)

round(123.345)

round(123.5)

round(124.5)



# ---------------------------------------------------------------------------
# trunc()
# ---------------------------------------------------------------------------
# truncate values to near integer toward to zero
( a <- (-10) : 20 / 3 )

trunc(a)



# ---------------------------------------------------------------------------
# zapsmall
# ---------------------------------------------------------------------------
( zx <- pi*100^(-1:1) )


zx / 1000


zapsmall(zx / 1000, digits = 4)



# ---------------------------------------------------------------------------
# round, signif, zapsmall
# ---------------------------------------------------------------------------
( zx <- pi*100^(-1:1) )


zx / 1000


round(zx/1000, digits=3)


# yuukou - suuji 3 digits
# default is 6 digits
signif(zx/1000, digits=3)



zapsmall(zx / 1000, digits = 2)



# ---------------------------------------------------------------------------
# Compare the result of round() and real rounding function
# ---------------------------------------------------------------------------
# function for real rounding
Round <- function(x, digit = 0){
  aux <- function(x) (x %/% 1) + ((10*(x%%1)) %/% 1 > 5)
  10^(-digit) * aux(x*10^digit)
}



# ----------
x <- rnorm(10, mean = 10, sd=20)
x

Round(x)
Round(x, 1)


# ----------
x <- rnorm(1e6, mean=1e4, sd=200)
x

mean(x - Round(x))
mean(x - round(x))
mean(abs(x - Round(x)))
mean(abs(x - round(x)))



# ----------
# round() is always smaller than real rounding 
mean(Round(x) - round(x))

