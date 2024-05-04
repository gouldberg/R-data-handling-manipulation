
# ---------------------------------------------------------------------------
# Replace body of function
# ---------------------------------------------------------------------------
foo <- function(x, y=3){
  z <- x * y
  a <- z + 100
}



# ----------
# extract body of function
body(foo)


str(body(foo))



# ----------
# other function "boo"
boo <- function(x, y=10){
  z <- x * y
  a <- z - 100
}



# ----------
( foo(x=1) )


# replace the body by "boo"
body(foo) <- body(boo)


( foo(x=1) )



# ----------
# you can replace by expression
body(foo) <- expression(x + 1)

body(foo)

( foo(x=1) )
