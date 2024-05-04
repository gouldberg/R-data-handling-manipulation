library(dplyr)
library(magrittr)



# ---------------------------------------------------------------------------
# the "tee" pipe: "%T>%"
#  - it returns the lefthand side instead of the righthand side
#  - It's called "tee" because it's like a literal T-shaped pipe
#  - This operator allows you to continue piping functions that normally cause termination
# ---------------------------------------------------------------------------
rnorm(100) %>% matrix(ncol = 2) %T>% plot() %>% str()


# --> This is like ...
rnorm(100) %>% matrix(ncol = 2) %>% plot()
rnorm(100) %>% matrix(ncol = 2) %>% str()



# ----------
mtcars %>% filter(carb > 1) %>% extract(., 1:4) %T>% plot() %>% summary()



# ---------------------------------------------------------------------------
# %$%:  explodes out the variables in a data frome so that you can refer to them explicitly
# ---------------------------------------------------------------------------
mtcars %$% cor(disp, mpg)


# --> This is like ...
with(mtcars, cor(disp, mpg))



# ---------------------------------------------------------------------------
# %<>%:  update a value by first piping it into one or more expressions, and then assigning the result
# ---------------------------------------------------------------------------
head(mtcars)

mtcars %<>% transform(cyl = cyl * 2)

head(mtcars)


# --> This is like ...
mtcars <- mtcars %>% transform(cyl = cyl * 2)


# ----------
mtcars$mpg %<>% sqrt


