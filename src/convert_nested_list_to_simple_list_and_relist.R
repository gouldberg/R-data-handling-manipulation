
# ---------------------------------------------------------------------------
# unlist() convert nested list to simple list
# ---------------------------------------------------------------------------
( l.ex <- list(a = list(1:2, LETTERS[1:3], b = "Z", c = NA)) )



# ----------
# convert nested list to simple list
l.ex %>% unlist(., recursive=FALSE)



# ---------------------------------------------------------------------------
# as.relistable() and relist()
# ---------------------------------------------------------------------------
( ipar <- list(mean=c(0,1), vcov=cbind(c(1,1),c(1,0))) )


# as.relistable()
ipar2 <- as.relistable(ipar)



# ----------
# unlist
( ul <- unlist(ipar2) )


str(ul)



# ----------
# relist
( rel <- relist(ul) )

identical(ipar2, rel)

