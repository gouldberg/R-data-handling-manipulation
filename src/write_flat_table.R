setwd("//media//kswada//MyFiles//R_basics")


# ---------------------------------------------------------------------------
# write flat table to disk
# ---------------------------------------------------------------------------
str(esoph)

( tab <- xtabs(cbind(ncases, ncontrols) ~ ., data = esoph) )

( tab_f <- ftable(tab) )



# ----------
write.ftable(tab_f, file=".//tmp//tmp0.txt", sep="\t", row.names=F, quote=F)

write.table(tab_f, file=".//tmp//tmp1.txt", sep="\t", row.names=F, quote=F)

write.table(tab, file=".//tmp//tmp2.txt", sep="\t", row.names=F, quote=F)

