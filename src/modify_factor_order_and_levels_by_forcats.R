library(dplyr)
library(forcats)



# ---------------------------------------------------------------------------
# data:  General Social Survey
#  - a long-running US survey conducted by the independent research organization NORC at the University of Chicago.
# ---------------------------------------------------------------------------
gss_cat

str(gss_cat)



# ---------------------------------------------------------------------------
# See the factor levels
# ---------------------------------------------------------------------------
gss_cat %>% count(race)


# ----------
# By default, ggplot2 will drop levels that do not have any values
# You can force them to display with
ggplot(gss_cat, aes(race)) + geom_bar()

ggplot(gss_cat, aes(race)) + geom_bar() + scale_x_discrete(drop = FALSE)



# ---------------------------------------------------------------------------
# Visualize by reordering the levels by fct_reorder()
# fct_reorder() takes 3 arguments
#  - f, the factor whose levels you want to modify
#  - x, a numeric vector that you want to use to reorder the levels
#  - Optionally, fun, a function that's used if there are multiple values of x for each value of f. The default value is median.
# ---------------------------------------------------------------------------
# average number of hours spent watching TV per day across religions

relig <- gss_cat %>% group_by(relig) %>% summarize(
  age = mean(age, na.rm=TRUE),
  tvhours = mean(tvhours, na.rm=TRUE),
  n = n()
)


relig



# ----------
# It is difficult to interpret this plot because there's no overall pattern.
ggplot(relig, aes(tvhours, relig)) + geom_point()



# ----------
# We can improve it by reordering the levels using fct_reorder()
ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) + geom_point()



# ---------------------------------------------------------------------------
# Visualize by releveling the levels by fct_relevel()
# ---------------------------------------------------------------------------

# how average age varies across reported income level ?
rincome <- gss_cat %>% group_by(rincome) %>% summarize(
  age = mean(age, na.rm = TRUE),
  tvhours = mean(tvhours, na.rm = TRUE),
  n = n()
)

rincome



# ----------
# Here, arbitrarily reordering the levels is not a good idea !
# That's because rincome already has aprincipled order.
ggplot(rincome, aes(age, fct_reorder(rincome, age))) + geom_point()



# ----------
# Relevel "Not applicable" to move to the front of the line
ggplot(rincome, aes(age, fct_relevel(rincome, "Not applicable"))) + geom_point()



# ---------------------------------------------------------------------------
# Visualize by reordering by fct_reorder2()
# ---------------------------------------------------------------------------
# fct_reorder2() reorders the factor by the y values associated with the largest x values
by_age <- gss_cat %>% filter(!is.na(age)) %>% group_by(age) %>% count(marital) %>% mutate(prop = n / sum(n))

by_age

ggplot(by_age, aes(age, prop, color = marital)) + geom_line(na.rm=TRUE)

ggplot(by_age, aes(age, prop, color = fct_reorder2(marital, age, prop))) + geom_line() + labs(color = "marital")



# ---------------------------------------------------------------------------
# Visualize barchart by ordering levels in increasing frequency by fct_infreq(), combined with fct_rev()
# ---------------------------------------------------------------------------
gss_cat %>% mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>% ggplot(aes(marital)) + geom_bar()

gss_cat %>% mutate(marital = marital %>% fct_infreq()) %>% ggplot(aes(marital)) + geom_bar()



# ---------------------------------------------------------------------------
# Modify factor levels
#  - fct_recode()
#  - fct_collapse()
#  - fct_lump()
# ---------------------------------------------------------------------------
# The levels are terse and inconsistent.
gss_cat %>% count(partyid)



# ----------
# fct_recode() will leave levels that are not explicitly mentioned as is, and will warn you if you accidentally refer to a level that does not exist.
gss_cat %>% mutate(partyid = fct_recode(partyid,
  "Republican, strong" = "Strong republican",
  "Republican, weak" = "Not str republican",
  "Independent, near rep" = "Ind,near rep",
  "Independent, near dem" = "Ind,near dem",
  "Democrat, weak" = "Not str democrat",
  "Democrat, strong" = "Strong democrat"
)) %>% count(partyid)



# ----------
# To combine groups, you can assign multiple old levels to the same new level ("Other")

gss_cat %>% mutate(partyid = fct_recode(partyid,
                                        "Republican, strong" = "Strong republican",
                                        "Republican, weak" = "Not str republican",
                                        "Independent, near rep" = "Ind,near rep",
                                        "Independent, near dem" = "Ind,near dem",
                                        "Democrat, weak" = "Not str democrat",
                                        "Democrat, strong" = "Strong democrat",
                                        "Other" = "No answer",
                                        "Other" = "Don't know",
                                        "Other" = "Other party"
)) %>% count(partyid)



# ----------
# If you want to collapse a lot of levels, fct_collapse() is a usuful variant of fct_recode()
gss_cat %>% mutate(partyid = fct_collapse(partyid,
                                          other = c("No answer", "Don't know", "Other party"),
                                          rep = c("Strong republican", "Not str republican"),
                                          ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                          dem = c("Not str democrat", "Strong democrat")))



# ----------
# You just want to lump together all the small groups to make a plot or table simpler
# default behavior is to progressively lump together the smallest groups, ensuring that the aggregate is still the smallest group.
# In this case it is not very helpful:  overcollapsed
gss_cat %>% mutate(relig = fct_lump(relig)) %>% count(relig)


# instead, we can use the n parameter to specify how many groups (excluding other) we want to keep
gss_cat %>% mutate(relig = fct_lump(relig, n = 10)) %>% count(relig, sort = TRUE) %>% print(n = Inf)
