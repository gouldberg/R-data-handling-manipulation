setwd("//media//kswada//MyFiles//R//R_basics")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Simulate Metropolis Algorithm
#  - The algorithm will still work even if we allow the king to be equally likely to propose a move to any island from any island, not just among neighbors
#  - The algorithm would also work for any size archipelago, even if the king did not know how many islands were in it.
#    All he needs to know at any point in time is the population of the current island and the population of the proposal island.
#    Then, without any forward planning or backwards record keeping, the king can satisfy his royal obligation to visit his people proportionally.
# ------------------------------------------------------------------------------
num_weeks <- 1e5

positions <- rep(0, num_weeks)

current <- 10

for(i in 1:num_weeks){
  positions[i] <- current
  
  proposal <- current + sample(c(-1, 1), size = 1)
  
  if(proposal < 1) proposal <- 10
  if(proposal > 10) proposal <- 1
  
  prob_move <- proposal / current
  current <- ifelse(runif(1) < prob_move, proposal, current)
}


par(mfrow=c(1,1))
plot(positions[1:200], type="l")
plot(positions[1:1000], type="l")

barchart(table(positions), horiz=FALSE)