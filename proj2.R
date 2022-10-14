# Statistical Programming Coursework 2

# Members and contributions ---------------------------------------------------------------
## 1. Stefi Tirkova
## 2. Passara Chanchotisatien

# CODE DESCRIPTION

# The purpose of this code is to stimulate the prisoner problem which is defined as follows (from coursework spec):
  # • 2n prisoners each have a unique prisoner number from 1 to 2n.
  # • The prison contains a room in which there are 2n boxes, each with a unique number from 1 to 2n painted
  # on its lid.
  # • 2n cards, each printed with a unique number from 1 to 2n, are randomly placed one in each box.
  # • The prisoners have the task of finding the card with their number on it by opening a maximum on n boxes.
  # • After each prisoner’s go, the room is returned exactly to its original state and the prisoner is not allowed to
  # communicate with prisoners yet to have their go.
  # • If all prisoners succeed in finding their number, then they all go free.

# This code calculates the probability of prisoners going free if they follow one of 3 possible strategies (from coursework spec):
  # Strategy 1: The prisoner starts at the box with their number on it, opens it and reads the number on the card: k, say. If k
  # is not their prisoner number, they go to box number k, open it and repeat the process until they have either
  # found the card with their number on it, or opened n boxes without finding it.
  # Strategy 2: As strategy 1, but starting from a randomly selected box.
  # Strategy 3: They open n boxes at random, checking each card for their number.


# Helper / Additional functions


is_successful <- function(n, pris_n, card_num, strategy) {
  if(strategy == 3){if (pris_n %in% sample(card_num,n)){return(1)}else{return(0)}} 
  boxes_opened <- 0
  current_box_num <- c(pris_n, sample(as.integer(n+n), 1))[strategy]
  while(boxes_opened < n) {
    if(card_num[current_box_num] == pris_n) { return(1) }
    boxes_opened = boxes_opened + 1
    current_box_num <- card_num[current_box_num]
  }
  return (0)  
}


# Question 1 -----------------------------------------------------------------------------------------------------------


pone <- function(n, k, strategy, nreps) {
  two_n <- as.integer(n+n) 
  success_count <- 0
  for(i in 1:nreps){success_count<-success_count+is_successful(n,k,sample(1:two_n),strategy)}
  return((success_count / nreps))
}


# Question 2 ------------------------------------------------------------------------------------------------------------

pall <- function(n, strategy, nreps) {
  success_vec <- rep(0,nreps)
  two_n <- as.integer(n+n)
  for(i in 1:nreps){
    card_numbers <- sample(1:two_n)
    prisoner_status = rep(1,two_n)
    for(prisoner_num in 1:two_n){ prisoner_status[prisoner_num] <- is_successful(n, prisoner_num, card_numbers, strategy)}
    success_vec[i] <- sum(prisoner_status)
  }
  probability_all_succeed <- (length(success_vec[success_vec == two_n])/nreps)
  return(list(probability_all_succeed, success_vec))
}


# Question 3 -------------------------------------------------------------------------------------------------------------

# Here, we estimate the individual and joint success probabilities for each strategy
# for when n = 5 and n = 50.

num_trials = 10000

# one prisoner succeeding in finding their number
print("Estimating the probability of a single prisoner succeeding:")
n = 5
print("n = 5")
cat("Strategy 1 resulted in probability of ", pone(n, 3, 1,num_trials), ".",sep="")
cat("Strategy 2 resulted in probability of ", pone(n, 3, 2,num_trials), ".",sep="")
cat("Strategy 3 resulted in probability of ", pone(n, 3, 3,num_trials), ".",sep="")

n = 50
cat("Strategy 1 resulted in probability of ", pone(n, 34, 1,num_trials), ".",sep="")
cat("Strategy 2 resulted in probability of ", pone(n, 34, 2,num_trials), ".",sep="")
cat("Strategy 3 resulted in probability of ", pone(n, 34, 3,num_trials), ".",sep="")


# all prisoners succeeding in finding their number
print("Estimating the probability of all prisoners succeeding:")
n = 5
cat("Strategy 1 resulted in probability of ", unlist(pall(n,1,num_trials)[1]), ".",sep="")
cat("Strategy 2 resulted in probability of ", unlist(pall(n,2,num_trials)[1]), ".",sep="")
cat("Strategy 3 resulted in probability of ", unlist(pall(n,3,num_trials)[1]), ".",sep="")

n = 50
cat("Strategy 1 resulted in probability of ", unlist(pall(n,1,num_trials)[1]), ".",sep="")
cat("Strategy 2 resulted in probability of ", unlist(pall(n,2,num_trials)[1]), ".",sep="")
cat("Strategy 3 resulted in probability of ", unlist(pall(n,3,num_trials)[1]), ".",sep="")



# Question 4 -----------------------------------------------------------------------------------------------------------

# In this section, we elaborate why the results are surprising.

## From running the simulations, we are able to see that one of the three strategies work
# surprisingly well which is strategy 1 which gives an approximate 31% chance of all
# prisoners succeeding in finding their numbers. We found this surprising because we know
# that the probability of one prisoner succeeding is 0.5. Say if n = 50, and if we were to 
# think about this naively, to find the probability of all prisoners succeeding, we would do:
# P(success) = (0.5)^100 (where n = 50), which would give us an extremely extremely low
# probability. That is why it is surprising to find out that there is a way where there is a
# 31% chance of all prisoners succeeding. When we calculate the probability using the 3rd 
# strategy, the probability of each prisoner succeeding is independent of each other. However,
# when we use Strategy 2, that is no longer the case.

# Below we can see the difference in probabilities and number of successful prisoners between 
# Strategy 1 and Strategy 3. We visualize the frequencies of each number of successful prisoners
# below for strategies 1, 2, and 3 in the form of histograms.

par(mfrow=c(1,3))

strategy_1 <- unlist(pall(50,1,1000)[2])
hist(strategy_1, breaks=100, xlab="Successful prisoners count")

strategy_2 <- unlist(pall(50,2,1000)[2])
hist(strategy_2, breaks=100, xlab="Successful prisoners count")

strategy_3 <- unlist(pall(50,3,1000)[2])
hist(strategy_3, breaks=100, xlab="Successful prisoners count")

# We see that in strategy 1, either all prisoners win together or the majority loses together.
# Whereas in strategy 3, it is half and half (mostly between 4-60 successful prisoners).


# Question 5 ------------------------------------------------------------------------------------------------




# Question 6 ------------------------------------------------------------------------------------------------




