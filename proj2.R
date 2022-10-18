# Statistical Programming Coursework 2

# Members and contributions ---------------------------------------------------------------
## 1. Stefi Tirkova : Q4, Q5, Q6
## 2. Passara Chanchotisatien: Q1, Q2, Q3

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

# get pall_success_vec

get_pall_success_vec <- function(n, strategy, nreps) {
  success_vec <- rep(0,nreps)
  two_n <- as.integer(n+n)
  for(i in 1:nreps){
    card_numbers <- sample(1:two_n)
    prisoners_success <- rep(1,two_n)
    for(prisoner_num in 1:two_n){ 
      prisoners_success[prisoner_num] <- is_successful(n, prisoner_num, card_numbers, strategy)
    }
    # calculate how many prisoners succeeded in current simulation by summing over their successes
    success_vec[i] <- sum(prisoners_success)
  }
  return(success_vec)
}

# is_successful function

is_successful <- function(n, pris_n, card_num, strategy) {
  if(strategy == 3) {
    if (pris_n %in% sample(card_num,n)){ return(1) } 
    else{ return(0)} 
  } 
  boxes_opened <- 0
  # intitialise the first box to open according to the strategy
  # I THINK THIS IS EASIER TO READ
  current_box_num <- if (strategy == 1) pris_n else if (strategy == 2) sample(as.integer(2*n), 1)
  # current_box_num <- c(pris_n, sample(as.integer(n+n), 1))[strategy]
  while(boxes_opened < n) {
    if(card_num[current_box_num] == pris_n) { return(1) }
    boxes_opened <- boxes_opened + 1
    current_box_num <- card_num[current_box_num]
  }
  return (0)  
}

# pone function

pone <- function(n, k, strategy, nreps) {
  success_count <- 0
  for(i in 1:nreps){
    success_count <- success_count + is_successful(n,k,sample(1:as.integer(n+n)), strategy)
  }
  return((success_count / nreps))
}

# pall function

pall <- function(n, strategy, nreps) {
  success_vec <- get_pall_success_vec(n, strategy, nreps)
  # p_success = (number of simulations all prisoners succeed)/(number of simulations) 
  probability_all_succeed <- (length(success_vec[success_vec == as.integer(n+n)])/nreps)
  return(probability_all_succeed)
}


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
# save results to plot later
strategy_1_all50 <- get_pall_success_vec(n,1,num_trials)
strategy_2_all50 <- get_pall_success_vec(n,2,num_trials)
strategy_3_all50 <- get_pall_success_vec(n,3,num_trials)
cat("Strategy 1 resulted in probability of ", pall(n,1,num_trials), ".",sep="")
cat("Strategy 2 resulted in probability of ", pall(n,1,num_trials), ".",sep="")
cat("Strategy 3 resulted in probability of ", pall(n,1,num_trials), ".",sep="")


# In this section, we elaborate why the results are surprising.

## From running the simulations, we are able to see that one of the three strategies works
# surprisingly - strategy 1 which gives an approximate 31% chance of all
# prisoners succeeding in finding their numbers. 
# We found this surprising because we know that the probability of one prisoner succeeding is 0.5. 
# Say if n = 50, and if we were to think about this naively, to find the probability of all prisoners succeeding, 
# we would do the following calculation:
# P(success) = (0.5)^100 (where n = 50), 
# which would give us an extremely extremely low probability (almost zero). 
# That is why it is surprising to find out that there is a strategy that gives
# 31% chance of all prisoners succeeding. When we calculate the probability using the 3rd
# strategy, the probability of each prisoner succeeding is independent of each other. However,
# when we use Strategy 1, that is no longer the case.

# Below we can see the difference in probabilities and number of successful prisoners between 
# Strategy 1 and Strategy 3. We visualize the frequencies of each number of successful prisoners
# below for strategies 1, 2, and 3 in the form of histograms.

# PLEASE MAKE SURE TO OPEN PLOTS IN LARGE ENOUGH WINDOW IN ORDER TO SEE THEM PROPERLY
# NOTE: these figures might be overwritten by later figures, please rerun following 4 lines if you want to see the figures again
par(mfrow=c(1,3))
hist(strategy_1_all50, breaks=100, xlab="Successful prisoners count", main = "Histogram of strategy 1 (n=50)")
hist(strategy_2_all50, breaks=100, xlab="Successful prisoners count", main = "Histogram of strategy 2 (n=50)")
hist(strategy_3_all50, breaks=100, xlab="Successful prisoners count", main = "Histogram of strategy 3 (n=50)")

# We see that in strategy 1, either all prisoners win together or the majority loses together.
# Whereas in strategy 3, it is half and half (mostly between 4-60 successful prisoners).




# HELPER FUNCTION

get_loop_len <- function(start_box, random_shuffle, boxes_is_visited){
  loop_len <- 1
  boxes_is_visited[start_box] <- 1
  current_box <- start_box
  while (random_shuffle[current_box] != start_box){
    current_box <- random_shuffle[current_box]
    loop_len <- loop_len + 1
    boxes_is_visited[current_box] <- 1
  }
  return (list(loop_len, boxes_is_visited))
  
}

dloop <- function(n, nreps){
  # vector to store the number of times a loop of some length (from 1 to 2n) has been seen  
  loop_len_counts <- rep(0,2*n)
  
  # run simulation nrep times and count length of loops
  for (i in 1:nreps){
    loop_len_occurs <- rep(0,2*n)
    # get a random suffling of cards from 1 to 2n 
    random_shuffle <- sample(1:(2*n))
    boxes_is_visited <- rep(0,2*n)
    start_box <- 1
    while (sum(boxes_is_visited) != 2*n){
      result <- get_loop_len(start_box, random_shuffle, boxes_is_visited)
      loop_len_occurs[unlist(result[1])] <-  1
      boxes_is_visited <- unlist(result[2])
      # choose a new start box by choosing the first non-visited box
      start_box <- which(boxes_is_visited == 0)[1]
    }
    loop_len_counts <- loop_len_counts + loop_len_occurs
  }
  
  # convert counts to probabilites
  probs <- loop_len_counts/nreps
  return (probs)
}

# Visualising probabilities for n=50
nreps = 10000
n = 50
probs <- dloop(n,nreps)

# PLEASE MAKE SURE TO OPEN PLOTS IN LARGE ENOUGH WINDOW IN ORDER TO SEE THEM PROPERLY
# NOTE: Following 2 figures overwrite previous ones, please rerun visualisation code if you want to see previous figures
par(mfrow=c(2,1))
barplot(probs*100, xlab="Loop length", ylab="Probability (%)", names.arg = c(1:100))
title("Probability of each loop length occuring at least once (n=50)")


# A group of 2n prisoners will succeed with strategy 1 iff the longest loop is at most n
# The code below computes the probability distribution of longest loops for n=50

longest_loop <- function(n, nreps){
  longest_count <- rep(0,2*n)
  for (i in (1:nreps)){
    at_least_once_probs <- dloop(n, 1)
    # find longest loop (take the index of the last non-zero position in the at_least_once_probs vector)
    x <- tail(which(at_least_once_probs!= 0), 1)
    longest_count[x] <- longest_count[x] + 1
  }
  # return vector of probabilities
  return (longest_count/nreps)
}

data_longest_loop <- longest_loop(50, 10000)

# probaility that longest loop is between 1 and 50 (aka. not longer than 50)
longest_50_prob <- sum(data_longest_loop[1:50])
cat("Probability that there is no loop longer than 50 is approximatelly ", longest_50_prob*100, "%.")
cat("This matches the result of our earlier simulation!")

# Visualising longest loop length probability distribution
# PLEASE MAKE SURE TO OPEN PLOTS IN LARGE ENOUGH WINDOW IN ORDER TO SEE THEM PROPERLY
# use different colours for values <=50 and >50
colours <- c(rep("green3",50), rep("red",50)) 
barplot(data_longest_loop*100, names.arg = c(1:100), col= colours, 
        xlab="Length of longest loop (L)", ylab="Probability (%)")
title("Probability distribution of longest loop length (n=50)")
text(x=30,y=1.7,labels="P(L<=50) = 0.31", col="darkgreen")
text(x=90,y=1.7,labels="P(L>50) = 0.69", col="red")


