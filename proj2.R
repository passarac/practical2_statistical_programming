# Statistical Programming Coursework 2

# Members and contributions ----------------------------------------------------
## 1. Stefi Tirkova (s1713754) : Q4, Q5, Q6, comments
## 2. Passara Chanchotisatien (s2255740): Q1, Q2, Q3, Q4, comments

# GitHub repo:
# https://github.com/passaraink/practical2_statistical_programming

# CODE DESCRIPTION

# The purpose of this code is to stimulate the prisoner problem which is defined
# as follows (from coursework spec):
  # • 2n prisoners each have a unique prisoner number from 1 to 2n.
  # • The prison contains a room in which there are 2n boxes, each with a unique
  # number from 1 to 2n painted
  # on its lid.
  # • 2n cards, each printed with a unique number from 1 to 2n, are randomly
  # placed one in each box.
  # • The prisoners have the task of finding the card with their number on it by
  # opening a maximum on n boxes.
  # • After each prisoner’s go, the room is returned exactly to its original 
  # state and the prisoner is not allowed to
  # communicate with prisoners yet to have their go.
  # • If all prisoners succeed in finding their number, then they all go free.

# This code calculates the probability of prisoners going free if they follow one
# of 3 possible strategies (from coursework spec):
  # Strategy 1: The prisoner starts at the box with their number on it, opens it
  # and reads the number on the card: k, say. If k
  # is not their prisoner number, they go to box number k, open it and repeat the
  # process until they have either
  # found the card with their number on it, or opened n boxes without finding it.
  # Strategy 2: As strategy 1, but starting from a randomly selected box.
  # Strategy 3: They open n boxes at random, checking each card for their number.



is_successful <- function(n, pris_n, card_num, strategy) {
  # This function checks whether a prisoner is successful in finding their 
  # prisoner number or not
  
  # This is a helper function
  ## This function is called inside get_pall_success_vec and pone
  
  # @param n (integer): 2n is the number of prisoners/boxes/cards
  # @param pris_n (integer): current prisoner number
  # @param card_num (vector): of length 2n containing unique  random numbers form
  #        1 to 2n and represents the numbers on the card inside the boxes
  # @param strategy (integer): one of the 3 strategies to follow
  
  # @return an integer either 0 or 1, where 0 means that the prisoner was 
  # unsuccessful and 1 means the prisoner was successful in finding their number
  
  # in strategy 3, we generate a vector containing unique numbers from 1 to n 
  # (as prisoner can only open n boxes)
  if(strategy == 3) {
    # if their prisoner number is inside the vector, then it means they would 
    # have found their number the function returns 1 as they are successful
    if (pris_n %in% sample(card_num,n)){ return(1) }
    # if not, they were unsuccessful and the function returns 0
    else{ return(0)} 
  } 
  
  # at the start the prisoner has opened 0 boxes
  boxes_opened <- 0
  
  # intitialise the first box to open according to the strategy (1 or 2)
  current_box_num <- if (strategy == 1) pris_n 
                    else if (strategy == 2) sample(as.integer(2*n), 1)
  # as long as the number of boxes opened by the prisoner is less than n, the 
  # prisoner will continue opening the next box with the same number as the 
  # number written on the card
  while(boxes_opened < n) {
    # the prisoner finds the card with their prisoner number. This means they 
    # are successful and the function returns 1.
    if(card_num[current_box_num] == pris_n) { return(1) }
    # if the card inside the box they opened does not contain their prisoner 
    # number, they continue..
    # number of boxes opened is incremented by 1
    boxes_opened <- boxes_opened + 1
    # the prisoner next opens the box with the same number as the number they 
    # saw on the card
    current_box_num <- card_num[current_box_num]
  }
  # they have opened n boxes and still have not found their prisoner number; 
  # therefore, the function returns 0
  return (0)  
}



get_pall_success_vec <- function(n, strategy, nreps) {
  
  # simulates the prisoner problem nreps times and keeps track of how many 
  # successful prisoners are there in each trial
  
  # This is a helper function
  # This function is called inside pall
  
  # @param n (integer): 2n is the number of prisoners/boxes/cards in each 
  #                     simulation
  # @param strategy (integer): one of the 3 strategies to follow
  # @param nreps (integer): number of replicate simulations to run
  
  # @return success_vec (vector): consisting of nreps elements. The vector stores
  # how many prisoners successful found their number in each trial.
  
  # a vector to keep track of how many prisoners are successful in each trial
  success_vec <- rep(0,nreps)
  # declare a variable to store 2n
  two_n <- as.integer(n+n)
  
  # run the simulation nreps times
  for(i in 1:nreps){
    # randomly generated vector which stores the card numbers inside each box 
    # number (unique numbers from 1 to 2n)
    card_numbers <- sample(1:two_n)
    # a vector to store which prisoners were successful in each trial
    prisoners_success <- rep(1,two_n)
    # we do the following for each prisoner
    for(prisoner_num in 1:two_n){ 
      # if a prisoner is successful, the value inside prisoners_success at index
      # prisoners number is set to 1.If unsuccessful, it will be set to 0.
      prisoners_success[prisoner_num] <- is_successful(n, prisoner_num, 
                                                       card_numbers, strategy)
    }
    # calculate how many prisoners succeeded in current simulation by summing the
    # successes store that value in success_vec
    success_vec[i] <- sum(prisoners_success)
  }
  # return number of successful prisoners in each trial as a vector
  return(success_vec)
}



pone <- function(n, k, strategy, nreps) {
  # estimates the probability of a single prisoner finding their number by r
  # unning the simulation nreps times
  
  # @param n (integer): 2n is the number of prisoners/boxes/cards in each 
  #                     simulation
  # @param k (integer): current prisoner number
  # @param strategy (integer): one of the 3 strategies to follow
  # @param nreps (integer): number of replicate simulations to run
  
  # @return (double): the probability that a single prisoner succeeds in finding
  #                   their prisoner number
  
  # count successes across simulations 
  success_count <- 0
  
  # run simulation nreps times
  for(i in 1:nreps){
    # if the prisoner was successful, success count will be incremented by 1, 
    # else 0
    success_count <- success_count + is_successful(n,k,sample(1:as.integer(n+n)),
                                                   strategy)
  }
  # return the probability (number of successes divided by number of trials)
  return((success_count / nreps))
}



pall <- function(n, strategy, nreps) {
  # estimates the probability all prisoners find their number/ the probability 
  # that they all go free
  
  # @param n (integer): 2n is the number of prisoners/boxes/cards in each
  #                     simulation
  # @param strategy (integer): one of the 3 strategies to follow
  # @param nreps (integer): number of replicate simulations to run
  
  # @return probability_all_succeed (double): probability that all prisoners 
  # succeed in finding their number
  
  # get the number of successful prisoners in each trial of 1 to nreps trials
  success_vec <- get_pall_success_vec(n, strategy, nreps)
  # p_success = (number of simulations all prisoners succeed)/
  #             (number of simulations) 
  probability_all_succeed <- (length(success_vec[success_vec == as.integer(n+n)])
                              /nreps)
  # return the probability that all prisoners succeed in finding their number
  return(probability_all_succeed)
}





# Here, we estimate the individual and joint success probabilities for each#
# strategy for when n = 5 and n = 50.

num_trials = 10000

# one prisoner succeeding in finding their number
print("Estimating the probability of a single prisoner succeeding:")
n = 5
print("n = 5")
cat("Strategy 1 has probability of ", pone(n, 3, 1,num_trials), ".",sep="")
cat("Strategy 2 has probability of ", pone(n, 3, 2,num_trials), ".",sep="")
cat("Strategy 3 has probability of ", pone(n, 3, 3,num_trials), ".",sep="")

n = 50
print("n = 50")
cat("Strategy 1 has probability of ", pone(n, 34, 1,num_trials), ".",sep="")
cat("Strategy 2 has probability of ", pone(n, 34, 2,num_trials), ".",sep="")
cat("Strategy 3 has probability of ", pone(n, 34, 3,num_trials), ".",sep="")


# all prisoners succeeding in finding their number
print("Estimating the probability of all prisoners succeeding:")
n = 5
print("n = 5")
cat("Strategy 1 has probability of ", unlist(pall(n,1,num_trials)[1]), sep="")
cat("Strategy 2 has probability of ", unlist(pall(n,2,num_trials)[1]), sep="")
cat("Strategy 3 has probability of ", unlist(pall(n,3,num_trials)[1]), sep="")

n = 50
print("n = 50")
cat("Strategy 1 resulted in probability of ", pall(n,1,num_trials), ".",sep="")
cat("Strategy 2 resulted in probability of ", pall(n,2,num_trials), ".",sep="")
cat("Strategy 3 resulted in probability of ", pall(n,3,num_trials), ".",sep="")




# In this section, we elaborate why the results are surprising.

# From running the simulations, we are able to see that one of the three 
# strategies works surprisingly - strategy 1 which gives an approximate 31% 
# chance of all prisoners succeeding in finding their numbers.  We found this
# surprising because we know that the probability of one prisoner succeeding is
# 0.5. Say if n = 50, and if we were to think about this naively, to find the 
# probability all prisoners succeed, we would do the following calculation:
# P(success) = (0.5)^100 (where n = 50),  which would give us an extremely
# extremely low probability (almost zero). That is why it is surprising to find
# that there is a strategy that gives 31% chance of all prisoners succeeding.
# When we calculate the probability using the 3rd strategy, the probability of 
# each prisoner succeeding is independent of each other. However, when we use 
# Strategy 1, that is no longer the case.

# Below we can see the difference in probabilities and number of successful
# prisoners between Strategy 1 and Strategy 3. We visualize the frequencies of 
# each number of successful prisoners below for strategies 1, 2, and 3 in the
# form of histograms.

# save results to plot later
strategy_1_all50 <- get_pall_success_vec(n,1,num_trials)
strategy_2_all50 <- get_pall_success_vec(n,2,num_trials)
strategy_3_all50 <- get_pall_success_vec(n,3,num_trials)

# PLEASE MAKE SURE TO OPEN PLOTS IN LARGE ENOUGH WINDOW IN ORDER TO SEE THEM 
# PROPERLY. NOTE: these figures might be overwritten by later figures, please
# rerun following 4 lines if you want to see the figures again

par(mfrow=c(1,3))

hist(strategy_1_all50, breaks=100, xlab="Successful prisoners count",
     main = "Histogram of strategy 1 (n=50)")

hist(strategy_2_all50, breaks=100, xlab="Successful prisoners count",
     main = "Histogram of strategy 2 (n=50)")

hist(strategy_3_all50, breaks=100, xlab="Successful prisoners count",
     main = "Histogram of strategy 3 (n=50)")

# We see that in strategy 1, either all prisoners win together or the majority
# loses together. Whereas in strategy 3, it is half and half (mostly between 
# 40-60 successful prisoners).




get_loop_len <- function(start_box, random_shuffle, boxes_is_visited){
  # computes the length of a loop in a random shuffling of cards in boxes given
  # a starting box and records which boxes have been visited during the loop.
  
  # This is a helper function
  
  # @param start_box (integer): the first box in the loop to open
  
  # @param random_shuffle (integer vector): a random shuffling of cards in boxes
  
  # @param boxes_is_visited (binary vector): vector of boxes visited so far, 
  # 0 or 1 at index x means box x has not or has been visited, respectively
  
  # @return (list): a list, where the first element is the length of the current
  # loop and the second is the updated vector of visited boxes that reflects all
  # the newly visited boxes during the loop 
  
  loop_len <- 1
  boxes_is_visited[start_box] <- 1
  current_box <- start_box
  # iterate while the loop is not closed (i.e. the card in the current box is
  # not the same as the number of the starting box)
  while (random_shuffle[current_box] != start_box){
    # update the current box number to the the number on the card in the
    # previous box
    current_box <- random_shuffle[current_box]
    # increase loop length by 1
    loop_len <- loop_len + 1
    # update visited boxes
    boxes_is_visited[current_box] <- 1
  }
  return (list(loop_len, boxes_is_visited))
  
}




dloop <- function(n, nreps){
  # computes the probability that at a loop of length from 1 to 2n occurs at
  # least once in a random shuffling of cards in boxes
  
  # @param n (integer): 2n is the number of prisoners/boxes/cards in each
  # simulation
  
  # @param nreps (integer): number of replicate simulations to run
  
  # @return probs (double vector): vector of length 2n, where probs[x] is the
  # probability of loop of length x occuring at least once
  
  
  # vector to store the number of simulations in which a loop of some length 
  # (from 1 to 2n) has been seen  
  loop_len_occurances <- rep(0,2*n)
  
  # run simulation nrep times and count occurances of each loop length
  for (i in 1:nreps){
    # binary vector to store if a loop length has occured in current simulation
    loop_len_occurs <- rep(0,2*n)
    # get a random suffling of cards from 1 to 2n 
    random_shuffle <- sample(1:(2*n))
    # binary vector to store which boxes have been visited by following the 
    # loops so far (as each box can appear in only one loop)
    boxes_is_visited <- rep(0,2*n)
    # choose a first box to open (any box would be fine to start with)
    start_box <- 1
    # while there are still some unvisited boxes, go through loops and compute 
    # their length
    while (sum(boxes_is_visited) != 2*n){
      # compute the length of current loop
      result <- get_loop_len(start_box, random_shuffle, boxes_is_visited)
      # update binary vector to reflect that current loop length has occured
      loop_len_occurs[unlist(result[1])] <-  1
      # update vector of visited boxes to relect the newly visited boxes during
      # the current loop
      boxes_is_visited <- unlist(result[2])
      # choose a new start box by choosing the first non-visited box
      start_box <- which(boxes_is_visited == 0)[1]
    }
    # at the end of current simulation, add the occurred loop lengths to all
    # the vector of all occurances 
    loop_len_occurances <- loop_len_occurances + loop_len_occurs
  }
  
  # convert counts to probabilites
  probs <- loop_len_occurances/nreps
  return (probs)
}

# Visualising probabilities for n=50
nreps = 10000
n = 50
probs <- dloop(n,nreps)

# PLEASE MAKE SURE TO OPEN PLOTS IN LARGE ENOUGH WINDOW IN ORDER TO SEE THEM 
# PROPERLY. NOTE: Following 2 figures overwrite previous ones, please rerun
# visualisation code if you want to see previous figures
par(mfrow=c(2,1))
barplot(probs*100,xlab="Loop length",ylab="Probability (%)",names.arg=c(1:100))
title("Probability of each loop length occuring at least once (n=50)")





longest_loop <- function(n, nreps){
  # A group of 2n prisoners will succeed with strategy 1 iff the longest loop
  # is of length at most n
  # This function computes the probability distribution of longest loops
  
  # @param n (integer): 2n is the number of prisoners/boxes/cards in each 
  # simulation
  
  # @param nreps (integer): number of replicate simulations to run
  
  # @return (double vector): vector of length 2n, where vector[x] is the
  # probability of loop of length x being the longest loop
  
  longest_loop_counts <- rep(0,2*n)
  # run simulation nrep times
  for (i in (1:nreps)){
    # get loop lengths that occur in a single simulation
    at_least_once_probs <- dloop(n, 1)
    # find longest loop (take the index of the last non-zero position in the
    # at_least_once_probs vector)
    x <- tail(which(at_least_once_probs!= 0), 1)
    # update longest loop count
    longest_loop_counts[x] <- longest_loop_counts[x] + 1
  }
  # return vector of probabilities
  return (longest_loop_counts/nreps)
}


data_longest_loop <- longest_loop(50, 10000)


# probability that longest loop is between 1 and 50 (aka. not longer than 50)
longest_50_prob <- sum(data_longest_loop[1:50])

cat("Probability that there is no loop longer than 50 is approximatelly ", 
    longest_50_prob*100, "%.")

cat("This matches the result of our earlier simulation!")

# Visualising longest loop length probability distribution
# use different colours for values <=50 and >50
# PLEASE MAKE SURE TO OPEN PLOTS IN LARGE ENOUGH WINDOW IN ORDER TO SEE
# THEM PROPERLY
colours <- c(rep("green3",50), rep("red",50)) 
barplot(data_longest_loop*100, names.arg = c(1:100), col= colours, 
        xlab="Length of longest loop (L)", ylab="Probability (%)")
title("Probability distribution of longest loop length (n=50)")
text(x=30,y=1.7,labels="P(L<=50) = 0.31", col="darkgreen")
text(x=90,y=1.7,labels="P(L>50) = 0.69", col="red")

