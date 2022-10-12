# Statistical Programming Coursework 2

# Members and contributions ---------------------------------------------------------------
## 1. Stefi Tirkova
## 2. Passara Chanchotisatien

# Code description



# Question 1 ------------------------------------------------------------------------------

# This question contains code for the function pone, which takes in n, k, strategy,
# and nreps as arguments and returns the probability of one prisoner succeeding in
# finding their number. k (integer) is the prisoner number and strategy (integer)
# indicates which strategy (an integer 1 to 3) to estimate the probability for. nreps
# is the amount of times to run the simulation. 


pone <- function(n, k, strategy, nreps) {
  twon <- as.integer(n+n) # we store 2n into a variable and cast it into an int
  
  # declare variable success_count which will be used to count how many times the prisoner
  # succeeded in finding their number
  success_count <- 0
  
  # declare a variable prisoner_num and make it equal to k, which is the number of the
  # prisoner that was passed into the function. This is just to give the variable a
  # more descriptive name.
  prisoner_num <- k
  
  # strategy 3 (aka the naive method) is separated because strategy 1 and 2 are similar
  # and uses a lot of the same code while strategy 3 is completely different.
  # if strategy 3 is selected
  if (strategy == 3) {
    # we loop / simulate this nreps times
    for (i in 1:nreps) {
      # declare card_number to be a vector containing unique random elements from 1 to 2n
      # this represents the numbers written on the cards inside the boxes
      card_number <- sample(1:twon)
      # since this strategy is opening n boxes completely randomly, we generated a vector
      # which randomly selects n elements (no duplicates) from card_number
      box_opened_hist = sample(card_number, n)
      # then we check whether the prisoner number is inside box_opened_hist
      if (prisoner_num %in% sample(card_number,n)) {
        # if prisoner_number is inside the vector box_opened_hist, then that means the
        # prisoner succeeded in finding their number
        # we increment success_count by 1
        success_count <- success_count + 1
      }
    }
  }
  # we group strategy 1 and 2 because they are very similar and there are only a few
  # lines of code different between the two strategies
  # else in the case where strategy is 1 or 2: 
  else if (strategy == 1 || strategy == 2){
    # similarly, we loop / simulate this nreps times
    for(i in 1:nreps){
      # we declare card_number to be a vector containing unique random numbers from 1-2n
      card_number <- sample(1:twon)
      # we declare a variable boxes_opened which is the number of boxes opened
      boxes_opened <- 0
      # if strategy = 1 we do the following:
      if (strategy == 1) {
        # Assign the current_box_num variable to equal the prisoner_num (in this case,
        # the current_box_num is the first box right now)
        current_box_num <- prisoner_num 
      } # if strategy = 2, we do the following:
      # the only difference between strategy 1 and strategy 2 is that strategy 2 starts by
      # choosing a randomly selected box rather than a box labeled with the same number as
      # their prisoner number
      else if (strategy == 2) {
        # prisoner starts from a randomly selected box in strategy 2
        # assign current_box_num to be a random number from 1 to 2n
        current_box_num <- sample(twon, 1)
      }
      # while the number of boxes opened (boxes_opened) is less than n,
      # we do the following:
      while(boxes_opened < n) {
        # check whether the number on the card inside the current box opened is the same as
        # the prisoner number
        if(card_number[current_box_num] == prisoner_num) {
          # if the number written on the card inside the box opened is the same as the
          # prisoner number, we increment success_count by 1. This means that the prisoner
          # has succeeded in finding their number.
          success_count <- success_count + 1
          # we can exit the loop as they no longer need to look for their number
          break
        }
        # if the number on the card of the box opened does not equal to the prisoner number,
        # we increment boxes_opened by 1.
        boxes_opened = boxes_opened + 1
        # The current_box_num is assigned to the number on the card (following the strategy).
        current_box_num <- card_number[current_box_num]
      }
    } 
  }
  
  # return the probability of a single player succeeding in finding their number
  # which is the number of successes divided 
  return((success_count / nreps))
}





# Question 2 ------------------------------------------------------------------------------

# This question contains code for the function pall, which takes in n, strategy,
# nreps as arguments and returns the probability of all prisoners succeeding in
# finding their number.

# certain comments explaining the reasoning behind the code or variables will be
# ommitted as much of the concept is the same as in pone

pall <- function(n, strategy, nreps) {
  # we declare a vector called success_vec of length nreps. It will be used to
  # store how many prisoners succeeded in finding their number in each trial.
  success_vec <- rep(0,nreps)
  # declare twon to equal 2n
  twon <- as.integer(n+n)
  
  # in strategy 3, prisoners open the boxes randomly
  # if strategy = 3 we do the following:
  if(strategy == 3) {
    # we loop / simulate this nreps times
    for(i in 1:nreps){
      # create a vector which stores the numbers written on the card in each box
      card_number <- sample(1:twon)
      # declare a variable num_pris_success which stores the number of prisoners
      # that succeeded in finding their number in each trial
      num_pris_success <- 0
      # now wee loop through 1 to 2n prisoners
      for(prisoner_num in 1:twon){
        # create a vector containing unique randomly generated numbers 1 to 2n
        box_opened_hist <- sample(card_number, n)
        # if the prisoner number is in box_opened_hist, then that means they have
        # succeeded in finding their number
        if (prisoner_num %in% sample(card_number,n)) {
          # we increment num_pris_success by 1
          num_pris_success <- num_pris_success + 1
        }
      }
      # at the end of each trial, we store the number of successful prisoners of
      # the trial inside success_vec.
      success_vec[i] <- sum(num_pris_success)
    }
  } else if (strategy == 1 || strategy == 2) {
    for(i in 1:nreps){
      card_number <- sample(1:twon)
      num_pris_success <- 0
      for(prisoner_num in 1:twon){
        boxes_opened = 0
        if (strategy == 1) { current_box_num <- prisoner_num }
        else if (strategy == 2) { current_box_num <- sample(twon, 1) }
        while(boxes_opened<n){
          if(card_number[current_box_num] == prisoner_num) {
            num_pris_success <- num_pris_success + 1
            break
          }
          boxes_opened <- boxes_opened + 1
          current_box_num <- card_number[current_box_num]
        }
      }
      success_vec[i] <- num_pris_success
    } 
  }
  
  probability_all_succeed <- (length(success_vec[success_vec == twon])/nreps)*100
  return(probability_all_succeed)
}



# Question 3 ------------------------------------------------------------------------------

# Here, we estimate the individual and joint success probabilities for each strategy
# for when n = 5 and n = 50.

num_trials = 100000

# one prisoner succeeding in finding their number
print("Estimating the probability of a single prisoner succeeding:")
n = 5
print("n = 5")
cat("Strategy 1 resulted in a success rate of ", pone(n, 3, 1,num_trials), ".",sep="")
cat("Strategy 2 resulted in a success rate of ", pone(n, 3, 2,num_trials), ".",sep="")
cat("Strategy 3 resulted in a success rate of ", pone(n, 3, 3,num_trials), ".",sep="")

n = 50
cat("Strategy 1 resulted in a success rate of ", pone(n, 34, 1,num_trials), ".",sep="")
cat("Strategy 2 resulted in a success rate of ", pone(n, 34, 2,num_trials), ".",sep="")
cat("Strategy 3 resulted in a success rate of ", pone(n, 34, 3,num_trials), ".",sep="")


# all prisoners succeeding in finding their number
print("Estimating the probability of all prisoners succeeding:")
n = 5
cat("Strategy 1 resulted in a success rate of ", pall(n,1,num_trials), "%.",sep="")
cat("Strategy 2 resulted in a success rate of ", pall(n,2,num_trials), "%.",sep="")
cat("Strategy 3 resulted in a success rate of ", pall(n,3,num_trials), "%.",sep="")

n = 50
cat("Strategy 1 resulted in a success rate of ", pall(n,1,num_trials), "%.",sep="")
cat("Strategy 2 resulted in a success rate of ", pall(n,2,num_trials), "%.",sep="")
cat("Strategy 3 resulted in a success rate of ", pall(n,3,num_trials), "%.",sep="")


# Question 4 ------------------------------------------------------------------------------

# In this section, we elaborate why the results are surprising.

# Question 5 ------------------------------------------------------------------------------



# Question 6 ------------------------------------------------------------------------------



