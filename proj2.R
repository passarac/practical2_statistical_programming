# Statistical Programming Coursework 2

# Members and contributions


# Code description



# Question 1

# This question contains code for the function pone, which takes in n, k, strategy,
# and nreps as arguments and returns the probability of one prisoner succeeding in
# finding their number. k (integer) is the prisoner number and strategy (integer)
# indicates which strategy (an integer 1 to 3) to estimate the probability for. nreps
# is the amount of times to run the simulation. 


# Question 2

# This question contains code for the function pall, which takes in n, strategy,
# nreps as arguments and returns the probability of all prisoners succeeding in
# finding their number.

pall <- function(n, strategy, nreps) {
  success_vec <- rep(0,nreps)
  twon <- as.integer(n+n)
  
  if(strategy == 1) {
    for(i in 1:nreps){
      card_number <- sample(1:twon)
      num_pris_success <- 0
      for(prisoner_num in 1:twon){
        boxes_opened = 0
        current_box_num <- prisoner_num
        while(boxes_opened<n){
          if(card_number[current_box_num] == prisoner_num) {
            num_pris_success <- num_pris_success + 1
            break
          }
          boxes_opened = boxes_opened + 1
          current_box_num <- card_number[current_box_num]
        }
      }
      success_vec[i] <- num_pris_success
    } 
  } else if (strategy == 2) {
    for(i in 1:nreps){
      card_number = sample(1:twon)
      num_pris_success <- 0
      for(prisoner_num in 1:twon){
        if(card_number[prisoner_num] == prisoner_num) {
          num_pris_success <- num_pris_success + 1
        } else {
          box_opened_hist = sample(card_number, n-1)
          if (prisoner_num %in% box_opened_hist) {
            num_pris_success <- num_pris_success + 1
          } 
        }
      }
      success_vec[i] = sum(num_pris_success)
    }
  } else if (strategy == 3) {
    for(i in 1:nreps){
      ticket = sample(1:twon)
      num_pris_success <- 0
      for(prisoner_num in 1:twon){
        box_opened_hist = sample(ticket, n)
        if (prisoner_num %in% sample(ticket,n)) {
          num_pris_success <- num_pris_success + 1
        }
      }
      success_vec[i] = sum(num_pris_success)
    }
  }
  
  probability_all_succeed <- (length(success_vec[success_vec == twon])/nreps)*100
  return(probability_all_succeed)
}



# Question 3

# Here, we estimate the individual and joint success probabilities for each strategy
# for when n = 5 and n = 50.

# one prisoner succeeding in finding their number
## n = 5


## n = 50


# all prisoners succeeding in finding their number
## n = 5
cat("Strategy 1 resulted in a success rate of ", pall(5,1,10000), "%.",sep="")
cat("Strategy 1 resulted in a success rate of ", pall(5,2,10000), "%.",sep="")
cat("Strategy 1 resulted in a success rate of ", pall(5,3,10000), "%.",sep="")

## n = 50
cat("Strategy 1 resulted in a success rate of ", pall(50,1,10000), "%.",sep="")
cat("Strategy 1 resulted in a success rate of ", pall(50,2,10000), "%.",sep="")
cat("Strategy 1 resulted in a success rate of ", pall(50,3,10000), "%.",sep="")

# Question 4



# Question 5



# Question 6