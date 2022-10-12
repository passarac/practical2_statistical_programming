old_pone <- function(n, k, strategy, nreps) {
  twon <- as.integer(n+n) # we store 2n into a variable and cast it into an int
  # declare variable success_count which will be used to count how many times the prisoner
  # succeeded in finding their number
  success_count <- 0
  # declare a variable prisoner_num and make it equal to k, which is the number of the
  # prisoner that was passed into the function. This is just to give the variable a
  # more descriptive name.
  prisoner_num <- k
  
  # if strategy 1 is selected:
  if(strategy == 1) {
    # we will loop this nreps times (which is how many times we want to simulate this)
    for(i in 1:nreps){
      # declare a vector called card_number which contains 2n elements of unique 
      # random values ranging from 1 to 2n. The index of each element represents
      # the box number and the value of the element represents the number on the card
      # inside each box.
      card_number <- sample(1:twon)
      # declare a variable 'boxes_opened' to keep track of how many boxes the prisoner
      # has opened so far.
      boxes_opened <- 0
      # In the first strategy, the prisoner will start by opening a box with the same number
      # as their prisoner number, therefore we assign current_box_num to equal the prisoner_num.
      current_box_num <- prisoner_num
      # As long as the number of boxes opened (boxes_opened) is less than n, the program will:
      while(boxes_opened < n) {
        # check whether the number on the card inside the current box opened is the same as
        # the prisoner number
        if(card_number[current_box_num] == prisoner_num) {
          # If it is the same, the we increment success_count by 1.
          success_count <- success_count + 1
          # We can now exit the loop. The prisoner has already found their number, and there is
          # no longer any need to continue.
          break
        }
        # if the number on the card of the box opened does not equal to the prisoner number,
        # we increment boxes_opened by 1. The current_box_num is assigned to the number on
        # the card.
        boxes_opened = boxes_opened + 1
        current_box_num <- card_number[current_box_num]
      }
    } 
  }
  
  # if strategy 2 is selected
  else if (strategy == 2) {
    # similarly, we loop/simulate this nreps times
    for(i in 1:nreps) {
      card_number <- sample(1:twon)
      if(card_number[prisoner_num] == prisoner_num) {
        success_count <- success_count + 1
      }
      else {
        box_opened_hist = sample(card_number, n-1)
        if (prisoner_num %in% box_opened_hist) {
          success_count <- success_count + 1
        } 
      }
    }
  }
  
  # if strategy 3 is selected
  else if (strategy == 3) {
    for (i in 1:nreps) {
      card_number <- sample(1:twon)
      box_opened_hist = sample(card_number, n)
      if (prisoner_num %in% sample(card_number,n)) {
        success_count <- success_count + 1
      }
    }
  }
  
  # return the probability of a single player succeeding in finding their number
  # which is the number of successes divided 
  return((success_count / nreps))
}
