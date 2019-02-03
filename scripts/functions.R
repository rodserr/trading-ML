predict_tp <- function(data = data, tp = 0.015, sl = 0.02, h = 10){
  
  class <- c()
  for (d in 1:(nrow(data)-h)){
    
    buy <- data$close[d]
    
    # Goals
    goal_limit <- buy*( 1 + tp )
    goal_stop <- buy*( 1 - sl )
    
    # Higs and lows within the horizon
    highs <- data$high[(d+1):(d+h)]
    lows <- data$low[(d+1):(d+h)]
    
    # Check for limits triggered
    tests_high <- highs >= goal_limit
    tests_low <- lows <= goal_stop
    
    # Check the first trigger
    first_limit <- ifelse(any(tests_high == TRUE), min(which(tests_high == TRUE)), NA)
    first_stop <- ifelse(any(tests_low == TRUE), min(which(tests_low == TRUE)), NA)
    
    # Make Classification
    if(is.na(first_limit)) {
      react <- 'stay'
    } else {
      if (is.na(first_stop)){
        react <- 'buy'
      } else {
        react <- ifelse(first_limit < first_stop, 'buy', 'stay')
      }
    }
    
    class %<>% append(react)
  }
  
  # Attach class to data
  data %<>% cbind(class = c(class, rep(NA, h)))
  
  return(data)
}
