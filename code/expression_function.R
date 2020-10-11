### function to take expression

expression_Q <- function(limit, curve) {
  if(is.na(limit[1])) {
    
    expression_limit <- expression(Q < 0)
    ## 1a) if 1 threshold value and it's lower than the peak (ascending slope)
  } else if(length(limit)==1 && limit < curve){
    # sum the amount of time above threshold
    expression_limit <- expression(Q >= limit)
    
    ## 1b) if 1 threshold value and it's higher than the peak (descending slope)
  } else if (length(limit)==1 && limit > curve){
    # sum the amount of time below the threshold
    expression_limit <- expression(Q <= limit)
    
    ## 2a) if 2 threshold values and the first one is lower than the peak(positive parabol)
  } else if (length(limit)==2 && limit[1] < curve) { 
    # sum the amount of time above the first and below the 2nd threshold
    expression_limit <- expression(Q >= limit[1] & Q <= limit[2])
    
    ## 2b) if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
  } else if(length(limit)==2 && (limit[1] > curve || limit[2] < curve )) {
    # sum the amount of time below the first and above the 2nd threshold
    expression_limit <- expression(Q <= limit[1] & Q >= limit[2])
    
    ## if 3 threshold values and the 1st one is lower then the peak (begins negative slope)
  } else if (length(limit) == 3 && (limit[1] < curve && limit[2] < curve && limit[3] > curve) ||
             (limit[1] > curve && limit[2] > curve && limit[3] > curve)) {
    # sum the amount of time above the first and below the 2nd threshold and above the 3rd
    expression_limit <- expression(Q <= limit[1] | Q >= limit[2] & Q <= limit[3])
    
    ## if 3 threshold values and the 3rd one is higher then the peak (begins positive slope)
  } else if (length(limit) == 3 && (limit[1] < curve && limit[2] > curve && limit[3] > curve) ||
             (limit[1] > curve && limit[2] > curve && limit[3] < curve)) {
    # sum the amount of time below the first and above the 2nd threshold and below the 3rd
    expression_limit <- expression(Q >= limit[1] & Q <= limit[2] | Q >= limit[3])
    
    ## 4a) if 4 threshold values and all are higher than the peak (begins positive slope)
  } else if (length(limit) == 4 && limit[1] < curve) {
    # sum the amount of time above the first and below the 2nd threshold or above the 3rd and below 2nd
    expression_limit <- expression(Q >= limit[1] & Q <= limit[2] |  Q >= limit[3] & Q <= limit[4])
    
    ## 4b) if 4 threshold values and all are higher than the peak, the 1st one and 2nd are lower, or all are lower  (begins negative slope)
  } else if (length(limit) == 4 && (limit[1] < curve && limit[2] < curve && limit[3] < curve && limit[4] < curve) || (limit[1] > curve 
                                                                                                                               && limit[2] > curve && limit[3] > curve && limit[4] > curve)  || (limit[2] < curve && limit[3] > curve)) {
    # sum the amount of time above the first and below the 2nd threshold and above the 3rd
    expression_limit <- expression(Q <= limit[1] & Q >= limit[2] |  Q <= limit[3] & Q >= limit[4])
  }
  return (expression_limit)
}

expression_Q(newx1a, peakQM)

save(expression_Q, file="expression_Q_limit_function.RData")

