# Analytical solution for linear interpolation (stable)
# Suppose we have some (x, y) data. After a linear interpolation find all x such that the value of the interpolant equals y0.

## with default value y0 = 0, it finds all roots of the interpolant
RootLinearInterpolant <- function (x, y, y0 = 0) {
  if (is.unsorted(x)) {
    ind <- order(x)
    x <- x[ind]; y <- y[ind]
  }
  z <- y - y0
  ## which piecewise linear segment crosses zero?
  k <- which(z[-1] * z[-length(z)] < 0)
  ## analytically root finding
  xk <- x[k] - z[k] * (x[k + 1] - x[k]) / (z[k + 1] - z[k])
  xk
}

save(RootLinearInterpolant, file="root_interpolation_function.Rdata")

# A more complicated example and test.

set.seed(0)
  x <- sort(runif(10, 0, 10))
y <- rnorm(10, 3, 1)
y0 <- 2.5
x
y
xk <- RootLinearInterpolant(x, y, y0)
xk
#[1] 3.375952 8.515571 9.057991

plot(x, y, "l"); abline(h = y0, lty = 2)
points(xk, rep.int(y0, length(xk)), pch = 19)

newx1aL

test <- new_dataLx %>%
  dplyr::group_by(year) %>%
  ## if 1 threshold value and it's lower than the peak (ascending slope)
  dplyr::mutate(Low = if(length(newx1aL==1 && newx1aL < peakQL)){
    # sum the amount of time above threshold
    sum(Q >= newx1aL)/length(DateTime)*100
  }else{
    print("no")
  }

  newx1aL
  newx2aL
  time_statsLx
  
  sum(new_dataLx$Q <= newx1aL[1] & new_dataLx$Q >= newx1aL[2] & new_dataLx$Q <= newx1aL[3])/length(new_dataLx$DateTime)*100
  
  sum(new_dataLx$Q >= newx1aL[2])/length(new_dataLx$DateTime)*100
  
  time_statsLx <- new_dataLx %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(Low = sum(Q <= newx1aL[1] | Q >= newx1aL[2] & Q <= newx1aL[3])/length(DateTime)*100) %>%
    dplyr::mutate(Medium = sum(Q <= newx2aL)/length(DateTime)*100) %>%
    dplyr::mutate(High = sum(Q <= newx3aL)/length(DateTime)*100) %>%
    distinct(year, Low , Medium, High) %>%
    mutate(position="LOB")
  
  time_statsMx <- new_dataMx %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(Low = sum(Q <= newx1a)/length(DateTime)*100) %>%
    dplyr::mutate(Medium = sum(Q <= newx2a)/length(DateTime)*100) %>%
    dplyr::mutate(High = 0) %>%
    distinct(year, Low , Medium, High) %>%
    mutate(position="MC")
  
  head(new_data)
  
  ggplot(new_data, aes(x = DateTime, y=Q)) +
    geom_line(aes( group = variable, lty = variable)) +
    facet_wrap(~variable, scales="free_x", nrow=3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs(title = "F57C: Q- TS",
         y = "Q",
         x = "Time") #+ theme_bw(base_size = 15)
  
  
  time_statsLx 
  time_statsMx
  length(newx1aL)==1
  is.na(newx3a)
  length(newx3a[2])
  
  newx2a
  newx1a
  newx3a
  
  ifelse(length(newx3a)==2 && newx3a[1] > peakQM || newx3a[2] < peakQM, "true", "False")
  
  ifelse(length(newx1aL)==1, "True", "False")
  
time_statsLxx <- new_dataLx %>%
  dplyr::group_by(year) %>%
  ## if 1 threshold value and it's lower than the peak (ascending slope)
  dplyr::mutate(Low = if(length(newx1aL==1 && newx1aL < peakQL)){
    # sum the amount of time above threshold
    sum(Q >= newx1aL)/length(DateTime)*100
    ## if 1 threshold value and it's higher than the peak (descending slope)
  } else if (length(newx1aL==1 && newx1aL > peakQL)){
    # sum the amount of time below the threshold
    sum(Q <= newx1aL)/length(DateTime)*100
    ## if 2 threshold values and the first one is lower than the peak(positive parabol)
  } else if (length(newx1aL==2 && newx1aL[1] < peakQL)) { 
    # sum the amount of time above the first and below the 2nd threshold
    sum(Q >= newx1aL[1] & Q <= newx1aL[2])/length(DateTime)*100
    ## if 2 threshold values and the first one is higher OR the 2nd one is lower than the peak (negative parabol)
  } else if(length(newx1aL==2 && newx1aL[1] > peakQL || newx1aL[2] < peakQL )) {
    # sum the amount of time below the first and above the 2nd threshold
    sum(Q <= newx1aL[1] & Q >= newx1aL[2])/length(DateTime)*100
    ## if 3 threshold values and the 3rd one is higher then the peak (begins negative slope)
  } else if (length(newx1aL == 3 && newx1aL[3] > peakQL)) {
    # sum the amount of time below the first and above the 2nd threshold and below the 3rd
    sum(Q <= newx1aL[1] & Q >= newx1aL[2] & Q <= newx1aL[3])/length(DateTime)*100
    ## if 3 threshold values and the 1st one is lower then the peak (begins positive slope)
  } else if (length(newx1aL == 3 && newx1aL[1] < peakQL)) {
    # sum the amount of time above the first and below the 2nd threshold and above the 3rd
    sum(Q >= newx1aL[1] & Q <= newx1aL[2] & Q >= newx1aL[3])/length(DateTime)*100
  }) %>%
  distinct(year, Low)

time_statsLxx,
newx2aL

time_statsL_test
time_statsL_test <- new_dataLx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(Q <= newx1aL)/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(Q <= newx2aL[1] & Q >= newx2aL[2] & Q <= newx2aL[3])/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(Q >= newx3aL & Q <= newx3aL)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(Q <= newx1aL)/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(Q <= newx1aL[1] & Q >= newx1aL[2] & Q <= newx1aL[3])/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(Q >= newx3aL & Q <= newx3aL)/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="LOB")

ID01 = data.table::rleid(new_dataM, ifelse(is.na(newx1a[1]), "true", "false"))
ID01 

new_dataM

?rleid

new_dataM <- new_dataM %>%
  group_by(month, day, year, ID01 = data.table::rleid (ifelse(is.na(newx1a[1]), 0, ifelse(length(newx1a)==1 && newx1a < peakQM), ))) {
    0
  } else if (length(newx1a)==1 && newx1a < peakQM){
    # sum the amount of time above threshold
    Q >= newx1a
    ## if 1 threshold value and it's higher than the peak (descending slope)
  } else {
  
  }

low_eq <- Q >= newx1a
low_thresh <- function(x) {
  amount_of_time <- Q >= newx1a
  return(amount_of_time)
}
  