# Lab 9
# Maclaurin Series

# function to calculate the maclaurin with provided values
functionMac <- function(f0, derivatives, numX, numN){
  i <- 1
  # start with f0
  fx <- f0
  # loop for summation
  while (i <= numN){
    # add calculation to fx total
    fx <- fx + ( (derivatives[i]/factorial(i)) * (numX ^ i) )
    i <- i + 1
  }
  
  # return calculated result
  return(fx)
}

# function to calculate the maclaurin of f(x) = 10 + e^x cos(x)
maclaurin <- function(){
  # ask for input
  numX <- readline(prompt = "Please enter the value of x: ")
  numX <- as.integer(numX)
  
  # f(0)
  f0 <- 11
  # first derivative = -e^x(sinx - cosx)
  fd1 <- 1
  # second derivative = -2e^x(sinx)
  fd2 <- 0
  # third derivative = -2e^x(sinx+cosx)
  fd3 <- -2
  # fourth derivative = -4e^x(cosx)
  fd4 <- -4
  # fifth derivative = fe^x(sinx = cosx)
  fd5 <- -4
  
  # call function to calculate, print result
  fx <- functionMac(f0, c(fd1,fd2,fd3,fd4,fd5), numX, 5)
  print(paste("Maclaurin series calculation when x =", numX, " and n = 5 is", fx), quote = FALSE)
  
  # create a data frame for maclaurin series from -5 <= x <= +5
  calcVals <- data.frame("x" = c(seq(from = -5, to = 5, by = 0.1)))
  
  # create a table for values to be printed
  valMatrix <- matrix(nrow = 15, ncol = 3)
  # create a vector for convergence values
  convergenceVec <- vector(length = nrow(calcVals))
  # loop through x values and caluclate
  i <- 1
  j <- 1
  k <- 1
  while (i <= nrow(calcVals)){
    # calculate the maclaurin observed values
    calcVals$fitted[i] <- functionMac(f0, c(fd1,fd2,fd3,fd4,fd5), calcVals$x[i], 5)
    # calculate the actual values
    calcVals$observed[i] <- (10 + ( exp(calcVals$x[i]) * cos(calcVals$x[i]) ) )
    # calculate the absolute error
    calcVals$absolute.error[i] <- abs(calcVals$observed[i] - calcVals$fitted[i])
    # calculate the relative error
    calcVals$relative.error[i] <- abs((calcVals$absolute.error[i] / calcVals$observed[i]) * 100)
    
    # check if this value will be printed and store
    if (calcVals$x[i] == 0 || (calcVals$x[i] >= -3.5 && calcVals$x[i] <= 3.5 && (calcVals$x[i] %% 0.5 == 0))){
      valMatrix[j,1] <- calcVals$x[i]
      valMatrix[j,2] <- calcVals$absolute.error[i]
      valMatrix[j,3] <- calcVals$relative.error[i]
      j <- j + 1
    }
    
    # check convergence
    if (calcVals$observed[i] == calcVals$fitted[i]){
      # store if converged
      convergenceVec[k] <- paste("(", calcVals$x[i], ",", calcVals$observed[i], ")")
      k <- k + 1
    }
    
    i <- i + 1
  }
  
  # plot the graph
  plot(calcVals$x, calcVals$observed , type="l", lty=2, col="black", main="Approximate vs Observed", sub="Observed in black dashed line vs. Approximate in solid blue",
       xlab="x values", ylab="f(x)")
  lines(calcVals$x, calcVals$fitted, col="blue")
  
  # print table of values
  colnames(valMatrix) <- c("x", "  Absolute Error", "  Relative Error")
  prmatrix(valMatrix, rowlab=rep("",15))
  #print(calcVals)
  
  # print convergence values
  convergenceVec <- convergenceVec[1:k-1]
  print("The two graphs converge at the following value(s)", quote = FALSE)
  print(convergenceVec, quote = FALSE)
}

# call function for calculation
maclaurin()

