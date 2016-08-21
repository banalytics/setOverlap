#iteration wrapper is a top level function that returns the frequency for each combination (and number of sets),  it only needs with TRUE / FALSE flags for each set affinity
iterationWrapper <- function(data = data.frame()){
  variableNames <- names(data)
  combinationFrame <- as.data.frame(matrix(ncol = 2, nrow = 0))
  #k represents total number of variables
  k <- length(variableNames)
  queryString <- ""
  combinationFrame <- iteration(i = 1, k = k, data = data, variableNames, queryString = "", combinationFrame = combinationFrame)
  names(combinationFrame) <- c("combination", "frequency")
  combinationFrame[, "frequency"] <- as.numeric(as.character(combinationFrame[, "frequency"]))
  return(combinationFrame)
}

#iteration is a recursive function that requires the same input as iterationWrapper,  all other inputs are detected in the wrapper part
iteration <- function(i = 1, k = k, data = data.frame(), variableNames, queryString = "", combinationFrame){
  # loop goes over all variables adding permutations through recursive calls
  for(j in i:k){
    combinationFrame <- rbind(combinationFrame, cbind(paste(queryString, if(queryString!= ""){", "}, variableNames[j], sep = ""), nrow(subset(data, data[, variableNames[j]] == TRUE))))
    if((i <= (k - j + (k - 2))) & (j != k)){combinationFrame <- iteration(i = j + 1, k = k, data = subset(data, data[, variableNames[j]] == TRUE), variableNames, queryString = paste(queryString, if(queryString!= ""){", "}, variableNames[j], sep = ""), combinationFrame = combinationFrame)}    
  }
  return(combinationFrame)
}