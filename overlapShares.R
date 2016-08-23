overlapShares <- function(x = data.frame()){
  output <- matrix(nrow=ncol(x), ncol=ncol(x))
  for(i in 1:ncol(x)){
    output[, i] <- unlist(
      lapply(
        X=1:ncol(x), FUN = function(X) sum(x[x[, i]==1, i]==x[x[, i]==1, X])/(nrow(x[x[, i] == 1, ]))
        )
    )
  }
  return(output)
}