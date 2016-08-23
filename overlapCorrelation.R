#this functions calculates the correlations only on "overlaps" which means either one of the variables has to have a 1 (or TRUE)
#input is a data frame with set memberships
#output is a k*k matrix where k is the number of columns in the input data frame 

overlapCor <- function(x=data.frame()){
  output <- matrix(
    unlist(
      lapply(1:ncol(x),FUN = function(idx){
        lapply(1:ncol(x),FUN = function(idr){suppressWarnings(
          cor(x[x[,idx] == 1 | x[,idr] == 1,idr],x[x[,idx] == 1 | x[,idr] == 1,idx])
          )
          }
        )
      }
      )
    ),nrow = ncol(x),ncol = ncol(x)
  )
  output[is.na(output)] <- 0
  output <- 1 + output
  row.names(output) <- colnames(x)
  colnames(output) <- colnames(x)
  return(output)
}
