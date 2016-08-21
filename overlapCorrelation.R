#This functions calculates the correlations only on "overlaps" which means either one of the variables has to have a 1 (or TRUE)

overlapCor<-function(x){
  output<-matrix(
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
