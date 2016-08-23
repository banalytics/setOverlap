overlapShares <- function(x = data.frame()){
  output<-matrix(
    unlist(
      lapply(1:ncol(x),FUN = function(idx){
        unlist(
          lapply(1:ncol(x),FUN = function(idr){
          suppressWarnings(
            sum(x[x[, idx] == 1, idx] == x[x[, idx] == 1, idr]) / (nrow(x[x[, idx] == 1,]))
          )
          }
          )
          )
        }
        )
      )
    ,nrow=ncol(x),ncol=ncol(x)
    )
  row.names(output) <- colnames(x)
  colnames(output) <- colnames(x)
  return(output)
}

