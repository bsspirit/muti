#' Estimate the transition matrix for computing the critical threshold values on
#' mutual information.
#'
#' \code{mutual_info} is a helper function for estimating the transition matrix
#' used in creating resampled vectors for the (1 - alpha)\% critical threshold
#' value on the mutual info.
#'
#' @param x A \code{vector} of values.
#' @param n_bins The number of bins for the entropy calculation.
#'
#' @return A \code{list} with the following components:
#' \describe{
#' \item{\code{xn}}{An [n x 2] matrix of the original and discretized vectors.}
#' \item{\code{MM}}{Transition probability matrix from bin-i to bin-j.}
#' }
#'
#' @importFrom stats runif aggregate
#'
transM<-function(x,n_bins){
  x[is.na(x)] <- runif(length(x[is.na(x)]), min(x, na.rm = TRUE),max(x, na.rm = TRUE))
  n<-length(x)
  xn<-cbind(x,hin=cut(x,7))

  hin<-xn[,2]
  a<-data.frame(id=1:(n-1),a=hin[1:(n-1)])
  b<-data.frame(id=1:(n-1),b=hin[2:n])
  ab<-merge(a,b,by="id",all=TRUE)
  ab$v<-1
  abm<-aggregate(v~a+b,data=ab,sum)
  
  MM <- matrix(0, n_bins, n_bins)
  apply(abm,1,function(row){
    MM[row[1],row[2]]<<-row[3]
    invisible()
  })
  
  MN<-apply(MM,1,function(row){
    s<-sum(row)
    if(s>0){
      row/s
    }else{
      rep(1/n_bins,n_bins)
    }
  })
  MN<-t(MN)
  
  return(list(xn = xn, MM = MN))
} ## end function
