#' Convert numeric vectors to symbolic vectors.
#'
#' \code{symbolize} converts numeric vectors to symbolic vectors. It is a helper
#'   function for \code{muti}.
#'
#' @param xy An n x 2 \code{matrix} or \code{data.frame} containing the two
#'   vectors of interest.
#'
#' @return An (n-2) x 2 \code{matrix} of integer symbols that indicate whether
#'   the i-th value, based on the i-1 and i+1 values, is a "trough" (=1),
#'   "decrease" (=2), "same" (=3), "increase" (=4), or "peak" (=5).
#'
symbolize<-function(xy){
  if(ncol(xy)!=2) {
    stop("xy must be an [n x 2] matrix \n\n", call. = FALSE)
  }
  
  idx<-2:(nrow(xy)-1)
  apply(xy,2,function(row){
    na<-which(is.na(row[idx]+row[idx-1]+row[idx+1]))
    n3<-which(row[idx]==row[idx-1] & row[idx]==row[idx+1])
    n5<-which(row[idx]>row[idx-1] & row[idx]>row[idx+1])
    n4<-which(row[idx]>row[idx-1] & row[idx]<=row[idx+1])
    n1<-which(row[idx]<row[idx-1] & row[idx]<row[idx+1])
    n2<-which(row[idx]<row[idx-1] & row[idx]>=row[idx+1])

    line<-rep(0,length(idx))
    line[na]<-NA
    line[n1]<-1
    line[n2]<-2
    line[n3]<-3
    line[n4]<-4
    line[n5]<-5
    c(NA,line,NA)
  })
}

