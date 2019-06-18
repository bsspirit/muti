#' Calculate the mutual information between two vectors.
#'
#' \code{mutual_info} calculates the mutual information between two vectors. It
#'   is a helper function for \code{muti}.
#'
#' @param su An n x 2 \code{matrix} containing two discrete vectors.
#' @param normal A logical indicator as to whether the mutual information
#'   should be normalized to [0,1].
#'
#' @return A scalar of class `numeric` of the (possibly normalized) mutual
#'   information.
#'
#'
#' @importFrom philentropy H JE
#'
mutual_info<-function(su,normal){
  n<-nrow(su)
  px<-table(su[,1])/n
  py<-table(su[,2])/n
  
  hX<-H(px)
  hY<-H(py)
  
  tf<-table(su[,1],su[,2])
  pXY<-prop.table(tf)
  hXY<-JE(pXY)
  
  MI<-hX+hY-hXY
  if(normal){
    return(MI/sqrt(hX * hY))
  }else{
    return(MI)
  }
}
