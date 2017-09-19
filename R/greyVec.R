#' Return a greyscale vector corresponding to values in vector (useful for plotting colors according to values)
#'
#' @param xCol A vector with numerical values
#' @param maxGrey Max value for greyscale (defaults to 85)
#' @param minGrey Min value for greyscale (defaults to 10)
#'
#' @return A character vector with greyscale names
#' @export
greyVec=function(xCol,maxGrey=85,minGrey=10) {
  x.col=minGrey+round(maxGrey*((max(xCol)-xCol)/(diff(range(xCol)))))
  colSc=paste("gray",x.col,sep="")
}
