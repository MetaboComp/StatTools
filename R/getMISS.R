#' get miss classification from classification analysis
#'
#' @param actual Vector of actual classifications of samples
#' @param predicted Vector of predicted classifications of samples
#'
#' @return Balanced Error Rate (BER)
#' @export
getMISS<-function (actual, predicted){
  if(length(actual)!=length(predicted)){stop("Must have same length")}
  if (!is.factor(actual)) actual = factor(actual)
  len<-length(actual)
  miss<-0
  for(i in 1:length(actual)){
    if(actual[i]!=predicted[i]){
      miss=miss+1
    }
  }
  
  return(miss)
}