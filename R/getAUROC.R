#' get miss classification from classification analysis
#'
#' @param actual Vector of actual classifications of samples
#' @param predicted Vector of predicted classifications of samples
#'
#' @return Balanced Error Rate (BER)
#' @export
getAUROC<-function (actual, predicted){
  if(length(actual)!=length(predicted)){stop("Must have same length")}
  if (!is.factor(actual)) {actual = factor(actual)}
  if (!is.factor(predicted)) {predicted = factor(predicted,levels = levels(actual))}
  auc <- vector(length=length(levels(actual)))

  for (cl in 1:length(levels(actual))) {
    auc[cl] <- roc(as.numeric(actual == (levels(actual)[cl])),
                   as.numeric(predicted==(levels(predicted)[cl])),
                      quiet = TRUE)$auc

  }
  return(auc)
}