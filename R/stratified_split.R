#' @param X a dataframe
#' @param Y a vector
#' @param k kfold
#' @export
#' @return split

stratified_split<-function(X,Y,k){
  library(caret)
  fold<-createFolds(y=Y, k = k)  ###stratified spliting
  data_folds<-list()
  for(i in 1:k){
    data_folds[[i]]<-data[fold[[i]],]
  }
  data$X<-data_folds
  data$Y<-fold
  return(data)
}
