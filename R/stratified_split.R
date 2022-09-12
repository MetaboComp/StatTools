#' Stratified split
#' @param X a dataframe
#' @param Y a vector
#' @param k kfold
#' @export
#' @return split

stratified_split<-function(X,Y,k){
  library(caret)
  fold<-caret::createFolds(y=Y, k = k)  ###stratified spliting
  data_folds<-list()
  data<-list()
  data$Y<-list()
  for(i in 1:k){
    data_folds[[i]]<-X[fold[[i]],]
    
  }
  for(i in 1:k){
  data$Y[[i]]<-Y[fold[[i]]]
  }
  data$X<-data_folds
  
  return(data)
}
