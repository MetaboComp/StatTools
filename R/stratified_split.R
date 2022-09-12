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
    data_folds[[i]]<-X[fold[[i]],]
  }
  data<-list()
  data$X<-data_folds
  data$Y<-Y[fold]
  return(data)
}
