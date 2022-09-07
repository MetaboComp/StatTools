#' @param X a dataframe
#' @param Y a vector
#' @param ratio ratio
#' @export
#' @return data
split_traintest<-function(X,Y,ratio){
  indexes<-createDataPartition(y=Y,
                               times=1,
                               p=ratio)
  indexes<-indexes[[1]]
  train_X <- X[indexes,]
  test_X <- X[-indexes,]
  train_Y <- Y[indexes]
  test_Y <- Y[-indexes]
  data<-list()
  data$train_X<-train_X 
  data$test_X<-test_X 
  data$train_Y<-train_Y 
  data$test_Y<-test_Y
  return(data)
}