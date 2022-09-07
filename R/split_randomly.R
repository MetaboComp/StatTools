#' @param X a dataframe
#' @param Y a vector
#' @param k k fold

#' @export
#' @return split
split_randomly<-function(X,Y,k){
  Y<-as.data.frame(Y)
  data<-cbind(X,Y)
  n=nrow(data)
  remains=n%%k
  split_plus1<-split(data, sample(rep(1:(k+1),times=c(rep(n%/%k,k),remains))))
  for(i in 1:remains)
  {split_plus1[[i]]<-rbind(split_plus1[[i]],split_plus1[[k+1]][i,])}
  split_samples<-list()
  for (j in 1:k){
    split_samples[[j]]<-split_plus1[[j]]
  }
  X_split<-list()
  Y_split<-list()
  for (j in 1:k){
    X_split[[j]]<-split_samples[[j]][,-ncol(split_samples[[k]])]
    Y_split[[j]]<-split_samples[[j]][,ncol(split_samples[[k]])]
    
  }
  split<-list()
  split$X<-X_split
  split$Y<-Y_split
  return(split)
}