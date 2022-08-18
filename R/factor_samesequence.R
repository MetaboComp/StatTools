#' This function is to check for overlapping of values or dataframe columns in different objects.
#' You could see this as a venn diagram but in nicer text version. It does not have the limitation of venn diagram that only at most 5-6 object can be shown together
#' 
#' 
#' 
#' @param X a factor variable
#' @return X_factor
#' @export
#' 
#' 
factor_samesequence<-function(X){
  XX<-X
  X<-as.character(X)
  X_levels<-vector()   
  if(length(X_levels)==0){X_factor<-c(X_levels,X[1])} 
  for(i in 1:length(X)){
  if(X[i] %in% X_levels){X_levels<-X_levels}
    else{X_levels<-c(X_levels,X[i]) }
  }  
  X_factor<-factor(XX,levels=X_levels)
  return(X_factor)
}