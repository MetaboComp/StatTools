#'factor_samesequence
#' 
#' 
#' @param X a factor variable or a character variable
#' @param level levels if want to manually set
#' @param sequence default as F, if T normal factor
#' @return X_factor
#' @export
#' 
#' 
factor_samesequence<-function(X,level,sequence){
  
  if(is.character(X)){
    if(missing(level)){
      if(missing(sequence)){sequence=F}
      if(sequence==F){
        X_factor<-factor(X,levels=unique(X))
      }
      if(sequence==T){
        X_factor<-factor(X,levels=unique(X))
      }
    }
    if(!missing(level)){X_factor<-factor(X,levels=unique(X))}
    return(X_factor)
  }  
  
  
  
  
  if(is.factor(X)){
  if(missing(level)){
  if(missing(sequence)){sequence=F}
  if(sequence==F){
  XX<-X
  X<-as.character(X)
  X_levels<-vector()   
  if(length(X_levels)==0){X_factor<-c(X_levels,X[1])} 
  for(i in 1:length(X)){
  if(X[i] %in% X_levels){X_levels<-X_levels}
    else{X_levels<-c(X_levels,X[i]) }
  }  
  X_factor<-factor(XX,levels=X_levels)
  }
  if(sequence==T){
    X_factor=factor(X,levels=unique(X))
  }
  }
  if(!missing(level)){X_factor<-factor(X,levels=level)}
  return(X_factor)
  }  
}