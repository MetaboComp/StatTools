#' @param X a vector of names
#' @param oldname_format a vector,what you want to change. 
#' @param newname_format a vector, what you want to change to.
#' @param game a vector of different operations
#' @export
#' @return X_new new vector of names
#' 
string_game<-function(X,
                      oldname_format,
                      newname_format,
                      game=c("grep","substitute","split","merge","detect","detect_subst")
){
  if(!game%in%c("grep","substitute","split","merge","detect","detect_subst")){
    stop("Do not support this method")
  }
  
  
}
