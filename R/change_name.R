#' @param X a vector of names
#' @param oldname_format a vector,what you want to change. 
#' @param newname_format a vector, what you want to change to 
#' @param add_head logical, if you want to add something infromt of your new name format
#' @param add_tail logical, if you want to add something behind of your new name format
#' @param remove_rest T or F remove the part that is not substituted 
#' @export
#' @return X_new new vector of names

## be careful about the sequence about the name format you want to change. 
## if you want to change "£a£" to "€@€" in the first operation, then you may mess it up in the second operation where you want to change
## original "@" into "a", so you need to do this operation first.
change_name<-function(X,
                      oldname_format,
                      newname_format,
                      remove_rest=F,
                      add_head=F,
                      add_tail=F){
  
  if(add_head==T){head<-newname_format[1]
  newname_format<-newname_format[2:length(newname_format)]
  }
  if(add_tail==T){tail<-newname_format[length(newname_format)]
  newname_format<-newname_format[1:(length(newname_format)-1)]
  }
  
  library(stringr)
  if(add_head==F&add_tail==F){
  if(length(oldname_format)!=length(newname_format)){
    stop("They must be same length")}
  }
  
  if(missing(remove_rest)){remove_rest=F}
  if(remove_rest==F){
  X_new<-X
  
  for(i in 1:length(oldname_format)){
    X_new<-gsub(oldname_format[i], 
                newname_format[i], 
                X_new, 
                ignore.case=FALSE)
  }
  
  }
  
  if(remove_rest==T){
    X_new<-as.vector(rep("",length(X)))
    for(i in 1:length(oldname_format)){
      for(j in 1:length(X)){
        if(str_detect(X[j],oldname_format[i])){
          X_new[j]<-paste0(X_new[j],newname_format[i])
        }
      }
    }
    
  }
  
  if(add_head==T){
    for(i in 1:length(X_new)){
      X_new[i]<-paste0(head,X_new[i])
    }
  }
  if(add_tail==T){
    for(i in 1:length(X_new)){
      X_new[i]<-paste0(X_new[i],tail)
    }
  }
  return(X_new)
}