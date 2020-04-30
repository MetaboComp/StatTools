#' Switch values in vector
#'
#' @param vector Vector where you want to switch from one set of values to another
#' @param old Vector with "levels" in the old format
#' @param new Vector with "levels" in the new format
#'
#' @return Vector with new values
#' @export
switchNames=function(vector,old,new) {
  if(is.null(names(vector))) names(vector)=vector
  whichChange=vector%in%old
  vectorSubst=vector[whichChange]
  vectorSubst=sapply(vectorSubst,function(x) new[old==x])
  vector[whichChange]=vectorSubst
  return(vector)
}
