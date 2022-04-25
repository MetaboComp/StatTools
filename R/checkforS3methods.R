#' checkforS3methods
#' @param func A data frame with variables with class "numeric", "factor", "logical","character","ordered factor"
#' @return A list of S3 methods cthat can match this function
#' @export

checkforS3methods<-function(func){
  result<-list()
  allname<-vector()
  require(utils)
  method<-methods(paste(func))

  for(i in 1:length(methods(paste(func))))
    
    try({require(utils)
      f <- eval(substitute(getAnywhere(method[i])$objs[[1]], list(fn = method[i])))
      #c(fn, deparse(args(f))[1])
      allname<-c(allname,c(paste(method[i], ":",deparse(args(f))[1])))
      #cat(fn, ":\n\t", deparse(args(f))[1], "\n")   ##Turn unevaluated expressions into character strings.
    },
    silent = TRUE)
  result$allname<-allname
  result$methods<-methods(paste(func))
  return(result)
}