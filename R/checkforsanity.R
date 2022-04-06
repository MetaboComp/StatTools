#' @param data The dataset used for checking. It could be a dataframe or matrix
#' @param check "NA","NaN","negative",0,infinite. The default value is NA
#' @param output "number","percentage". The default value

#' @return A table: The first row is how many values is "NA","NaN","negative",0,infinite. The second row is how many columns has this value
#' @export
#' @examples
checkforsanity<-function(data,
                         check=c("NA","NaN","negative","0","infinite"),
                         output=c("number","percentage")){
  if(missing(output)){output="number"}
  if(missing(check)){check="NA"}
  if(check=="NA"){
    if(output=="number"){result=table(apply(data,2,function(x){sum(is.na(x))}))}
    if(output=="percentage"){result=table(apply(data,2,function(x){sum(is.na(x))/nrow(data)}))
    cat("The first row is how many values is NA in individual columns.")
    cat("The second row is how many such individual columns have such values")}
  }
  if(check=="NaN"){
    if(output=="number"){result=table(apply(data,2,function(x){sum(is.nan(x))}))}
    if(output=="percentage"){result=table(apply(data,2,function(x){sum(is.nan(x))/nrow(data)}))
    cat("The first row is how many values is NaN in individual columns.")
    cat("The second row is how many such individual columns have such values")}
  }
  if(check=="negative"){
    if(output=="number"){result=table(apply(data,2,function(x){sum(x<0)}))}
    if(output=="percentage"){result=table(apply(data,2,function(x){sum(x<0)/nrow(data)}))
    cat("The first row is how many values is negative in individual columns.")
    cat("The second row is how many such individual columns have such values")}
  }
  if(check=="0"){
    if(output=="number"){result=table(apply(data,2,function(x){sum(x==0)}))}
    if(output=="percentage"){result=table(apply(data,2,function(x){sum(x==0)/nrow(data)}))
    cat("The first row is how many values is 0 in individual columns.")
    cat("The second row is how many such individual columns have such values")}
  }
  if(check=="infinite"){
    if(output=="number"){result=table(apply(data,2,function(x){sum(is.infinite(x))}))}
    if(output=="percentage"){result=table(apply(data,2,function(x){sum(is.infinite(x))/nrow(data)}))
    cat("The first row is how many values is infinite in individual columns.")
    cat("The second row is how many such individual columns have such values")}
  }
  return(result)
}