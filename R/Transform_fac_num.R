#'Transform all variable class as either numeric or factor.
#'Make the sequence of the levels for each factor variable become the "default sequence".
#'
#'Example: a<-factor(c(1,2,3)). When levels(a), the out put is 1,2,3. This is the "default sequence" we want.
#'         a<-factor(c(1,2,3),levels=c("3","1","2")). When levels(a), the out put is 3,1,2. This is not "default sequence".
#'         a<-Transform_fac_num(factor(c(1,2,3),levels=c("3","1","2"))). When levels(a), the output is 1,2,3.
#' @param X A data frame with variables with class "numeric", "factor", "logical","character","ordered factor"
#' @return A list with 2 items: A dataframe with all variable class as either numeric or factor; Names of ordinal factors that is originally classified as "ordered factor".
#' @export
#' @examples
#'install.packages("remotes")
#'library(remotes)
#'install_gitlab('CarlBrunius/MUVR')
#'factor_1<-factor(c(rep("2",8),rep("3",8),rep("4",8),rep("1",5)),levels=c("3","1","2","4"))
#'factor_2<-as.character(c(rep("f",5),rep("e",8),rep("h",5),rep("g",11)))
#'d<-cbind(Xotu[,1:7],factor_1,factor_2,Yotu)
#'e=Transform_fac_num(d)$dataframe
#'
#'
Transform_fac_num<-function(X){
  if(!is.data.frame(X)){X<-as.data.frame(X)
  }else{
    X<-as.data.frame(X)
    names=colnames(X)
    ordinal_factor_names=vector()

    for(i in 1:ncol(X)){
      if(class(X[,i])%in%c("numeric","integer")){
        X[,i]=as.numeric(X[,i])
      }
      if(class(X[,i])%in%c(c("ordered","factor"))){
        ordinal_factor_names=c(ordinal_factor_names,colnames(X)[i])
        X[,i]=factor(as.character(X[,i]),order=F)

      }
      if(class(X[,i])%in%c("logical","character")){
        X[,i]=factor(as.character(X[,i]),order=F)
        #X=transform(X,colnames(X)[i]=as.factor(colnames(X)[i]))
      }
      if(class(X[,i])%in%c("factor")){
        X[,i]=factor(as.character(X[,i]),order=F)
      }

    }
    colnames(X)=names
  }
  result=list()
  result$dataframe=X
  result$ordinal_factor_names=ordinal_factor_names
  return(result)

}
