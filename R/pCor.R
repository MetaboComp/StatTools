#' This function aims to construct one more flexible pcor()
#' @param X  A data frame of X variables (All variables needs to be numeric)
#' @param Y  A vector/dataframe of one Y variable (needs to be numeric)
#' @param C A data frame of covariates (All variables needs to be numeric)
#' @return A vector with adjusted partial correlations
#' @export
#'
#' @examples
#' install.packages(remotes)
#' library(remotes)
#' install_gitlab('CarlBrunius/MUVR@MUVR2')
#' library(MUVR)
#' X=Xotu[,1:4]
#' Y=as.numeric(Yotu)
#' C=Xotu[,5:7]
#'





pCor <- function(X,
                 Y,
                 C
) {
  if(is.null(dim(X))){
    Xframe=data.frame(X)
    rownamesX<-rownames(Xframe)
    colnamesX<-colnames(Xframe)
    X=as.numeric(X)
    X<-data.frame(X)
    colnames(X)<-colnamesX
    rownames(X)<-rownamesX
    } 
  if(is.null(dim(C))){
    Cframe=data.frame(C)
    rownamesC<-rownames(Cframe)
    colnamesC<-colnames(Cframe)
    C=as.numeric(C)
    C=data.frame(C)
    colnames(C)<-colnamesC
    rownames(C)<-rownamesC
  } 
   
   
  if(!is.null(dim(X))){
    colnamesX<-colnames(X)
    rownamesX<-rownames(X)
  for(i in 1:ncol(X))
  {X[,i]=as.numeric(X[,i])
  
  }
  X=data.frame(X)
  colnames(X)<-colnamesX
  rownames(X)<-rownamesX
  }
   if(!is.null(dim(C))){
     colnamesC<-colnames(C)
     rownamesC<-rownames(C)
     for(i in 1:ncol(C))
     {C[,i]=as.numeric(C[,i])
     
     }
      C=data.frame(C)
      colnames(C)<-colnamesC
     rownames(C)<-rownamesC
   }
  result<-list()
  cor=NULL
  
  
  if(is.null(dim(Y))){
  Yframe=data.frame(Y)
  colnamesY<-colnames(Yframe)
  rownamesY<-rownames(Yframe)
  Y=as.numeric(Y)
  Y=data.frame(Y)
  colnames(Y)<-colnamesY
  rownames(Y)<-rownamesY
  }
  else{
    colnamesY<-colnames(Y)
    rownamesY<-rownames(Y)
    for(i in 1:ncol(Y))
    {Y[,i]=as.numeric(Y[,i])
    
    }
    Y=data.frame(Y)
    colnames(Y)<-colnamesY
    rownames(Y)<-rownamesY
  }
 
  data=data.frame(X,Y,C)
  
  for (i in 1:ncol(X)){
    glmX <- glm(formula = as.formula(paste(colnames(X)[i],'~', paste(colnames(C),collapse="+"))),
                data=data)
    
    glmY <- glm(formula = as.formula(paste(colnames(Y),'~', paste(colnames(C),collapse="+"))),
                data=data)
    cor <- c(cor,
             cor(resid(glmX),
                 resid(glmY)))
  }
  cor<-data.frame(cor)
  rownames(cor)<-colnames(X)
  result$cor<-cor
  result$Y<-Y
  result$X<-X
  result$C<-C
  return(result)
}
