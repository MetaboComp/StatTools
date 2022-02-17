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
  result<-list()
  cor=NULL
  X=data.frame(X)
  Y=data.frame(Y)
  C=data.frame(C)
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
  result$cor<-cor
  return(result)
}
