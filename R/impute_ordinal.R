#'imputation for 2 types of variables
#'1.numeric variables
#'2.factor variables without order: non_ordinal factor variables
#'
#'Preparation: To use this package, 3 steps need to be done beforehand:
#'
#'Step 1: Put ordinal variables and numeric variables in one data frame.
#'This is done manual because computer cannot decide which variables we vant to give order to.
#'!!! Note: the class of variables in this data frame should be only "factor" or "numeric". Use the Transform_fac_num in the same package.
#'Step 2: Code the ordinal factor variables properly!!!. For example, code it with alphabet or number that could show sequence.
#'
#'Step 3: Make sure that the sequence of the levels for each factor variable become the "default sequence". (Use the Transform_fac_num in the same package.)
#'Example: a<-factor(c(1,2,3)). When levels(a), the out put is 1,2,3. This is the "default sequence" we want.
#'         a<-factor(c(1,2,3),levels=c("3","1","2")). When levels(a), the out put is 3,1,2. This is not "default sequence".
#'         a<-Transform_fac_num(factor(c(1,2,3),levels=c("3","1","2"))). When levels(a), the output is 1,2,3.
#'
#' @param ordinal A dataframe with ordinal variables and numeric variables
#' @export
#' @return A dataframe with imputed values
#'
#' install.packages("remotes")
#' library(remotes)
#' install_gitlab('CarlBrunius/MUVR')
#' library(MUVR)
#' install_gitlab('YingxiaoYan/StatTools')
#' library(StatTools)
#' factor_1<-factor(c(rep("a",8),rep("b",8),rep("c",8),rep("d",5)))
#' factor_2<-as.character(c(rep("f",5),rep("e",8),rep("h",5),rep("g",11)))
#' d<-cbind(Xotu[,1:7],factor_1,factor_2,Yotu)
#' d[29,10]=NA
#' d[28,9]=NA
#' d[27,8]=NA
#' d[25,6]=NA
#' d[24,5]=NA
#' d[23,4]=NA
#' d[22,3]=NA
#' d[21,2]=NA
#' d[20,1]=NA
#' d[20,2]=NA
#' e=Transform_fac_num(d)$dataframe
#' f<-impute_ordinal(e)
#' # f is the imputed data frame of d

impute_ordinal<-function(ordinal){
##1 Make the sequence of the levels for each factor variable should be the "default sequence".  Save column names of the dataframe.
  ordinal<-Transform_fac_num(ordinal)$dataframe           ##it will not be in comment in the package

  ordinal=data.frame(ordinal)
  colnames_ordinal<-colnames(ordinal)
  rownames_ordinal<-rownames(ordinal)
##2. impute the data
  ############################################################################################################
  ###2.1 save the names of levels for ordinal data frame

  factor_location<-NULL
  levels_names_ordinal<-list()
  for(i in 1:ncol(ordinal)){
    if(class(ordinal[,i])=="factor"){
      levels_names_ordinal[[i]]<-levels(ordinal[,i])
      ordinal[,i]=as.numeric(ordinal[,i])
      factor_location=c(factor_location,i)}
  }
  ############################################################
  ###2.2 Imputation
  imputed_big_matrix=mvImpWrap(ordinal)
  imputed_ordinal=imputed_big_matrix

  #############################################################################################################
###3 When doing imputation, the names of levels for factor variables are lost, this step is to gives the name back
  ###3.1 give back the level names of ordinal factor variables
if(!is.null(factor_location)){
  for(i in 1:length(factor_location)){
    imputed_ordinal[,factor_location[i]]=round(imputed_ordinal[,factor_location[i]],0)
  }

  imputed_ordinal=data.frame(imputed_ordinal)
  factor_imputed_ordinal=imputed_ordinal
  for(i in 1:length(factor_location)){
    factor_imputed_ordinal[,factor_location[i]]=factor(factor_imputed_ordinal[,factor_location[i]])
  }
  ###the levels are transformed to numbers in character form

  relevel_matrix<-matrix(NA,
                         dim(factor_imputed_ordinal)[1],
                         dim(factor_imputed_ordinal)[2])
  for(i in 1:ncol(relevel_matrix)){
    if(class(factor_imputed_ordinal[,i])=="numeric"){
      relevel_matrix[,i]=factor_imputed_ordinal[,i]
    }
  }
  relevel_matrix=as.data.frame(relevel_matrix)

  for(i in 1:length(factor_location)){    ####factor variable numbers
    for(j in 1:length(levels(factor_imputed_ordinal[,factor_location[i]])))
      ##for each factor variable level numbers
    {
      for(k in 1:length(factor_imputed_ordinal[,factor_location[i]])){   ##observation numbers

      if(factor_imputed_ordinal[k,factor_location[i]]==levels(factor_imputed_ordinal[,factor_location[i]])[j]){
        relevel_matrix[k,factor_location[i]]=levels_names_ordinal[[factor_location[i]]][j]
      }
    }
    }

  }

  for(i in 1:ncol(relevel_matrix)){
    if(class(factor_imputed_ordinal[,i])=="numeric"){
      relevel_matrix[,i]=factor_imputed_ordinal[,i]
    }
    if(class(factor_imputed_ordinal[,i])=="factor"){
      relevel_matrix[,i]=factor(relevel_matrix[,i])
      }
  }

###################################################################################################################################
  ##4.  Rename the column of ordinal variables
  imputed_ordinal=data.frame(relevel_matrix)
  colnames(imputed_ordinal)<-colnames_ordinal
  rownames(imputed_ordinal)<-rownames_ordinal
}

  return(imputed_ordinal)

}



