#'imputation for non-ordinal factor variables
#'
#'Preparation: To use this package, 1 steps need to be done beforehand:
#'
#'Step 1: Made all variables that will be used a dataframe. For example, a data frame with name X
#'
#'!!! Note: the class of variables in these data frames should be only "factor". (Use the Transform_fac_num in the same package.)
#'
#'Step 2:  Make sure that the sequence of the levels for each factor variable become the "default sequence". (Use the Transform_fac_num in the same package.)
#'Example: a<-factor(c(1,2,3)). When levels(a), the out put is 1,2,3. This is the "default sequence" we want.
#'         a<-factor(c(1,2,3),levels=c("3","1","2")). When levels(a), the out put is 3,1,2. This is not "default sequence".
#'         a<-Transform_fac_num(factor(c(1,2,3),levels=c("3","1","2"))). When levels(a), the output is 1,2,3.
#'
#' @param non-ordinal A dataframe with non-ordinal variables
#' @return A dataframe with imputed values
#' @export
#' 
#'
#' install.packages("remotes")
#' library(remotes)
#' install_gitlab('CarlBrunius/MUVR')
#' library(MUVR)
#' factor_1<-factor(c(rep("a",8),rep("b",8),rep("c",8),rep("d",5)))
#' factor_2<-as.character(c(rep("f",5),rep("e",8),rep("h",5),rep("g",11)))
#' d<-cbind(factor_1,factor_2,Yotu)
#' d[29,1]=NA
#' d[28,2]=NA
#' d[27,3]=NA
#
#' e=Transform_fac_num(d)$dataframe
#' f<-impute_non_ordinal(e)
#' # f is the imputed data frame of d

impute_non_ordinal<-function(
                     non_ordinal){

##1 Make the sequence of the levels for each factor variable should be the "default sequence". Save column names of the dataframe.
  non_ordinal<-Transform_fac_num(non_ordinal)$dataframe   ##it will not be in comment in the package
  non_ordinal=data.frame(non_ordinal)
  names_non_ordinal<-colnames(non_ordinal)

  ##2. impute the data
  ##2.1. separate each non_ordinal variable and transform them to one hot encoding matrix
  matrix_new=list()
  for(j in 1:ncol(non_ordinal)){
    matrix_new[[j]]<-matrix(NA,
                            length(non_ordinal[,j]),
                            length(levels(non_ordinal[,j])))

    for(i in 1:length(non_ordinal[,j])){
      if(is.na(non_ordinal[i,j])){
        matrix_new[[j]][i,]<-NA
      }
      if(!is.na(non_ordinal[i,j])){
        for(k in 1:length(levels(non_ordinal[,j])))
        {if(non_ordinal[i,j]==levels(non_ordinal[,j])[k]){
          matrix_new[[j]][i,k]<-1
        }else{
          matrix_new[[j]][i,k]<-0
        }
        }
      }
    }
  }
  ##2.2. combine all matrix of non_ordinal variables
  matrix_imputed=list()
  if(ncol(non_ordinal)==1){big_matrix=matrix_new[[1]]
  }else{big_matrix=matrix_new[[1]]
  for(j in 2:ncol(non_ordinal)){
    big_matrix=cbind(big_matrix,matrix_new[[j]])}
  }

  ############################################################
  ###2.3 impute the data frame them using mvImpWrap
  imputed_big_matrix=mvImpWrap(big_matrix)

  imputed_non_ordinal=imputed_big_matrix



  #############################################################################################################
  ###3 When doing imputation, the names of levels for factor variables are lost, this step is to gives the name back

  ##########################################################################################################################
  ##3.1 shrink each one hot encoding data frame for each non-ordinal factor variable and recombine them
  ##3.2 give back the level names of non_ordinal factor variables
  sum=1
  for(j in 1:ncol(non_ordinal)){
    levels=length(levels(non_ordinal[,j]))
    matrix_imputed[[j]]=imputed_non_ordinal[,c(sum:(levels+sum-1))]
    sum=sum+levels
  }
  imputed_variable=list()
  for (j in 1:ncol(non_ordinal)){
    levels_names_non_ordinal=levels(non_ordinal[,j])
    imputed_variable[[j]]=vector()
    for (i in 1:dim(matrix_imputed[[j]])[1]){

      imputed_variable[[j]][i]=levels_names_non_ordinal[which.max(matrix_imputed[[j]][i,])]
    }
  }
  new_non_ordinal=matrix(NA,
                         nrow(non_ordinal),
                         ncol(non_ordinal))
  new_non_ordinal=as.data.frame(new_non_ordinal)

  for (j in 1:ncol(non_ordinal)){
    new_non_ordinal[,j]=as.factor(imputed_variable[[j]])
  }

  ###################################################################################################################################
##4. Rename the column of non_ordinal variables

  colnames(new_non_ordinal)<-names_non_ordinal
  return(new_non_ordinal)

}



