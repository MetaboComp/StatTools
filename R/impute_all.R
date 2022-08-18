#'imputation for 3 types of variables
#'1.numeric variables
#'2.factor variables without order: non_ordinal factor variables
#'3.factor variable with order: ordinal factor variables
#'
#'Preparation: To use this package, 4 steps need to be done beforehand:
#'
#'Step 1: Made all variables that will be used a dataframe. For example, a data frame with name X
#'
#'Step 2: Put ordinal variables and numeric variables in one dataframe. Put  non-ordinal variables in another data frame.
#'This is done manual because computer cannot decide which variables we vant to give order to.
#'!!! Note: the class of variables in these data frames should be only "factor" or "numeric". (Use the Transform_fac_num in the same package.)
#'
#'Step 3: Code the ordinal factor variables properly!!!. For example, code it with alphabet or number that could show sequence.
#'
#'Step 4: Make sure that the sequence of the levels for each factor variable become the "default sequence". (Use the Transform_fac_num in the same package.)
#'Example: a<-factor(c(1,2,3)). When levels(a), the out put is 1,2,3. This is the "default sequence" we want.
#'         a<-factor(c(1,2,3),levels=c("3","1","2")). When levels(a), the out put is 3,1,2. This is not "default sequence".
#'         a<-Transform_fac_num(factor(c(1,2,3),levels=c("3","1","2"))). When levels(a), the output is 1,2,3.
#'
#' @param ordinal A dataframe with ordinal variables and numeric variables
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
#' f<-impute_all(e[,1:8],e[,9:10])
#' #f is the imputed data frame of d

impute_all<-function(ordinal,
                     non_ordinal){
##1 Make the sequence of the levels for each factor variable should be the "default sequence". Save column names of dataframes.
  ordinal<-Transform_fac_num(ordinal)$dataframe           ##it will not be in comment in the package
  non_ordinal<-Transform_fac_num(non_ordinal)$dataframe   ##it will not be in comment in the package
  non_ordinal=data.frame(non_ordinal)
  ordinal=data.frame(ordinal)
  names_non_ordinal<-colnames(non_ordinal)
  names_ordinal<-colnames(ordinal)


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
############################################################################################################
###2.3 save the names of levels for ordinal data frame

  factor_location<-NULL
  levels_names_ordinal<-list()
  for(i in 1:ncol(ordinal)){
    if(class(ordinal[,i])=="factor"){
      levels_names_ordinal[[i]]<-levels(ordinal[,i])
      ordinal[,i]=as.numeric(ordinal[,i])
      factor_location=c(factor_location,i)}
  }
############################################################
###2.4 impute the combined data frame (ordinal and non_ordinal one hot encoding) using mvImpWrap().
  big_matrix=cbind(ordinal,big_matrix)
  imputed_big_matrix=mvImpWrap(big_matrix)
  imputed_ordinal=imputed_big_matrix[,1:ncol(ordinal)]
  imputed_non_ordinal=imputed_big_matrix[,c((ncol(ordinal)+1):ncol(imputed_big_matrix))]



#############################################################################################################
###3 When doing imputation, the names of levels for factor variables are lost, this step is to gives the name back
###3.1 give back the level names of ordinal factor variables
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
    for(j in 1:length(levels(factor_imputed_ordinal[,factor_location[i]])))  ##for each factor variable level numbers
    {for(k in 1:length(factor_imputed_ordinal[,factor_location[i]])){   ##observation numbers

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
    if(class(factor_imputed_ordinal[,i])=="factor")
      relevel_matrix[,i]=factor(relevel_matrix[,i])
  }


##########################################################################################################################
##3.2 shrink each one hot encoding data frame for each non-ordinal factor variable and recombine them
##3.3 give back the level names of non_ordinal factor variables
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
##4. combine ordinal and non_ordinal data frame into one data frame and rename the columns
  imputed_data=data.frame(relevel_matrix,new_non_ordinal)
  colnames(imputed_data)<-c(names_ordinal,names_non_ordinal)
  return(imputed_data)

}



