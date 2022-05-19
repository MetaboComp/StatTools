#' This function is to check for overlapping of values or dataframe columns in different objects.
#' You could see this as a venn diagram but in nicer text version. It does not have the limitation of venn diagram that only at most 5-6 object can be shown together
#' 
#' 
#' 
#' @param X This should be a list of vectors or dataframes (There must be names of each object in the list). 
#' Each object in the list should be in the same format (all in vector or all in data.frame).  
#' If it is a list of dataframe, then the rows should be observations and columns variables, 
#' and only completely identical columns (both column names and values) will be considered as overlapped
#' If it is a list of dataframe, the columns with the same names accross object should have exactly identical vaule,
#' otherwise an eroor is given (Thus this function also serve the purpose of check if data are identical or not) 
#' 

#' @return all the "and" and "or"options 
#' @export
# Example code
# install.packages("remotes")
# library(remotes)
# install_gitlab("CarlBrunius/MUVR@MUVR2")
# library(MUVR)
# X1<-data.frame(Xotu[,1:10])
# X2<-data.frame(Xotu[,1:13])
# X3<-data.frame(Xotu[,9:15])
# X4<-data.frame(Xotu[,16:20])
# X<-list(X1=X1,X2=X2,X3=X3,X4=X4)
# 
# Z1<-c("a","b","c","d","e","f")
# Z2<-c("e","f","g")
# Z3<-c("e","f","g","h","i","j")
# Z4<-c("k","l","m")
# Z<-list(Z1=Z1,Z2=Z2,Z3=Z3,Z4=Z4)
#
#
overlapping_check<-function(X){
  result<-list()
  num_obj<-length(X)
  obj_names<-names(X)
  possibility<-2^(length(X))-1
  
  
##1.sanity check to make sure the data has right format:
  ##There should be more than one object
  ##The class() in each object should be the same
  ##There should be names of each object and they are not identical.
  if(length(X)<2){stop("There should be more than one object.")}
  
  class_type<-vector()
  for (i in 1:num_obj){
    class_type<-c(class_type,class(X[[i]]))
  }
  if(length(table(class_type))!=1){
    stop(
      "All object in the list should have same format.")
  }
  
  
  if(is.null(obj_names)|length(unique(obj_names))!=length(obj_names)){
    stop("There should be names of each object and they are not identical.")
  }
  
  
  ##do not add it, since there are many type of vectors, do not want to miss things 
  #if(class(X[[1]])!="vector" & class(X[[1]])!="data.frame"){
  #  stop("class type must be vector and dataframe")
  #}
  
##2. The scenario when each object in the list is vector 
  if(class(X[[1]])!="data.frame"){
    alldata<-vector()
  for (i in 1:num_obj){
    alldata<-c(alldata,X[[i]])
  }
  alldata<-unique(alldata) 
  
  
  ##2.1 a matrix, row are values,columns are  objects
  #if a value appear in one object, 1, other wise 0
  alldata_summary<-matrix("no",
                          length(alldata),
                          num_obj)
  alldata_summary<-data.frame(alldata_summary)
  rownames(alldata_summary)<-alldata           ##there will not be identical in rownames since alldata is unique (dataframe do not allow identical rownames anyway)
  colnames(alldata_summary)<-obj_names         ##there will not be identical colnames since obj_names is checked all unique
  for(i in 1:length(alldata)){
    for(j in 1:num_obj){
      if(alldata[i]%in%X[[j]]){
        alldata_summary[i,j]="yes"
      }
    }
  }

  ##2.2 Find all the unique combinations of no and yes
  alldata_summary_unique<-alldata_summary[!duplicated(alldata_summary),]
  

 rownames(alldata_summary_unique)<-NULL
  rownames_alldata_summary<-rownames(alldata_summary)
  overlapping_list<-list()
  for(i in 1:nrow(alldata_summary_unique)){
    overlapping_list[[i]]<-vector()
    for(j in 1:nrow(alldata_summary)){
      ####temporary remove rownames to fulfill the identical function
     

      rownames(alldata_summary)<-NULL
      if(identical(as.vector(as.matrix(alldata_summary[j,])),
                   as.vector(as.matrix(alldata_summary_unique[i,])))){

        overlapping_list[[i]]<-c(overlapping_list[[i]],rownames_alldata_summary[j])
      }
      rownames(alldata_summary)<-rownames_alldata_summary
    }
    
  }
  rownames(alldata_summary)<-rownames_alldata_summary
  
  ##2.3 give overlapping list proper names
  overlapping_list_names<-vector()
  for(i in 1:nrow(alldata_summary_unique)){
    
    howmanyyes<-table(as.vector(as.matrix(alldata_summary_unique[i,])))[2]
    list_name<-paste0("appear ",howmanyyes," time(s): in")
    for(j in 1:num_obj){
      if(alldata_summary_unique[i,j]=="yes"){
        list_name<-paste0(list_name," ",colnames(alldata_summary_unique)[j]," ")
      }
      
    }
    overlapping_list_names<-c(overlapping_list_names,list_name)
    
    
  }
  ##2.4 give the name to the overlapping list
  
  
  
  result$overlapping_list<-overlapping_list
  names(result$overlapping_list)<-NULL
  names(result$overlapping_list)<-overlapping_list_names
  result$overlapping_list_names<-overlapping_list_names
  result$alldata_summary<-alldata_summary
  result$alldata_summary_unique<-alldata_summary_unique
  
  }

 

##3. The scenario when each object in the list is dataframe (dataframe allow different colnames but not different rownames)
##There could be normal scenario that 2 columns has diffrent colnames but identical values,
##therefore, only stop when there are identical colnames
  if(class(X[[1]])=="data.frame"){
##3.1 Sanity check for the list:'
    ## There should not be identical colnames in each object
    ## The number of rows in each object should be the same
    ## The rownames of the objects should be the same
    for(i in 1:num_obj){
      if(!identical(unique(colnames(X[[i]])),colnames(X[[i]]))){
        stop("There are repeated column names in",names(X)[i])
      }
    }
    
    data_row<-vector()
    for(i in 1:num_obj){
      data_row<-c(data_row,nrow(X[[i]]))
        }
    if (length(table(data_row))!=1){
    stop("The number of rows should be identical.")
    
    }
    
    data_rownames<-rownames(X[[1]])
    for(i in 2:num_obj){
      if (!identical(rownames(X[[i]]),data_rownames)){
      stop("The number of rows should be identical. The ",i," object has different rownames than the rownames of the first object")
      
    }
    }
    
    
##3.2 Check if the values in the columns with same column names across objects are identical. They should be!
    alldata<-data.frame(row.names = data_rownames)
    for (i in 1:num_obj){
      alldata<-cbind(alldata,X[[i]])
    }
    alldata_unique<-alldata[!duplicated(as.list(alldata))]
    for (i in 1:ncol(alldata_unique)){
      for (i in 1:ncol(alldata)){
        if(colnames(alldata)[i]==colnames(alldata_unique)[j]){
        if(!identical(alldata[,i],alldata_unique[,j])){
          stop("There are not identical values between list objects in the column with column name: ",)
        }
        }
      }
    }
    
##3.3 After all those are checked, use column names as vector to do overlapping check as in section 2  
    
    
    
    
    alldata<-unique(colnames(alldata)) 
    
    
    
    
    ##2.1 a matrix, row are values,columns are  objects
    #if a value appear in one object, 1, other wise 0
    alldata_summary<-matrix("no",
                            length(alldata),
                            num_obj)
    alldata_summary<-data.frame(alldata_summary)
    rownames(alldata_summary)<-alldata           ##there will not be identical in rownames since alldata is unique (dataframe do not allow identical rownames anyway)
    colnames(alldata_summary)<-obj_names         ##there will not be identical colnames since obj_names is checked all unique
    for(i in 1:length(alldata)){
      for(j in 1:num_obj){
        if(alldata[i] %in% colnames(X[[j]])){
          alldata_summary[i,j]="yes"
        }
      }
    }
    
    ##2.2 Find all the unique combinations of no and yes
    alldata_summary_unique<-alldata_summary[!duplicated(alldata_summary),]
    rownames(alldata_summary_unique)<-NULL
    
    
    rownames_alldata_summary<-rownames(alldata_summary)
    overlapping_list<-list()
    for(i in 1:nrow(alldata_summary_unique)){
      overlapping_list[[i]]<-vector()
      for(j in 1:nrow(alldata_summary)){
        ####temporary remove rownames to fulfill the identical function
        
        
        rownames(alldata_summary)<-NULL
        if(identical(as.vector(as.matrix(alldata_summary[j,])),
                     as.vector(as.matrix(alldata_summary_unique[i,])))){
          
          overlapping_list[[i]]<-c(overlapping_list[[i]],rownames_alldata_summary[j])
        }
        rownames(alldata_summary)<-rownames_alldata_summary
      }
      
    }
    rownames(alldata_summary)<-rownames_alldata_summary
    
    ##2.3 give overlapping list proper names
    overlapping_list_names<-vector()
    for(i in 1:nrow(alldata_summary_unique)){
      
      howmanyyes<-table(as.vector(as.matrix(alldata_summary_unique[i,])))[2]
      list_name<-paste0("appear ",howmanyyes," time(s): in")
      for(j in 1:num_obj){
        if(alldata_summary_unique[i,j]=="yes"){
          list_name<-paste0(list_name," ",colnames(alldata_summary_unique)[j]," ")
        }
        
      }
      overlapping_list_names<-c(overlapping_list_names,list_name)
      
      
    }
    ##2.4 give the name to the overlapping list of column
    
    
    result$unique_columns<-alldata_unique
    
    result$overlapping_list<-overlapping_list
    names(result$overlapping_list)<-NULL
    names(result$overlapping_list)<-overlapping_list_names
    result$overlapping_list_names<-overlapping_list_names
    result$alldata_summary<-alldata_summary
    result$alldata_summary_unique<-alldata_summary_unique
    
    
    
    
    
    
    
    
  }
  
  
  return(result)
}