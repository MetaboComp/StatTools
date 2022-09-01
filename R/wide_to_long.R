
#' @param dataframe_orginal rows as samples, columns as covariates andomics data
#' @param datablock A data block in the wide format, which will be changed to long format
#' 
#' @param breakpoint A vector with numeric values that says where it will sepate columns. For example, 
#' If breakpoints<.c(1,3,7). It means it separate the datablock timepoint to long format as column 1, 2-3,4-7, 8-ncol(datablock_timepoint)
#' @param Class_names The new variable names that you want to give to the new variable. The length should be length of 
#' @return dataframe_orgnanized
#' @export
#'
#' 
#'
wide_to_long<-function(dataframe_orginal,
                       datablock,
                       breakpoint,
                       Class_names,
                       datablock_long_colnames
                       
                       
    ){
  if(length(breakpoint)<1){stop("must have break point>=1")}
  if(length(Class_names)<2){stop("must have at least 2 Classes")}
  if(ncol(datablock)<=breakpoint[length(breakpoint)]){
    stop("Such breakpoint does not exist since it is beyond/equal the number of columns of dataframe")
  }
  if(length(Class_names)!=(length(breakpoint)+1)){
    stop("number of classes -1 should equal to the length of breakpoint")
  }
  
  if(nrow(dataframe_orginal!=datablock)){
    stop("number of samples should be the same")
  }
datablock_list<-list()
datablock_list[[1]]<-datablock_timepoint[,1:breakpoint[1]]

for(i in 1:length(breakpoint)){
  if(i!=length(breakpoint)){
  datablock_list[[i+1]]<-datablock[,i+1:breakpoint[i+1]]
  }
  if(i==length(breakpoint)){
  datablock_list[[i+1]]<-datablock[,i+1:ncol(datablock)]
  }
}
difference<-vector()
breakpoint_plus2<-c(0,breakpoint,ncol(datablock))
for(i in 1:(length(breakpoint_plus2)-1)){
difference<-c(difference,breakpoint_plus2[i+1]-breakpoint_plus2[i])

}
############unfinished
########################################################################################
datablock_long<-matrix(NA,
                       length(rep(rownames(dataframe_orginal),length(Class_names))),
                       )
for(i in 1:length(datablock_list)){
  colnames(datablock_list[[i]])<-NULL
}

######################################################################################
Class_variable<-vector()
for(i in 1:length(Class_names)){
  Class_variable<-c(Class_variable,rep(Class_names[i],nrow(dataframe_orginal)))
  dataframe_original_long<-rbind(dataframe_original_long,dataframe_original_long)
}
##########################################################################################
dataframe_original_long<-dataframe_original
for(i in 1:(length(Class_names)-1)){
  dataframe_original_long<-rbind(dataframe_original_long,dataframe_original)
}



  return(dataframe_organized)
}


