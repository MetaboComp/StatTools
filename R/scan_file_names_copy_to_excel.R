#' Scan file names, copy to excel
#' @param Dir directory where the folder is. This folder contains folder where contain all files with names you want to scan
#' @param full_names logical, If you want the file names output with the directory names or not
#' @param keyphrase a vector of key phase in the file names that separate the columns. Case sensitive
#' @param rerank logical. Rerank base on the number at the
#' @return a excel file and a dataframe
#' @export


scan_file_names_copy_to_excel<-function(Dir,  ##"C:/Users/yingxiao/Desktop/rye_wheat/Y1_Ryeweight_mzMLmgf"
                                        full_names=F,
                                        keyphrase,
                                        rerank=T){
  nameslist<-list()
  for (i in 1:length(keyphrase)){
    nameslist[[i]]<-list.files (path = Dir,
              pattern=keyphrase[i],
              full.names=full_names)
  }
  nameslist_length<-vector()
  for (i in 1:length(nameslist)){
  nameslist_length<-c(nameslist_length,length(nameslist[[i]]))
  }
  frame<-as.data.frame(
    matrix(NA,
         max(nameslist_length),
         length(nameslist)
         ))
  for(i in 1:ncol(frame)){
    for(j in 1:length(nameslist[[i]])){
      frame[j,i]<-nameslist[[i]][j]
    }
  }
  colnames(frame)<-keyphrase
  
  library(openxlsx)
  write.xlsx(frame, 
                   paste0(format(Sys.Date(), 
                                 format="%Y-%m-%d"),
                          "_file_names",
                          ".xlsx"
                   ))
  return(frame)
}
