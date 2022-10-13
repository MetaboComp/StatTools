#' Please consult King Yan if smth is not clear
#' One important thing is that you need to set your directory correctly before using the function
#' You need to double check if your xlsx file name is correct
#' @param Dir
#' @param File_first The first file, POS or NEG, end with xlsx
#' @param File_second The first file, POS or NEG, end with xlsx
#' @param MS_type you need to specify if the work list is for MS1 or MS2. In most cases MS2
#' @export
#' @return data
#' 
merge_worklist<-function(Dir=getwd(),
                         File_first,
                         File_second,
                         MS_type ="MS2"){
  
  
  library(stringr)
  library(openxlsx)
  library(stringi)
  if(!missing(Dir)){setwd(Dir)}
  data1<-read.xlsx(File_first)
  data2<-read.xlsx(File_second)
  data<-rbind(data1,data2)
  ###################put other columns into NA
  data[2:nrow(data),1:7]<-NA
  
  ##### column 10, 11 ,12 needs to be adjusted
  ##column 10
  #Injection numbers
  data[,10]<-1:nrow(data)
  data[1:9,10] <- paste0("00",data[1:9,10])
  #If less than 100 add "0" to highest number
  if(nrow(data) < 101){
    data[10:nrow(data),10] <- paste0("0",data[10:nrow(data),10])
  } else {
    data[10:101,3+7] <- paste0("0",data[10:101,10])
  }
  
  # column 11
  data[(nrow(data1)+7):(nrow(data1)+9),11]<-"Vial 3"

  samps <- 1
  samps_samps<-9
  numbers <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
  letters <- c("A", "B", "C", "D", "E", "F")
  plates <- c("P1", "P2")
  
  position_name<-vector()
  #Filling in position
  
  if(MS_type =="MS2"){
  for(i in 1:(nrow(data)-18)){
    
    position_name <- c(position_name,
                       paste0(ifelse(samps>54, 
                                   plates[2], 
                                   plates[1]), 
                            "-", 
                            letters[ceiling(samps/9)], 
                            numbers[floor(samps_samps%%9)+1]),
                       paste0(ifelse(samps>54, 
                                     plates[2], 
                                     plates[1]), 
                              "-", 
                              letters[ceiling(samps/9)], 
                              numbers[floor(samps_samps%%9)+1])
    )
    samps_samps <- samps_samps+1
    samps <- samps+1
  }
  position_name <-position_name[-c(1:2)]
  
  data[-c(1:9,(nrow(data1)+1):(nrow(data1)+9)),11]<-NA
  
  samps<-1
  for(i in 1:nrow(data)){
    if(is.na(data[i,11])==T){
      data[i,11]<-position_name[samps]
      samps<-samps+1
    }
  }
  
  
  }else if(MS_type =="MS2"){
    
    for(i in 1:(nrow(data)-18)){
      
      position_name <- c(position_name,
                         paste0(ifelse(samps>54, 
                                       plates[2], 
                                       plates[1]), 
                                "-", 
                                letters[ceiling(samps/9)], 
                                numbers[floor(samps_samps%%9)+1])
      )
      samps_samps <- samps_samps+1
      samps <- samps+1
    }
    position_name <-position_name[-c(1:2)]
    
    data[-c(1:9,(nrow(data1)+1):(nrow(data1)+9)),11]<-NA
    
    samps<-1
    for(i in 1:nrow(data)){
      if(is.na(data[i,11])==T){
        data[i,11]<-position_name[samps]
        samps<-samps+1
      }
    }
    
    
  } else {stop("Only MS1 and MS2 available")}
  

  # column 12
  
  for(i in 1:nrow(data2)){
    data[(nrow(data2)+i),12]<-change_name(data[(nrow(data2)+i),12],
                                          data2[i,10],
                                          data[(nrow(data2)+i),10])
  }
  
  
  
  
  
  
  
  ###write.xlsx
  write.xlsx(data, 
             paste0(format(Sys.Date(), 
                           format="%Y-%m-%d"),
                    "_WorkList_all",
                    ".xlsx"
             ), 
             col.names=FALSE)
  return(data)
}