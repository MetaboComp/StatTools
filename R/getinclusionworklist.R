#' @param saveDir A directory you want to save the features and generate files. (You need to manually setwd() before doing this)
#' @param Dataframe_Obj A DATAFRAME with rows as as sample and columns as covariates/features (even one column in dataframe format), which must contain the feature aames you want to find 
#' The rownames are names in a format theat is easy to change, colnames are feature names
#' @param featurenames A vector of feature names. It must look like a format that is easy to change. for example RP_neg_674_3536a5_84 is okay

#' @param oldfeaturename_format a vector,what you want to change. 
#' @param newfeaturename_format a vector, what you want to change to note that your new name should be Mass, splitmark, rentension time
#' @param featurename_removerest logical, remove the rest of the name or not
#' @param oldsamplename_format a vector,what you want to change. 
#' @param newsamplename_format a vector, what you want to change to 
#' @param samplename_removerest logical, remove the rest of the name or not

#' @param minute_to_second, logical, change retention time from minute to seconds
#' @param rtOrNot Whether to use RT window when doing MS2 or not, default = False because usually we won't know where something is and there used to be trouble with the window historically
#' @param chromPol Either RN or RP
#' @param instrumentName Either Zoidberg or Fry

#' @param splitmark  ## the mark you used to separate mass and retension time
#' @param percentage What is the percentage thereshold that you want to selectrelatively highest features

#' @param splitfeatures_withcloserententiontime logical, if true, use split sample feature function
#' @param rententiontime_threshold  Usable when splitfeatures_withcloserententiontime is T
#' @param samplesplitlistmark  Usable when splitfeatures_withcloserententiontime is T

#' @param mergesamples_withfarthermz To be developed, logical, this is only for the sake of people in CMSI to create method conveniently
#' @param mergesamples_withfarthermz To be developed, default 10
#' @export
#' @return sampleSet


getinclusionworklist<- function(featurenames,  ## a 
                                oldfeaturename_format,
                                newfeaturename_format,
                                featurename_removerest=F, 
                                saveDir,   ## The name of the directory that you want to save
                                Dataframe_Obj, 
                                oldsamplename_format,
                                newsamplename_format,
                                samplename_removerest=F,
                                splitmark="@",  
                                rtOrNot=F, 
                                percentage=0.1,
                                chromPol, 
                                splitfeatures_withcloserententiontime,
                                mergesamples_withfarthermz=T,
                                instrumentName = c("Zoidberg", "Fry"),
                                rententiontime_threshold=20,
                                samplesplitlistmark="£",
                                minute_to_second=F){
## 1. save the feauture names and feature dataframe in an object 
  if(missing(splitfeatures_withcloserententiontime)){
    stop("You need to specify if you want to separate rentention time")
  }
  features <- featurenames
  table <- Dataframe_Obj
  if(!is.data.frame(table)){
    stop("The put Dataframe_obj should be a dataframe")
  }
  if(any(!features%in%colnames(table))){
    stop("feature names must in the column names of Dataframe_Obj")
  }
  #table_col<-c()
  # only select features that are in the feature names
  #for(z in 1:ncol(table)){
  #  if (colnames(table)[z]%in%features){
  #    table_col<-c(table_col,colnames(table)[z])}
  #}
  table<-table[,features]
  #table <- table[,which(colnames(table) %in% features)] 
  rtTest <- ifelse(rtOrNot, "1", "")
  
  if(instrumentName=="Zoidberg"){
    instrumentPath <- paste0("D:\\MassHunter\\Data\\Metabolomics\\", saveDir, "\\")
    methodMS2 <- "D:\\MassHunter\\Methods\\Metabolomics\\MS2 Targeted\\"
    
    if(chromPol=="RP"){
      methodMS1 <- "D:\\MassHunter\\Methods\\Metabolomics\\POS_CMSI-SOP-002v1_NoSwitch.m"
      chromPolAbbrev <- "RP_POS"
    } else {
      methodMS1 <- "D:\\MassHunter\\Methods\\Metabolomics\\NEG_CMSI-SOP-002v1_NoSwtch.m"
      chromPolAbbrev <- "RP_NEG"
    }
  } else if(instrumentName=="Fry") {
    instrumentPath <- paste0("E:\\MassHunter\\Data\\Metabolomics\\", saveDir, "\\")
    methodMS2 <- "E:\\MassHunter\\Methods\\Metabolomics\\MS2 Targeted\\"
    
    if(chromPol=="RP"){
      methodMS1 <- "E:\\MassHunter\\Methods\\Metabolomics\\POS_CMSI-SOP-002v1_NoSwitch.m"
      chromPolAbbrev <- "RP_POS"
    } else {
      methodMS1 <- "D:\\MassHunter\\Methods\\Metabolomics\\NEG_CMSI-SOP-002v1_NoSwtch.m"
      chromPolAbbrev <- "RP_NEG"
    }
  }
## 2. build the vector of sample names and a list of feature names(vector) under each sample name
  #### sampleSet structure ####
  #$sampleName - Name of sample (also fixed already if features overlapping in RT to include in sample-name)
  #$features - features to be analyzed in this sample
  sampleSet <- list()
  sampleSet$sampleFeatures <- list()
  i <- 0
  sampleSet$sampleNames <- c()
  
  
  ## since at this point sample names and feature names are rownames and colnames are the table, we change the name format here
  
  if(!missing(oldfeaturename_format)&!missing(newfeaturename_format)){
    features<-change_name(X=featurenames,
                oldname_format=oldfeaturename_format,
                newname_format=newfeaturename_format,
                remove_rest = featurename_removerest)
    colnames(table)<-features
  }
  if(!missing(oldsamplename_format)&!missing(newsamplename_format)){
    samplenames_new<-change_name(X=rownames(table),
                          oldname_format=oldsamplename_format,
                          newname_format=newsamplename_format,
                          remove_rest = samplename_removerest)
    rownames(table)<- samplenames_new
    
  }

  #### Figuring out which combinations of samples are the optimum combination

  while(is.data.frame(table)){
    i <- i+1

    scorematrix <- matrix(0L, 
                          nrow=nrow(table), 
                          ncol=ncol(table))
    colnames(scorematrix) <- colnames(table)
    rownames(scorematrix) <- rownames(table)
    ## order gives the position from the smallest to biggest number 
    ## for example order(c(6,4,7,8,1,3)) gives [1] 5 6 2 1 3 4, which means the first small number is at the 5th position
    for(j in 1:ncol(table)){
      temphigh <- order(table[,j],    ##gives the position from the biggest to smallest number in each column
                        decreasing=T)[1:round(nrow(Dataframe_Obj)*percentage)]  ## re rank it from biggest to smallest and choose the top 10% higest peak
      scorematrix[temphigh,j] <- 1  ## give this positions as 1
    }
    
    whsamp <- which.max(rowSums(scorematrix))  
    ## select the row position which gives the highest rowsum
    
    whfeats <- which(scorematrix[whsamp,]==1)
    ## select that row and generate a logical vector of where is 1
    
    table <- table[,-whfeats]  ## remove that column the next round of i
    sampleSet$sampleNames <- c(sampleSet$sampleNames, 
                               rownames(scorematrix)[whsamp]) ## put that sample name in the vector
    sampleSet$sampleFeatures[[i]] <- colnames(scorematrix)[whfeats]  ## the names of the features which are 1
    ## note that the sampleSet$sampleFeatures[[i]] do not have a name yet
 
    if (is.vector(table)){  ## The scenario when there is only one column
      sampleSet$sampleNames <- c(sampleSet$sampleNames, 
                                 samplenames_new[which.max(table)])
      sampleSet$sampleFeatures[[i+1]] <- colnames(scorematrix)[which(!(colnames(scorematrix) %in% sampleSet$sampleFeatures[[i]]))]
    }else if (dim(table)[2]==0){
      table <- vector()
    }
  }
  
  

  ##3. Splitting up features into mz & RT ####
  for(i in 1:length(sampleSet$sampleNames)){   ### for each sample name
    featuresSplit <- data.frame(matrix(nrow=length(sampleSet$sampleFeatures[[i]]),  ## How many features should be for this sample
                                       ncol=2))
    for(j in 1:length(sampleSet$sampleFeatures[[i]])){
      colnames(featuresSplit) <- c("mz","rt")
      
      temp <- unlist(strsplit(sampleSet$sampleFeatures[[i]][j], splitmark))  ## this could be adjusted
      if(minute_to_second==T){temp[2]<-as.double(temp[2])*60}
      featuresSplit[j,1] <- round(as.double(temp[1]),5)
      featuresSplit[j,2] <- round(as.double(temp[2]),0)
    }
    
    featuresSplit <- featuresSplit[order(featuresSplit[,2]),]  ## rank by rention time ealier comes first
    sampleSet$sampleFeatures[[i]] <- featuresSplit
  }
  
  baseSamples <- sampleSet$sampleNames
####4. split samples if rentention time is close 
  if(splitfeatures_withcloserententiontime==T){
    sampleSet<-splitsamplefeatures(sampleNames=sampleSet$sampleNames,
                        sampleFeatures=sampleSet$sampleFeatures,
                        rententiontime_threshold=0.5,
                        samplesplitlistmark="£"
    )
    
    
  }
  

  #### Setting up and writing inclusion lists for samples ####
  for(i in 1:length(sampleSet$sampleNames)){
    incListTemplate<-data.frame(matrix(ncol=9))
    incListTemplate[1,]<-c("AutoPreferredExcludeMSMSTable",NA,NA,NA,NA,NA,NA,NA,NA)
    incListTemplate[2,]<-c("On","Prec. m/z","Delta m/z (ppm)","Z","Prec. Type","Ret. Time (min)","Delta Ret. Time (min)","Iso. Width","Collision Energy")
    
    for(j in 1:nrow(sampleSet$sampleFeatures[[i]])){
      incListTemplate<-rbind(
        incListTemplate,
        c("True",
          round(sampleSet$sampleFeatures[[i]][j,1],5),
          "20",
          "1",
          "Preferred",
          round(sampleSet$sampleFeatures[[i]][j,2],0),
          rtTest,
          "Narrow (~1.3 m/z)",NA))
    }
    
    if(chromPol=="RP"){
      write.table(
        incListTemplate,
        paste0(
          format(Sys.Date(), format="%Y-%m-%d"),
          "_MS2_RP_POS_",
          sampleSet$sampleNames[i],
          ".csv"),
        col.names=FALSE,
        row.names=FALSE,
        sep=",",
        na="",
        quote=FALSE
      )
    
      
      sampleSet$directory[i] <- paste0(
        format(Sys.Date(), format="%Y-%m-%d"),
        "_MS2_RP_POS_",
        sampleSet$sampleNames[i]
      )
      
    } else {
      write.table(
        incListTemplate,
        paste0(
          format(Sys.Date(), format="%Y-%m-%d"),
          "_MS2_RP_NEG_",
          sampleSet$sampleNames[i],
          ".csv"
        ),
        col.names=FALSE,
        row.names=FALSE,
        sep=",",
        na="",
        quote=FALSE
      )
      
      sampleSet$directory[i] <- paste0(
        instrumentPath,
        format(Sys.Date(), format="%Y-%m-%d"),
        "_MS2_RP_NEG_",
        sampleSet$sampleNames[i]
      )
      
    }
  }
  
  #### Write work list ####
  workListTemplate<-data.frame(matrix(ncol=14))
  #Headers
  workListTemplate[1,] <- c("CHARGE", "Well content", "STUDY INJ ID", "Position", 
                            "DATA", "", "", "", "", "", "", "", "", "Method")
  
  #Sample names
  workListTemplate[2:(length(sampleSet$sampleNames)*2+10), 1] <- ifelse(chromPol=="RP","POS","NEG")
  workListTemplate[2:5,2] <- "blank"
  workListTemplate[6:7,2] <- "solv-blank"
  workListTemplate[8:10,2] <- "cond"
  workListTemplate[11:(length(sampleSet$sampleNames)*2+10),2] <- rep(sampleSet$sampleNames, each=2)
  for(i in seq(12, length(sampleSet$sampleNames)*2+10, 2)){
    workListTemplate[i,2] <- paste0(workListTemplate[i,2], "_MS2")
  }
  
  #Injection numbers
  workListTemplate[2:nrow(workListTemplate),3]<-c(1:(nrow(workListTemplate)-1))
  workListTemplate[2:10,3] <- paste0("00",workListTemplate[2:10,3])
  #If less than 100 add "0" to highest number
  if(nrow(workListTemplate) < 101){
    workListTemplate[11:nrow(workListTemplate),3] <- paste0("0",workListTemplate[11:nrow(workListTemplate),3])
  } else {
    workListTemplate[11:101,3] <- paste0("0",workListTemplate[11:101,3])
  }
  
  #Position in sample holder, DATA and Method
  workListTemplate[2:5,4] <- "No injection"
  workListTemplate[6:7,4] <- "P1-A1"
  workListTemplate[8:10,4] <- "Vial 2"
  
  samps <- 1
  numbers <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
  letters <- c("A", "B", "C", "D", "E", "F")
  plates <- c("P1", "P2")
  
  #Filling in position
  for(i in 1:length(baseSamples)){
    
    workListTemplate[grepl(baseSamples[i], workListTemplate[,2]), 4] <- paste0(ifelse(samps>54, 
                                                                                      plates[2], 
                                                                                      plates[1]), 
                                                                               "-", 
                                                                               letters[floor(samps/10)+1%%9], 
                                                                               numbers[round(samps%%9)])
    
    samps <- samps+1
  }
  
  #Filling in method and DATA
  workListTemplate[2:10,14] <- methodMS1
  workListTemplate[2:10, 5] <- paste0(instrumentPath, 
                                      format(Sys.Date(), 
                                             format="%Y-%m-%d"), 
                                      "_", chromPolAbbrev,
                                      "_", workListTemplate[2:10,2])
  for(i in 1:(length(sampleSet$sampleNames)*2)){
    workListTemplate[i+10, 5] <- paste0(instrumentPath, 
                                        sampleSet$directory[ceiling(i/2)], 
                                        "_", 
                                        workListTemplate[i+10,3])
    if(i%%2==1){
      workListTemplate[i+10, 5] <- str_replace(workListTemplate[i+10, 5], "_MS2", "")
    }
    workListTemplate[i+10, 14] <- ifelse((i%%2)==1, methodMS1, paste0(methodMS2, "\\", saveDir, "\\", sampleSet$directory[ceiling(i/2)],".m"))
  }
  
  #Removing all double dashes
  workListTemplate[,5] <- str_replace_all(workListTemplate[,5], pattern="\\\\", replacement="//")
  workListTemplate[,5] <- str_replace_all(workListTemplate[,5], pattern="//", replacement="/")
  workListTemplate[,14] <- str_replace_all(workListTemplate[,14], pattern="\\\\", replacement="//")
  workListTemplate[,14] <- str_replace_all(workListTemplate[,14], pattern="//", replacement="/")
  
  write.xlsx(workListTemplate, 
             paste0(format(Sys.Date(), 
                           format="%Y-%m-%d"),
                    "_WorkList_",
                    ifelse(chromPol=="RP","POS","NEG"),
                    ".xlsx"
                    ), 
             col.names=FALSE)
  saveRDS(sampleSet, paste0("Report",ifelse(chromPol=="RP","POS","NEG"),".rds"))
  return(sampleSet)
  
}





