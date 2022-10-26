#' Please consult King Yan if smth is not clear
#' @param Dir The directory where you want to save every thing of your output (It is different from the saveDir)
#' @param Dataframe_Obj A DATAFRAME with rows as as sample and columns as covariates/features (even one column,it should be in dataframe format), which must contain the feature names you want to find,otherwise you get an error 
#' The rownames are names in a format that is easy to change, colnames are feature names
#' @param featurenames A vector of feature names. It must look like a format that is easy to change. for example RP_neg_674_3536a5_84 is okay

#' @param oldfeaturename_format a vector,what part in the feature name you want to change. Do not use $
#' @param newfeaturename_format a vector, what you want to change to for specific part of feature name, note that your new name should be Mass, splitmark, retention time. Do not use $
#' @param featurename_removerest logical, remove the rest of the name or not (the part in the name that is not changed)
#' 
#' @param oldsamplename_format a vector,what part in the sample name you want to change. Do not use $
#' @param newsamplename_format a vector, what you want to change to for specific parts of sample name. Do not use $
#' @param samplename_removerest logical, remove the rest of the name or not (the part in the name that is not changed)

#' @param minute_to_second, logical, change retention time from minute to seconds
#' @param rtOrNot Whether to use RT window when doing MS2 or not, default = False because usually we won't know where something is and there used to be trouble with the window historically
#' @param chromPol Either RN or RP
#' @param instrumentName Either Zoidberg or Fry
#' getinclusionworklist
#' @param splitmark The mark you used to separate mass and retention time. For example "@"
#' @param percentage What is the percentage threshold that you want to select relatively highest features. For example, 0.9 means you select features with intensity of at least 90% of the highest peak

#' @param splitfeatures_withcloserententiontime logical, if true, use split sample feature function
#' @param rententiontime_threshold  What difference value is considered as close. Usable when splitfeatures_withcloserententiontime is T
#' @param samplesplitlistmark  A split mark for split samples. Usable when splitfeatures_withcloserententiontime is T

#' @param mergesamples_withfarthermz To be developed, logical, this is only for the sake of people in CMSI to create method conveniently
#' @param mergesamples_withfarthermz To be developed, default 10
#' @param batch  Batch on Worklist
#' @param Date   Date on Worklist
#' @param Week  Week on Worklist
#' @param phase  Phase on Worklist
#' @export
#' @return sampleSet


getinclusionworklist<- function(Dir=getwd(), 
                                featurenames,  ## a 
                               
                                oldfeaturename_format,
                                newfeaturename_format,
                                featurename_removerest=F, 
                                Dataframe_Obj, 
                                oldsamplename_format,
                                newsamplename_format,
                                samplename_removerest=F,
                                splitmark="@",  
                                rtOrNot=F, 
                                percentage=0.9, 
                                ##by percentage of intensity not by rank
                                chromPol, 
                                splitfeatures_withcloserententiontime,
                                mergesamples_withfarthermz=T,
                                instrumentName = c("Zoidberg", "Fry"),
                                rententiontime_threshold=20,
                                samplesplitlistmark="£",
                                minute_to_second=F,
                                batch=1,
                                Week=1,
                                Date,
                                phase="RP"
                                ){
  library(stringr)
  library(openxlsx)
  library(stringi)
  if(!missing(Dir)){setwd(Dir)}

## 1. save the feauture names and feature dataframe in an object 
  if(missing(Date)){Date=format(Sys.Date(), 
                                format="%Y-%m-%d")}
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
    instrumentPath <- paste0("D:\\MassHunter\\Data\\Metabolomics\\MetID\\",batch,  "\\")
    methodMS2 <- "D:\\MassHunter\\Methods\\Metabolomics\\MS2 Targeted\\"
    
    if(chromPol=="RP"){
      methodMS1 <- "D:\\MassHunter\\Methods\\Metabolomics\\POS_CMSI-SOP-002v1_NoSwitch.m"
      chromPolAbbrev <- "RP_POS"
    } else {
      methodMS1 <- "D:\\MassHunter\\Methods\\Metabolomics\\NEG_CMSI-SOP-002v1_NoSwtch.m"
      chromPolAbbrev <- "RP_NEG"
    }
  } else if(instrumentName=="Fry") {
    instrumentPath <- paste0("E:\\MassHunter\\Data\\Metabolomics\\MetID\\",batch, "\\")
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
  } else {features<-featurenames
  colnames(table)<-features
  }
  if(!missing(oldsamplename_format)&!missing(newsamplename_format)){
    samplenames_new<-change_name(X=rownames(table),
                          oldname_format=oldsamplename_format,
                          newname_format=newsamplename_format,
                          remove_rest = samplename_removerest)
    rownames(table)<- samplenames_new
    
  }else{samplenames_new<-rownames(table)
    rownames(table)<-samplenames_new}

  #### Figuring out which combinations of samples are the optimum combination
  #table_original<-table
  while(is.data.frame(table)){
    i <- i+1

    scorematrix <- matrix(0L, 
                          nrow=nrow(table), 
                          ncol=ncol(table))
    colnames(scorematrix) <- colnames(table)
    rownames(scorematrix) <- rownames(table)
    ## order gives the position from the smallest to biggest number 
    ## for example order(c(6,4,7,8,1,3)) gives [1] 5 6 2 1 3 4, which means the first small number is at the 5th position
#######################################################################################################
    for(j in 1:ncol(table)){
      for(s in 1:nrow(table)){
      if(table[s,j]>=table[which.max(table[,j]),j]*percentage){
        scorematrix[s,j]<-1}
      }
      #temphigh <- order(table[,j],    ##gives the position from the biggest to smallest number in each column
      #                  decreasing=T)[1:round(nrow(Dataframe_Obj)*percentage)]  ## re rank it from biggest to smallest and choose the top 10% higest peak

    }
##############################################################################################    
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
          "_",
          sampleSet$sampleNames[i],
          "_RP_POS-MS2",
          ".csv"),
        col.names=FALSE,
        row.names=FALSE,
        sep=",",
        na="",
        quote=FALSE
      )
    
      
      sampleSet$directory[i] <- paste0(
        instrumentPath,
        format(Sys.Date(), format="%Y-%m-%d"),
        "_",
        batch,
        "W",
        Week,
        "_",
        "RP_POS-",
        sampleSet$sampleNames[i],
        "_MS2"
        
      )
      
    } else {
      write.table(
        incListTemplate,
        paste0(
          format(Sys.Date(), format="%Y-%m-%d"),
          "_",
          sampleSet$sampleNames[i],
          "_RP_NEG-MS2",
          
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
        "_",
        batch,
        "W",
        Week,
        "_",
        "RP_NEG-",
        sampleSet$sampleNames[i],
        "_MS2"
        

      )
      
    }
  }
  
  #### Write work list ####
  workListTemplate<-data.frame(matrix(ncol=14+7))  # add aditionally 8 columns
  #Headers
  workListTemplate[1,] <- c(
     "Path","Date", "","Batch","Week","","Phase",  ## add aditionally 8 columns
     "CHARGE", "Well content", "STUDY INJ ID", "Position", 
     "Injection volume(ug)","DATA", "", "", "", "", "", "", "",  
     "Method" )
  workListTemplate[2,1:7] <-c("D:/MassHunter/Data/Metabolomics/MetID",Date, "/_",batch,Week,"_",phase)
  #Sample names
  workListTemplate[2:(length(sampleSet$sampleNames)*2+10), 1+7] <- ifelse(chromPol=="RP","POS","NEG")
  workListTemplate[2:5,2+7] <- "blank"
  workListTemplate[6:7,2+7] <- "solv-blank"
  workListTemplate[8:10,2+7] <- "cond"
  
  
  workListTemplate[11:(length(sampleSet$sampleNames)*2+10),2+7] <- rep(sampleSet$sampleNames, each=2)
  for(i in seq(12, length(sampleSet$sampleNames)*2+10, 2)){
    workListTemplate[i,2+7] <- paste0(workListTemplate[i,2+7], "-MS2")
  }
  
  #Injection numbers
  workListTemplate[2:nrow(workListTemplate),3+7]<-c(1:(nrow(workListTemplate)-1))
  workListTemplate[2:10,3+7] <- paste0("00",workListTemplate[2:10,3+7])
  #If less than 100 add "0" to highest number
  if(nrow(workListTemplate) < 101){
    workListTemplate[11:nrow(workListTemplate),3+7] <- paste0("0",workListTemplate[11:nrow(workListTemplate),3+7])
  } else {
    workListTemplate[11:101,3+7] <- paste0("0",workListTemplate[11:101,3+7])
  }
  
  #Position in sample holder, DATA and Method
  workListTemplate[2:5,4+7] <- "No injection"
  workListTemplate[6:7,4+7] <- "P1-A1"
  workListTemplate[8:10,4+7] <- "Vial 2"
  
  samps <- 1
  numbers <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
  letters <- c("A", "B", "C", "D", "E", "F")
  plates <- c("P1", "P2")
  
  #Filling in position
  for(i in 1:length(baseSamples)){
    
    workListTemplate[grepl(baseSamples[i], workListTemplate[,2+7]), 4+7] <- paste0(ifelse(samps>54, 
                                                                                      plates[2], 
                                                                                      plates[1]), 
                                                                               "-", 
                                                                               letters[floor(samps/10)+1%%9], 
                                                                               numbers[round(samps%%9)+1])
    
    samps <- samps+1
  }
  
  workListTemplate[2:10,5+7] <- "As Method"
  for(i in 1:(length(sampleSet$sampleNames)*2)){
    if(i%%2==1){workListTemplate[i+10, 5+7]<-"As Method"
      
    }else{workListTemplate[i+10, 5+7]<-10
    }
    }
    
  #Filling in method and DATA
  workListTemplate[2:10,14+7] <- methodMS1
  workListTemplate[2:10, 6+7] <- paste0(instrumentPath, 
                                      Date, "_",batch,"W",Week,
                                      "_", chromPolAbbrev,
                                      "_", workListTemplate[2:10,2+7])
  workListTemplate[2:10, 6+7] <- paste0(workListTemplate[2:10, 6+7] ,
                                        "_", workListTemplate[2:10,3+7])
  for(i in 1:(length(sampleSet$sampleNames)*2)){
    workListTemplate[i+10, 6+7] <- paste0(instrumentPath, 
                                        Date,"_",batch,"W",Week, "_",chromPolAbbrev,
                                        "_", workListTemplate[i+10, 2+7],"_MS2_",
                                        workListTemplate[i+10,3+7])
    if(i%%2==1){
      workListTemplate[i+10, 6+7] <- str_replace(workListTemplate[i+10, 6+7], "_MS2", "-MS1")
    }
    else{
      workListTemplate[i+10, 6+7] <- str_replace(workListTemplate[i+10, 6+7], "-MS2_MS2_", "-MS2_")
    }
    workListTemplate[i+10, 14+7] <- ifelse((i%%2)==1, 
                                           methodMS1, 
                                           paste0(methodMS2,  "MetID", "\\", 
                                                  Date,"_",workListTemplate[i+10-1, 2+7],
                                                  "_",chromPolAbbrev,"-MS2",".m")
                                           )
  }

  #Removing all double dashes
  workListTemplate[,6+7] <- str_replace_all(workListTemplate[,6+7], pattern="\\\\", replacement="//")
  workListTemplate[,6+7] <- str_replace_all(workListTemplate[,6+7], pattern="//", replacement="/")
  workListTemplate[,14+7] <- str_replace_all(workListTemplate[,14+7], pattern="\\\\", replacement="//")
  workListTemplate[,14+7] <- str_replace_all(workListTemplate[,14+7], pattern="//", replacement="/")

  workListTemplate[,14+7] <- str_replace_all(workListTemplate[,14+7], 
                                             pattern="/", 
                                           replacement="\\\\")
  workListTemplate[,6+7] <- str_replace_all(workListTemplate[,6+7], 
                                             pattern="/", 
                                             replacement="\\\\")
  write.xlsx(workListTemplate, 
             paste0(format(Sys.Date(), 
                           format="%Y-%m-%d"),
                    "_WorkList_",
                    ifelse(chromPol=="RP","POS","NEG"),
                    ".xlsx"
                    ), 
             col.names=FALSE)
  saveRDS(sampleSet, paste0("Report",ifelse(chromPol=="RP","POS","NEG"),".rds"))
  names(sampleSet$sampleFeatures)<-sampleSet$sampleNames
  return(sampleSet)
  
}





