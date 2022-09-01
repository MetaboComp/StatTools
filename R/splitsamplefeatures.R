#' @param sampleNames A vector with sample names
#' @param sampleFeatures  A series of list that we want to investigate
#' @param rententiontime_threshold the difference of time point that we want to specify
#' @export
#' @return  A split list 
#' 

splitsamplefeatures<-function(sampleNames,
                              sampleFeatures,
                              rententiontime_threshold=20,
                              samplesplitlistmark="£"){





## for each sample name
for(i in 1:length(sampleNames)){   
  
  excludeList <- c()
  sampleSplitList <- list()
  sampleSplitList[[1]] <- as.data.frame(matrix(ncol=2))[-1,]
  colnames(sampleSplitList[[1]]) <- c("mz", "rt")
  
  
  
  ## j is how many features they have in the sample i
  for(j in 1:nrow(sampleFeatures[[i]])){
    
    #Skip already distributed masses and rts
    if(j %in% excludeList){
      next    ## if j in the inclusion list then skip this round
    }
    
    ## difference between each row and j 
    diffVec <- abs(sampleFeatures[[i]][,2]-sampleFeatures[[i]][j,2])
    
    diffVec[j] <- rententiontime_threshold+100000000000000 ### 
    
    if(any(diffVec < rententiontime_threshold)){
      ## the position of rows which has difference less than the threshold and that position 
      toSeparate <- c(j,
                      which(diffVec < rententiontime_threshold))  
      
      for(k in 1:length(toSeparate)){
        
        ## to separate is at least 1 because of j itself but it could not be one becasue of any(diffVec < 20)
        if(length(sampleSplitList) < k){
          sampleSplitList[[k]] <- as.data.frame(matrix(ncol=2))[-1,]
          colnames(sampleSplitList[[k]]) <- c("mz", "rt")
          ## add the features to the sample split list that do not have a closer rentention time
          sampleSplitList[[k]] <- rbind(sampleSplitList[[k]], 
                                        sampleFeatures[[i]][toSeparate[k], ])
        } else {
          ## if such list idoes not exist, create a new one
          #### MIGHT HAVE TO CHECK PREVIOUS SAMPLE FOR DISTANCE ####
          sampleSplitList[[k]] <- rbind(sampleSplitList[[k]],
                                        sampleFeatures[[i]][toSeparate[k], ])
        }
      }
      #Adding distributed samples to skip list
      excludeList <- c(excludeList, toSeparate)
    } else {
      ## if a feature does not need to split, add it to the shortest split list 
      ##for each sample i, there could be j features, there could be k in j features that needs to split
      ## then each k has a list of other features in that sample i that this features want to split from
      
      sampleSplitList[[which.min(sapply(sampleSplitList, nrow))]] <- rbind(sampleSplitList[[which.min(sapply(sampleSplitList, nrow))]], 
                                                                           sampleFeatures[[i]][j,])
    }
  }
  
  ## Still for the same sample i
  #If splits are needed, perform them
  
  if(length(sampleSplitList)>1){  
    ## it should be >1 is there is a sample split list. However, you do no tignore the scenario 
    ## that there is no sample split list and there is one empty sample split list 
    sampNumb <- 1
    
    for(j in (length(sampleNames)+1):(length(sampleNames)+length(sampleSplitList))){
      sampleNames <- c(sampleNames, 
                                 paste0(sampleNames[i],
                                        samplesplitlistmark,
                                        sampNumb))
      
      ## This is because the matrix has NA ine dfirst 2 rows
      # sampleSplitList[[sampNumb]] <- sampleSplitList[[sampNumb]][!is.na(sampleSplitList[[sampNumb]][,1]),]
      sampleFeatures[[j]] <- sampleSplitList[[sampNumb]]
      sampNumb <- sampNumb+1
    }
    ## abort the original sample names and features because it is already double counted in the sample splitlist 
    sampleNames[i] <- NA
    sampleFeatures[[i]] <- NA
  }
}

sampleNames <- sampleNames[!is.na(sampleNames)]
sampleFeatures <- sampleFeatures[!is.na(sampleFeatures)]
sampleSet<-list()
sampleSet$sampleNames <-sampleNames 
sampleSet$sampleFeatures<-sampleFeatures
return(sampleSet)
}

