#' This is for ignore loop errors (caused by extreme scenario of resampling or something else). 
#' You should run the code and know for sure the error is not a "hard error"
#' @param rep  loop number
#' @param empty_name  chunk of code, building empty object for saving results
#'                                     the code should be written in "..
#'                                                                  .. " With lines separate
#' @param main_function chunk of code, the main function to compute the result
#'                                     the code should be written in "..
#'                                                                  .. " With lines separate
#' @param record_values chunk of code, to save the result
#'                                     the code should be written in "..
#'                                                                  .. " With lines separate
#' @param output the variables names you want to save as output "", should be something in the recorded values
#' @return result
#' @export    
#   
# Note that in the "...", if there is a charater you need to specifiy, use ' '
# Note that you must guarantee that the  

##################################################################################################
###example
#library(MUVR)
#library(StatTools)
#rep=3
#dataX=Xotu
#dataY=Yotu
#empty_name<-"BER_vector_modelRF_MUVR_perm<-vector()
#            MISS_vector_modelRF_MUVR_perm<-vector()"
#
#main_function<-"perm=MUVR(X=dataX,Y=dataYPerm,method='RF',
#                DA=T, modReturn = T, nRep =2, nOuter=4)"
#record_values<-"BER_vector_modelRF_MUVR_perm<-c(BER_vector_modelRF_MUVR_perm,
#                                perm$ber['min'])
#MISS_vector_modelRF_MUVR_perm<-c(MISS_vector_modelRF_MUVR_perm,
#                                 perm$miss['min'])"
# output=c("BER_vector_modelRF_MUVR_perm","MISS_vector_modelRF_MUVR_perm")
#sss<-ignore_loop_error(rep,
#                 empty_name,
#                 main_function,
#                 record_values,
#                 output)

ignore_loop_error<-function(rep,
                            empty_name,
                            main_function,
                            record_values,
                            output){
result<-list()  
pp<-1
p<-1

########################################
#### empty name
eval(parse(text=paste(empty_name)))
###################################################33

while(p<rep){
  p<-pp
  cat("/permutation",p,"of",rep)
  dataYPerm = sampling_from_distribution(dataY)
  
  tryCatch(  {pp<-p
  ###################################################################
  ##main function
  eval(parse(text=paste(main_function)))
  ################################################################
  
  pp<- p+1

  ######################################################################
  ## recording
  eval(parse(text=paste(record_values)))
  ####################################################################
  
  
  },error=function(e){})
  
}

for(i in 1:length(output)){
  result[[i]]<-eval(parse(text=paste(output[i])))
}
names(result)<-output
return(result)
}

