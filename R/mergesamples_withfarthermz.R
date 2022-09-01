#' @param featureDir A csv file, single column features pasted from MUVR or wherever, need to look like this: "mz@rt"
#' @param saveDir A directory you want to  
#' @param Matrix_Obj A matrix with rows as as sample and columns as features
#' The rownames are names in the format??, colnames are feature names
#' @param rtOrNot Whether to use RT window when doing MS2 or not, default = False because usually we won't know where something is and there used to be trouble with the window historically
#' @param chromPol Either RN or RP
#' @param instrumentName Either Zoidberg or Fry
#' @export
#' 
mergesamples_withfarthermz<-function(){}