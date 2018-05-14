#' Filter out features based on presence in classes
#' 
#' To be kept, a feature need to be present in ≥`ratio` in ≥class
#' @param table Feature matrix (samples in rows, features in columns)
#' @param classes Vector of sample classes
#' @param ratio Ratio for decision keep/discard
#'
#' @return Feature table filtered by class presence 
#' @export
#'
#' @examples
#' otuRW=StatTools::filterClass(otuRW,rep(c('rye','wheat'),each=nRW),ratio = 1/3)
filterClass=function(table,classes,ratio=.5) {
  Cls=sort(unique(classes))
  nSamp=length(classes)
  keep=rep(F,ncol(table))
  for (f in 1:ncol(table)) {
    vector=table[,f]
    for (c in Cls) {
      vectClass=vector[classes==c]
      nClass=length(vectClass)
      keep[f]=keep[f]|((sum(vectClass!=0)/nClass)>=ratio)
    }
  }
  return(table[,keep])
}