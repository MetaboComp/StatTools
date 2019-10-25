#' Extract phylogenetic information at different taxonomic levels from character strings
#'
#' Often, OTUs are delivered with taxonomic information in string format:  
#' "D_0__Bacteria;D_1__Firmicutes;D_2__Clostridia;D_3__Clostridiales;D_4__Lachnospiraceae;D_5__Blautia" ...  
#' Rather than in speadsheets (data frames).  
#' This function provides a simple algorithm to extract phylogenetic information at different taxonomic levels 
#' and report them in a data frame format.
#' @param taxon Character with OTU taxonomic information (e.g. "D_0__Bacteria;D_1__Firmicutes;D_2__Clostridia;D_3__Clostridiales;D_4__Lachnospiraceae;D_5__Blautia" ...)
#' @param split1 Splitting character between taxonomic levels (e.g. ";" which is default)
#' @param split2 Splitting character within taxonomic levels (e.g. "__" which is default)
#' @param depth At what taxonomic level to truncate (defaults to 6 which normally corresponds to genus) 
#'
#' @return A data frame with taxonomic level information in columns
#' @export
#'
#' @examples
#' # load(file='MBBL.rda')
#' # taxDF <- getTaxonomy(tax$Taxon)
getTaxonomy <- function(taxon, split1=';', split2='__', depth=6) {
  nTaxon <- length(taxon)
  taxList <- strsplit(taxon, split = split1)
  taxLength <- sapply(taxList, length)
  taxMatrix <- matrix('',nrow=nTaxon, ncol=max(taxLength))
  nameIndex <- which(taxLength==max(taxLength))[1]
  for (i in 1:nTaxon) {
    taxiList <- taxList[[i]]
    taxiList <- strsplit(taxiList, split2) 
    wh1 <- which(sapply(taxiList, length)==1)
    for(j in wh1) taxiList[[j]] <- c(taxiList[[j]],'')
    taxiList <- taxiList %>% unlist() %>% matrix(.,ncol=2, byrow=T)
    taxMatrix[i,1:taxLength[i]] <- taxiList[,2]
    if (i==nameIndex) colnames(taxMatrix) <- taxiList[,1]
  }
  taxMatrix <- data.frame(taxMatrix[,1:depth])
}