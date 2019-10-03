#' Aggregate OTUs on different taxonomic levels
#'
#' @param otu a table of OTUs (columns) with samples in rows
#' @param tax a table with classification (levels {kingdom, phylum, etc} in columns and OTUs in rows)
#' @param level Which level to aggregate on (corresponding to column in `tax`)
#'
#' @return A list with:
#' @return `tax` Taxonomy up to the level of aggregation
#' @return `name` Full name of aggregated variables
#' @return `count` A table with aggregated counts
#' @export
#'
#' @examples
#' # genus <- aggregateOTUs(inData,taxonomyDF,6)
aggregateOTUs <- function(otu,tax,level) {
  library(dplyr)
  otu <- t(otu)
  tax <- tax[,1:level]
  otu <- cbind(tax,otu)
  count <- otu %>% group_by_at(colnames(tax)) %>% summarise_if(is.numeric,sum,na.rm=T)
  tax <- count[,1:level]
  name <- apply(tax,1,function(x) paste(x, collapse = ' '))
  count <- t(count[,-1:-level])
  colnames(count) <- name
  count <- as.data.frame(count)
  return(list(tax=tax, name=name, count=count))
}

