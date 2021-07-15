#' Plot Venn diagram 
#'
#' Convenience function to draw a Venn diagram directly in the plot window without exporting to a file
#' @param x Same as for the `venn.diagram()` function, i.e. a list of names
#' @param ... Standard `venn.diagram()` arguments 
#'
#' @return
#' @export
#'
#' @examples
#' StatTools::venn(x = list(set1 = sample(letters)[1:10], set1 = sample(letters)[1:15], set1 = sample(letters)[1:20]))
venn <- function(x, ...){
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  grid::grid.newpage()
  venn_object <- VennDiagram::venn.diagram(x, filename = NULL, ...)
  grid::grid.draw(venn_object)
}