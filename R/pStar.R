#' Report "stars" for a vector of p-values
#'
#' @param p A vector of p-values
#'
#' @return A vector of significances according to the star system
#' @export
#'
#' @examples
#' set.seed(11)
#' p_values <- runif(20)
#' data.frame(p_values, p_star = pStar(p_values))
pStar <- function(p) {
  ifelse(p<0.001, '***', ifelse(p<0.01, '**', ifelse(p<0.05, '*', ifelse(p<0.1, "'", ''))))
}
