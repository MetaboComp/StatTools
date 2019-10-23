#' Make XY scatterplot with X and Y histograms and densities on the margins
#'
#' @param x Vector of data
#' @param y Vector ofsame length as x
#' @param xlab Optional x label (otherwise defaults to the name of x)
#' @param ylab Optional y label (otherwise defaults to the name of y)
#' @param export Option to export to png or pdf
#' @param breaks number of breaks in the margin histograms
#' @param ... 
#'
#' @return A 2D plot
#' @export
#'
#' @examples
#' randx <- runif(100)
#' randy <- runif(100)
#' plotDens(randx, randy)
plotDens <- function(x, y, xlab, ylab, export=FALSE, breaks=30, ...){
  ## any export?
  if (export=='png') png(width=1024,height=1024,pointsize=20,filename=paste0('2D','.png'))
  if (export=='pdf') pdf(width=10,height=10,file=paste0('2D','.pdf'))
  ## check input & specify some parameters
  ## labels
  if (missing(xlab)) xlab <- deparse(substitute(x))
  if (missing(ylab)) ylab <- deparse(substitute(y))
  ## set up layout and graphical parameters
  xrange <- range(x)
  yrange <- range(y)
  xdens <- density(x)
  ydens <- density(y)
  layMat <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(layMat, widths=c(5/7, 2/7), heights=c(2/7, 5/7))
  ## scatter plot
  par(mar=c(4,4,0,0))
  plot(x, y, xlim=xrange, ylim=yrange, bty='l', xlab=xlab, ylab = ylab, las=1)
  ## X histogram and density 
  par(mar=c(0,4,0,0))
  # xhist <- hist(x, breaks=seq(from=min(x), to=max(x), length.out=breaks), xlab='', ylab='', main='', col='lightgrey', axes=F)
  xhist <- hist(x, breaks=seq(from=min(x), to=max(x), length.out=breaks), plot = FALSE)
  barplot(xhist$counts, axes = F, horiz=FALSE, xlab='', ylab='', main='', col='lightgrey')
  par(new=T)
  par(mar=c(0,4,0,0))
  plot(xdens$x, xdens$y, type='l', xlim=xrange, axes=F, bty='n', xlab='', ylab='', main='') # line
  # lines(xdens$x, xdens$y) # line
  ## X histogram and density 
  # yhist <- hist(y, breaks=seq(from=min(y), to=max(y), length.out=breaks), ylim = yrange, xlab='', ylab='', main='', col='lightgrey')
  par(mar=c(4,0,0,0))
  yhist <- hist(y, breaks=seq(from=min(y), to=max(y), length.out=breaks), plot = FALSE)
  barplot(yhist$counts, axes = F, horiz=TRUE, xlab='', ylab='', main='', col='lightgrey')
  par(new=T)
  par(mar=c(4,0,0,0))
  plot(ydens$y, ydens$x, type='l', ylim=yrange, axes=F, bty='n', xlab='', ylab='', main='') # line
  # lines(xdens$x, xdens$y) # line
  if(!isFALSE(export)) dev.off()
}
