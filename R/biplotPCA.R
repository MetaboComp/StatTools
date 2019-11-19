#' PCA biplot
#'
#' Makes a biplot of a PCA object from prcomp()
#'
#' @param pca A PCA model from prcomp()
#' @param comps Which components to plot
#' @param xCol (Optional) Continuous vector for grey scale gradient of observation (sample) color (e.g. continuous Y vector)
#' @param labPlSc Boolean to plot observation (sample) names (defaults to TRUE)
#' @param labs (Optional) Label names
#' @param vars Which variables to plot (names in rownames of loadings (i.e. rotation in prcomp))
#' @param labPlLo Boolean to plot variable names (defaults to TRUE)
#' @param pchSc Plotting character for observation scores
#' @param colSc Colors for observation scores (only if xCol omitted)
#' @param colLo Colors for variable loadings (defaults to red)
#' @param supLeg Boolean for whether to suppress legends
#'
#' @return A PCA biplot
#' @export
biplotPCA=function(pca,comps=1:2,xCol,labPlSc=TRUE,labs,vars,labPlLo=TRUE,pchSc=16,colSc,colLo=2,supLeg=FALSE) {
  cex=par()$cex
  par(mar=c(4,4,4,4)+.5)
  scores=pca$x[,comps]
  if(is.null(rownames(scores))) rownames(scores)=paste('O',1:nrow(scores),sep='')
  loads=pca$rotation[,comps]
  if(missing(vars)) vars=rownames(loads)
  loads=loads[rownames(loads)%in%vars,]
  nSamp=nrow(scores)
  nVar=nrow(loads)
  if(missing(xCol)) {
    if (missing(colSc)) {
      colSc=rep(1,nSamp)
      legPlot=FALSE
    } else {
      colScLeg=colSc
      colSc=as.factor(colSc)
      legPlot=TRUE
    }
  } else {
    x.col=10+round(85*((max(xCol)-xCol)/(diff(range(xCol)))))
    colSc=paste("gray",x.col,sep="")
    legPlot=TRUE
  }
  if(supLeg) legPlot=FALSE
  rLo=max(abs(loads))
  rLo=1.1*c(-rLo,rLo)
  pcVar=summary(pca)$importance[2,]
  xlab=paste('PC',comps[1],' scores (R2X=',signif(pcVar[comps[1]],3),')',sep='')
  ylab=paste('PC',comps[2],' scores (R2X=',signif(pcVar[comps[2]],3),')',sep='')
  plot(loads,xlim=rLo,ylim=rLo,type='n',xlab=xlab,ylab=ylab,main='',axes=F)
  box(bty='o')
  axis(3)
  axis(4,las=1)
  mtext('Loadings',3,line=3,cex=cex)
  mtext('Loadings',4,line=3,cex=cex)
  abline(h=0,v=0,lty=2,col='grey')
  arrows(rep(0,nrow(loads)),rep(0,nrow(loads)),loads[,1],loads[,2],col=colLo)
  if(labPlLo) text(1.1*loads,rownames(loads),cex=.7*cex,font=3)
  par(new=T)
  rSc=max(abs(scores))
  rSc=c(-rSc,rSc)
  plot(scores,col=colSc,pch=pchSc,ylim=rSc,xlim=rSc,bty='l',xlab='',ylab='',axes=F)
  axis(1)
  axis(2,las=1)
  if (labPlSc) {
    if (missing(labs)) labs=rownames(scores)
    # cat(labs)
    text(scores,as.character(labs),cex=.7*cex,pos=3)
  }
  if (legPlot) {
    if (missing(xCol)) {
      legend('topleft',legend=unique(colScLeg),pch=unique(pchSc),col=unique(colSc),bty='n')
    } else {
      whUnik=!duplicated(xCol)
      unik=xCol[whUnik]
      cols=colSc[whUnik][order(unik)]
      unik=sort(unik)
      if (length(unik)>10) {
        k=(length(unik)-1)/5
        n=1+k*0:5
        cols=cols[n]
        unik=unik[n]
      }
      legend('topleft',legend=signif(unik,3),fill=cols,bty='n')
    }
  }
}
