# StatTools
A collection of tools for random statistical (and other) tasks

## Tools
Tool | Description
:--- | :----------
AggregateOTUs | aggregation of microbiota OTUs on different taxonomic levels
AnDec | ANOVA Decomposition
biplotPCA | Make nice-looking score/loading biplot of PCA including several useful arguments for labels, colors, etc
filterClass | remove variables that are poorly populated by class
getTaxonomy | Extract phylogenetic information at different taxonomic levels from character strings
greyVec | convert numeric values to grey scale
mvImp | multivariate imputation (PLS and RF)
mvImpWrap | wrapper for mvImpWrap for automatic two-stage imputation and parallelisation
normalise | different normalisation options (ripped from some other package; don't remember which)
plotDens | 2D plot with histograms and densities on the margins
pStar | return stars from a vector of p-values
rfImp | deprecated. see mvImp
rfImpWrap | deprecated. see mvImpWrap
switchNames | switch names in a vector from old key to new key
venn | handy function to plot venn.diagram() in plot window

## Installation
Install `remotes` to be able to install packages from Git repositories.
```
install.packages('remotes')
```

Install `StatTools` package by:
```
library(remotes)
install_github('MetaboComp/StatTools')

```

## Version history
version | date | comment
:------ | :--- | :------
0.0.916 | 2021-07-15 | Added venn()
0.0.915 | 2020-06-09 | Added pStar()
0.0.914 | 2019-11-18 | Added biplotPCA()
0.0.913 | 2019-10-25 | Added getTaxonomy()
0.0.912 | 2019-10-23 | Added plotDens()
0.0.911 | 2019-10-03 | Added aggregateOTUs()
0.0.910 | 2018-09-17 | Long-time-no-update (just added mvImp())
0.0.900 | 2017-02-15 | AnDec()
