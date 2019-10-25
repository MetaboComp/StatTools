# StatTools
A collection of tools for random statistical (and other) tasks

## Tools
Tool | Description
:--- | :----------
AggregateOTUs | aggregation of microbiota OTUs on different taxonomic levels
AnDec | ANOVA Decomposition
filterClass | remove variables that are poorly populated by class
getTaxonomy | Extract phylogenetic information at different taxonomic levels from character strings
greyVec | convert numeric values to grey scale
mvImp | multivariate imputation (PLS and RF)
mvImpWrap | wrapper for mvImpWrap for automatic two-stage imputation and parallelisation
normalise | different normalisation options (ripped from some other package; don't remember which)
plotDens | 2D plot with histograms and densities on the margins
rfImp | deprecated. see mvImp
rfImpWrap | deprecated. see mvImpWrap
switchNames | switch names in a vector from old key to new key

## Installation
Install `devtools` to be able to install packages from Git repositories.

Install `StatTools` package by:

`devtools::install_git("https://gitlab.com/CarlBrunius/StatTools.git")`

## Version history
version | date | comment
:------ | :--- | :------
0.0.913 | 2019-10-25 | Added getTaxonomy()
0.0.912 | 2019-10-23 | Added plotDens()
0.0.911 | 2019-10-03 | Added aggregateOTUs()
0.0.910 | 2018-09-17 | Long-time-no-update (just added mvImp())
0.0.900 | 2017-02-15 | AnDec()
