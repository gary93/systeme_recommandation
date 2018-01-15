#
## Download R packages.
##

packages = c("jsonlite", "dplyr", "recommenderlab", "data.table", "tidytext", "tidyr", "stringr", "gridExtra", "ggplot2")
packages = lapply(packages, FUN = function(x) {
    if(!require(x, character.only = TRUE)) {
        install.packages(x)
        library(x, character.only = TRUE)
    }
})
