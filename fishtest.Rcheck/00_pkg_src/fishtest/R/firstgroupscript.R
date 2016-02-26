# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(devtools)
library(roxygen2)

library(plyr)
library(dplyr)
library(tidyr)

#' Filters our catches data
#'
#' @param data A dataframe.
#' @param species A character or string of characters.
#' @param region A character or string of characters.
#' @param function to be applied to groups of Reion and year
#' @return dataframe filtered by specicies and region and return of 'FUN'
#' @examples
#' select.catches(catches, 'Pargo', 'Golfito', mean)




select.catches <- function( data, species, region, fxn, fxn_col='fxn') {

  data %>%
    gather('month','catch',Jan:Dec) %>%
    select(-catch, -month) %>%
    filter( Species == c(species) & Region==c(region)) %>%
    group_by(Region, Year, Species) %>%
    summarise(fxn_col=fxn(Total)) %>%
    rename_(.dots = setNames('fxn_col', fxn_col))
}


