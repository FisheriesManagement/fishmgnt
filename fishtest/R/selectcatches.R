

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
#' select.catches(catches, c('Pargo', 'Pargo Seda'), 'Golfito', mean)





selectcatches <- function( data, species, region, fxn) {

  data %>%
    gather('month','catch', Jan:Dec) %>%
    select(-catch, -month) %>%
    filter( Species == c(species) & Region==c(region)) %>%
    group_by(Region, Year, Species) %>%
    summarise(fxn(Total))

}


