
#' Catch prices
#'
#' Function to calculate price per catch of a given species,  year and region
#'
#' @param data, dataframe to be cleaned
#' @param price, value or series of values
#' @param  species, charater or strem of characters
#' @param year, value or series of values
#' @param region, charater or strem of characters
#' @examples
#' catchprice(catches, 2000, 'Cabrilla', c(1990, 1995), c('Guanacaste'))

# library(devtools)
# library(roxygen2)
# library(dplyr)
# library(tidyr)


catchprice <- function(data, price, species, year, region){

  d=data %>%
    gather('month','catch', Jan:Dec) %>%
    select(-catch, -month) %>%
    filter(Species == species & Year == c(year) & Region == c(region)) %>%
    group_by(Year, Species) %>%
    summarize(meancatch = mean(Total, na.rm=T))

  p = price
  pkg <- d$meancatch*p

  return(pkg)
}




