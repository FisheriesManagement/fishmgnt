
#' Catch prices
#' Function to calculate price per catch of a given species,  year and region
#' @parameter data, dataframe to be cleaned
#' @parameter price, value or series of values
#' @parameter  species, charater or strem of characters
#' @parameter year, value or series of values
#' @parameter region, charater or strem of characters


library(devtools)
library(roxygen2)
library(dplyr)
library(tidyr)

load_all()

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

catchprice(catches, 2000, 'Cabrilla', c(1990, 1995), c('Guanacaste'))


