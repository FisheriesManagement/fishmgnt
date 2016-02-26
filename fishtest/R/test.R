# function that calculates values of price per kg of a species of interest.
# arguments are data of catches (whole data set), price, species and year
# returns price

library(devtools)
library(roxygen2)
library(plyr)
library(dplyr)
library(tidyr)


# load_all()

test <- function(catch, price, species, year){

  data <- catch %>%
    gather('month','catch',Jan:Dec) %>%
    select(-catch, -month) %>%
    filter(Species==species, Year==year) %>%
    group_by(Year, Species) %>%
    summarize(meanprice = mean(Total, na.rm=T))

  pkg <- data*price

  return(pkg)
}

source('centraltendency.R')

test(catches,'2000','Cabrilla','1990')
