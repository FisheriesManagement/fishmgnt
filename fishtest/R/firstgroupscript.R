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

#inputs for regions need to be in ''
#EX: 'Pargo'

select.catches <- function(data,species, region) {
                      out <- data %>%
                        gather('month','catch',Jan:Dec) %>%
                        select(-catch, -month) %>%
                        filter( Species == species & Region==region)
            #             group_by(Region, Year) %>%
            #             summarise(sum=sum(Total))

                      return(out)
}
