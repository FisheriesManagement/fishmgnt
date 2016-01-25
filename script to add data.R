# Fisheries management project - Catch MSY
#Alex, Caio, Juliana

# 1. Stacking data and selecting 
# add all the packages we need to read the data:
library(ggplot2) # This is to make pretty graphs
library(plyr) #
library(dplyr) #
library(tidyr) # to stack data
library(readxl) # To read excel sheets

# function to read all sheets in workbook
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

# read in all sheets


#dim(dat) dim measure the dimensions of a dataframe or matrix 

# head(dat[1:6,c('Species','Jan')])

#class tell you what kind of thing a thing is?

dat <- ldply(read_excel_allsheets('incopesca.xlsx'))

sapply(dat, class)


flat_dat <- (dat) %>% 
  select(-Total, -Year) %>%
  rename(Year = .id) %>% 
  gather('month','catch',Jan:Dec) %>%
  subset(Region == 'Guanacaste')

flat_func <- function(dat){
  
}

snapper <- flat_dat %>%
  filter(Species == 'PARGO')
  #subset(Species == 'PARGO') #Clean data set of only catches in Guanacaste = "snapper"
View(snapper)

snapper[snapper == 0] <- NA

meansnapper <- snapper %>% 
  group_by(Year,month) %>% 
  summarise(mean = mean(catch, na.rm = TRUE))
View(meansnapper)

library(xlsx)
write.xlsx(x = snapper, file= "snappertest.xlsx", sheetName = "test", row.names = FALSE)
