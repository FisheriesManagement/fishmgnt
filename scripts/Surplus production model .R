# Surplus production model
# 1/26/16

# Clear workspace/global environment
rm(list = ls())

# Load packages
library(ggplot2) # This is to make pretty graphs
library(plyr) #
library(dplyr) #
library(tidyr) # to stack data
library(readxl) # To read excel sheets

# Load data
cpue_data = read.csv('cpuedata.csv',stringsAsFactors = FALSE) %>% # Data: time series of CPUE in Coyote/Bejuco (2007 - 2013)
  select(Year, Average.Per.Trip, Average.Per.Hook) %>%
  gather("type","cpue", Average.Per.Trip: Average.Per.Hook) # Flattening/stacking the two types of cpue data we have into "Type" and "Value" columns

colnames(cpue_data) <- tolower(colnames(cpue_data)) # Dan likes lower case. Making column names lower case. 

snapper_data = read.csv('snappertest.csv',stringsAsFactors = FALSE) %>% # Data: time series of cathes in Guanacaste (1990 - 2013)
  group_by(Year,Region,Species) %>%
  summarize(yearly_catch = sum(catch, na.rm = T)) # Summing the repeated "Pargo" values for each year. 

colnames(snapper_data) <- tolower(colnames(snapper_data))
  

# Snapper model - Schaffer (Logistic Production Model)

snapper_model <- function(r = .1,k = 100,catch, init_depletion = 0){ # model needs values of r, k, catch, and initial depletion
  
  catch <- snapper_data # renaming snapper_data as catch
  
  catch_years <- (unique(catch$year)) # make a vector of each unique year value in the data set
  
  years <- min(catch_years):max(catch_years) # makes a vector from starting year to ending year of data set
  
  outs <- data_frame(year = years, biomass = NA) %>% # Making data frame with blank biomass column to be filled
    left_join(catch[,c('year','yearly_catch')], by = 'year')
  
  outs$biomass[1] <- k*(1-init_depletion) #Biomass in the first year is equal to k*(1-initial depletion) - might assume that biomass in first year = k (init_depletion = 0)
  
  for (i in 1:(length(years)-1)) # loop running equation for each year of data inputted (not year 1 - specified above)
  {
    
    outs$biomass[i + 1] <- pmax(.001,outs$biomass[i] + r*outs$biomass[i]*(1-(outs$biomass[i]/k)) - outs$yearly_catch[i])
    
  }
  
  return(list(outs = outs)) # Getting the outs data frame in the form of a list
  
}


######################
# Wrapping function that links our snapper model to the optimization thingy by something called "pars" (r, K, q), cpue, catch, and init_depletion

fit_model <- function(pars,cpue,catch, init_depletion = 0){
  
  #pars[1] == r
  #pars[2] == k
  #pars[3] == q
  
  pop_predict <- snapper_model(r = pars[1],k = pars[2],catch = catch, init_depletion = init_depletion)$outs %>% # $ outs makes list output from last section work
        left_join(cpue,by = 'year')
  
  pop_predict$cpue_predict <- pars[3]*pop_predict$biomass
  
  
  pop_predict$sq_diff <- (pop_predict$cpue - pop_predict$cpue_predict)^2
  
  
  sum_of_squares <- sum(pop_predict$sq_diff, na.rm = T)
  
  return(sum_of_squares) # output of fit_model is the sum of squares between predicted cpue and observed cpue
  
}

######################
# Using the optim function to optimises the fit_model parameters (reducing sum of squares) which then go into the snapper model to predict biomass.

fitted_model <- optim(par = c(.6,1000*max(snapper_data$yearly_catch, na.rm = T),.0001), fn = fit_model, cpue = cpue_data, catch = snapper_data,init_depletion = 0, lower = c(0.5,max(snapper_data$yearly_catch, na.rm=T),1e-6) , upper = c(10,1000*max(snapper_data$yearly_catch, na.rm = T),1)) # it's a vector, the "optim (par = c(r, k, q))" that's the order of things it shoots.


possibles <- expand.grid(r = .1:.6,K = 10:100,q = 0:1)

for (i in 1:dim(possibles)[1])
{
  
}




