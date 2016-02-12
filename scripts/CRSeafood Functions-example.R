#===================================================================

# Using optim in R to fit a logistic growth model to Costa Rican snapper

# 'catches.csv' -> gives regional (Guanacaste) catches between 1990 - 2013
# 'CPUE.csv' -> gives local catch per unit effort (number caught per trip) between 2007 - 2013

# Modified by Kat Millage from a script written by Trevor A. Branch tbranch@uw.edu

# Last revised 2/2/16

#====================================================================

#===PART 1=======================================
# Loading packages
#================================================

library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)

#===PART 2=======================================
# Logistic growth equation, takes parameters 
# Uses CPUE data to calculate NLL for those years
# N0 (numbers in 1990), r (rate of increase), K (carrying capacity), startyear and endyear
#================================================

getNLL <- function(par, catches, CPUE, plot=F) {
  
  r <- par[1]
  K <- par[2]
  q <- par[3]

  start.yr <- min(catches$Year)
  end.yr <- max(catches$Year)
  years <- start.yr:end.yr
  nyr <- length(years)
  
  B <- vector(mode = 'numeric',length = nyr)
  B[1] <- K*0.8
  
  NLL <- vector(mode = 'numeric', length=nyr)
  obsB <- vector(mode = 'numeric', length=nyr)
  obsCPUE <- vector(mode = 'numeric', length=nyr)
  
  #Logistic growth model fitting. max() ensures it does not go to 0
  for(i in 1:(nyr-1)){
   B[i+1] = max(0.001, B[i] + r*B[i]*(1-B[i]/K) - catches[i,2])
  }
  
  #Finds current year of the model and checks to see if there is CPUE for that year
  for (i in 1:nyr) {
    current.yr <- years[i]
    
    #If there is CPUE data in that year then NLL is calculated
    if (current.yr %in% CPUE[,1]) {
      obsCPUE[i] <- CPUE[CPUE[,1]==current.yr,2]
      obsCV <- CPUE[CPUE[,1]==current.yr,3]
      predCPUE <- B[i]*q
     
      NLL[i] = (log(obsCPUE[i]/predCPUE))^2/(2*obsCV^2)
      
      obsB[i] <- obsCPUE[i]/q
    }
    
    else {NLL[i]=NA
          obsB[i]=NA}
  }
  
  #Combines predicted biomass and calculated NLL for each year into a data frame
  Results <- data.frame(Year = years, Catch = catches[,2], Biomass = B, ObsB = obsB, Like = NLL)
  
  NLLSum <- sum(Results$Like, na.rm=T)
  
  #PLOTTING PART
  if (plot==T) {
    par(mfrow=c(2,1), mar=c(2,1,1,1), oma=c(3,3,1,1))
    plot(x=years,y=Results$Biomass, lwd=2,col="black",type="l", yaxs="i",
        ylim=c(0,1.05*max(Results$Biomass)), ylab = "Biomass (kg)", xlab = "Year")
    points(x=years, y=Results$ObsB, col="black", pch=16)
    mtext(side=2,"Biomass (kg)", line=2.5)
    
    plot(x=years,y=Results$Catch, lwd=2,col="blue",type="l",yaxs="i", 
         ylim=c(0,1.1*max(Results$Catch)), xlab = "Year", ylab = 'Catch (kg)')
    mtext(side=2,"Catch (kg)", line=2.5)
  }
  
  invisible(Results)  #return the biomass and NLL
  return(NLLSum)
}


#===PART 3=======================================
# Optimizing parameters for model
#================================================
  
BestModel <- optim(par = c(.5,1000*max(catches$catch, na.rm = T),.00001), 
                   fn = getNLL, 
                   CPUE = CPUE, 
                   catches = catches, 
                   plot=F, 
                   lower = c(0.1,max(catches$catch, na.rm=T),1e-6) , 
                   upper = c(3,1000*max(catches$catch, na.rm = T),1), 
                   control=list(maxit=20000))

#Possibles <- function()

possibles <- expand.grid(r = .05:.6,K = 1000000:10000000,q = 0.0000001:1)

for (i in 1:dim(possibles)[1])
{
  Parameters <- BestModel[i]
}

#===PART 3=======================================
# Making likelihood profiles for our parameters
#================================================

Rprofile.NLL <- function(par, catches, CPUE, r) {
  
  K <- par[1]
  q <- par[2]
  
  start.yr <- min(catches$Year)
  end.yr <- max(catches$Year)
  years <- start.yr:end.yr
  nyr <- length(years)
  
  B <- vector(mode = 'numeric',length = nyr)
  B[1] <- K*0.8
  
  NLL <- vector(mode = 'numeric', length=nyr)
  obsB <- vector(mode = 'numeric', length=nyr)
  obsCPUE <- vector(mode = 'numeric', length=nyr)
  
  #Logistic growth model fitting. max() ensures it does not go to 0
  for(i in 1:(nyr-1)){
    B[i+1] = max(0.001, B[i] + r*B[i]*(1-B[i]/K) - catches[i,2])
  }
  
  #Finds current year of the model and checks to see if there is CPUE for that year
  for (i in 1:nyr) {
    current.yr <- years[i]
    
    #If there is CPUE data in that year then NLL is calculated
    if (current.yr %in% CPUE[,1]) {
      obsCPUE[i] <- CPUE[CPUE[,1]==current.yr,2]
      obsCV <- CPUE[CPUE[,1]==current.yr,3]
      predCPUE <- B[i]*q
      
      NLL[i] = (log(obsCPUE[i]/predCPUE))^2/(2*obsCV^2)
      
      obsB[i] <- obsCPUE[i]/q
    }
    
    else {NLL[i]=NA
          obsB[i]=NA}
  }
  
  #Combines predicted biomass and calculated NLL for each year into a data frame
  Results <- data.frame(Year = years, Catch = catches[,2], Biomass = B, ObsB = obsB, Like = NLL)
  
  NLLSum <- sum(Results$Like, na.rm=T)
  
  return(NLLSum)
  
}

#Rprofile.NLL(par = c(7000000, 0.00001), catches, CPUE, r=0.25)
#optim(par = c(7000000, 0.00001), fn=Rprofile.NLL, catches = catches, CPUE = CPUE, method="L-BFGS-B", r=0.25, lower=c(1000000, 0.000001), upper=c(10000000, 0.1))

# Actually does the likelihood profile
Rprofile <- function(R.vec, lower=c(1000000, 0.000001), upper=c(10000000, 0.1)) {
  nR <- length(R.vec)
  saved.NLL <- vector(length=nR)
  
  for (i in 1:nR) {
    x <- optim(par = c(7000000, 0.00001), 
               fn=Rprofile.NLL, 
               catches = catches, 
               CPUE = CPUE, 
               method="L-BFGS-B", 
               r=R.vec[i], 
               lower=c(1000000, 0.000001), 
               upper=c(10000000, 0.1))
    
    print(paste("Run",i,"of",nR))
    
    saved.NLL[i] <- x$value
  }
  
  plot(x=R.vec, y=saved.NLL, type="l",xaxs="i", las=1, ylab="NLL", xlab="r values", 
       lwd=2, col="blue")   
  abline(h=min(saved.NLL), lty=2, col="gray50")
  abline(h=min(saved.NLL)+1.92, lty=2, col="gray50")  #95% CI
  
  return(saved.NLL)
}

#Profile for K
Kprofile.NLL <- function(par, catches, CPUE, K) {
  
  r <- par[1]
  q <- par[2]
  
  start.yr <- min(catches$Year)
  end.yr <- max(catches$Year)
  years <- start.yr:end.yr
  nyr <- length(years)
  
  B <- vector(mode = 'numeric',length = nyr)
  B[1] <- K*0.8
  
  NLL <- vector(mode = 'numeric', length=nyr)
  obsB <- vector(mode = 'numeric', length=nyr)
  obsCPUE <- vector(mode = 'numeric', length=nyr)
  
  #Logistic growth model fitting. max() ensures it does not go to 0
  for(i in 1:(nyr-1)){
    B[i+1] = max(0.001, B[i] + r*B[i]*(1-B[i]/K) - catches[i,2])
  }
  
  #Finds current year of the model and checks to see if there is CPUE for that year
  for (i in 1:nyr) {
    current.yr <- years[i]
    
    #If there is CPUE data in that year then NLL is calculated
    if (current.yr %in% CPUE[,1]) {
      obsCPUE[i] <- CPUE[CPUE[,1]==current.yr,2]
      obsCV <- CPUE[CPUE[,1]==current.yr,3]
      predCPUE <- B[i]*q
      
      NLL[i] = (log(obsCPUE[i]/predCPUE))^2/(2*obsCV^2)
      
      obsB[i] <- obsCPUE[i]/q
    }
    
    else {NLL[i]=NA
          obsB[i]=NA}
  }
  
  #Combines predicted biomass and calculated NLL for each year into a data frame
  Results <- data.frame(Year = years, Catch = catches[,2], Biomass = B, ObsB = obsB, Like = NLL)
  
  NLLSum <- sum(Results$Like, na.rm=T)
  
  return(NLLSum)
  
}

# Actually does the likelihood profile
Kprofile <- function(K.vec, lower=c(0.05, 0.000001), upper=c(0.6, 0.1)) {
  nR <- length(K.vec)
  saved.NLL <- vector(length=nR)
  
  for (i in 1:nR) {
    y <- optim(par = c(7000000, 0.00001), 
               fn=Kprofile.NLL, 
               catches = catches, 
               CPUE = CPUE, 
               method="L-BFGS-B", 
               K=K.vec[i], 
               lower=c(0.05, 0.000001), 
               upper=c(0.6, 0.1))
    
    print(paste("Run",i,"of",nR))
    
    saved.NLL[i] <- y$value
  }
  
  plot(x=K.vec, y=saved.NLL, type="l",xaxs="i", las=1, ylab="NLL", xlab="K values", 
       lwd=2, col="red")   
  abline(h=min(saved.NLL), lty=2, col="gray50")
  abline(h=min(saved.NLL)+1.92, lty=2, col="gray50")  #95% CI
  
  return(saved.NLL)
}

##===PART 4=======================================
# Plotting the catch and biomass projections
#================================================

Project.Model <- function(par, catches, CPUE, proj.yr.end, E.proj, plot=T) {
  
  r <- par[1]
  K <- par[2]
  q <- par[3]
  
  start.yr <- min(catches$Year)
  end.yr <- max(catches$Year)
  years <- start.yr:end.yr
  nyr <- length(years)
  
  B <- vector(mode = 'numeric',length = nyr)
  B[1] <- K*0.8
  
  E <- vector(mode = 'numeric', length=nyr)
  
  obsB <- vector(mode = 'numeric', length=nyr)
  obsCPUE <- vector(mode = 'numeric', length=nyr)
  
  #Logistic growth model fitting. max() ensures it does not go to 0
  for(i in 1:(nyr-1)){
    B[i+1] = max(0.001, B[i] + r*B[i]*(1-B[i]/K) - catches[i,2])
  }
  
  for(i in 1:(nyr)){
    E[i] = catches[i,2]/(q*B[i])
  }
  
  #Finds current year of the model and checks to see if there is CPUE for that year
  for (i in 1:nyr) {
    current.yr <- years[i]
    
    #If there is CPUE data in that year then NLL is calculated
    if (current.yr %in% CPUE[,1]) {
      obsCPUE[i] <- CPUE[CPUE[,1]==current.yr,2]
      obsCV <- CPUE[CPUE[,1]==current.yr,3]
      predCPUE <- B[i]*q
      
      obsB[i] <- obsCPUE[i]/q
      predC <- predCPUE
    }
    
    else {obsB[i]=NA}
  }
  
  Results1 <- data.frame(Year = years, Catch = catches[,2], Biomass = B, ObsB = obsB, Effort = E)
  
  #Combines predicted biomass and calculated NLL for each year into a data frame
  
  proj.yr.start <- max(catches$Year)
  proj.years <- proj.yr.start:proj.yr.end
  proj.nyr <- length(proj.years)
  
  B.proj <- vector(mode = 'numeric',length = proj.nyr)
  B.proj[1] <- B[nyr]
  
  C.proj <- vector(mode = 'numeric', length = proj.nyr)

  obsCPUE <- vector(mode = 'numeric', length=proj.nyr)
  
  #Logistic growth model fitting. max() ensures it does not go to 0
  for(i in 1:(proj.nyr-1)){
    B.proj[i+1] = max(0.001, B.proj[i] + r*B.proj[i]*(1-B.proj[i]/K) - C.proj[i])
  }
  
  for(i in 1:(proj.nyr)){
    C.proj[i] = q*E.proj*B.proj[i]
  }
  
  Results2 <- data.frame(Year = proj.years, Catch = C.proj, Biomass = B.proj)
  
  #PLOTTING PART
  #if (plot==T) {
  if (plot==T) {
    
    xrange <- start.yr:proj.yr.end
    
    plot(x=xrange,y=Results1$Biomass, lwd=2,col="black",type="l", yaxs="i",
       ylim=c(0,1.2*max(Results1$Biomass)), ylab = "Biomass (kg)", xlab = "Year")
    lines(x=xrange, y=Results2$Biomass, lwd=2, col="blue")
    
  
   # par(mfrow=c(2,1), mar=c(2,1,1,1), oma=c(3,3,1,1))
    #plot(x=years,y=Results1$Biomass, lwd=2,col="black",type="l", yaxs="i",
     #    ylim=c(0,1.05*max(Results1$Biomass)), ylab = "Biomass (kg)", xlab = "Year")
    #mtext(side=2,"Biomass (kg)", line=2.5)
    
    #plot(x=proj.years,y=Results2$Biomass, lwd=2,col="blue",type="l",yaxs="i", 
      #   ylim=c(0,1.1*max(Results2$Biomass)), xlab = "Year", ylab = 'Catch (kg)')
    #mtext(side=2,"Catch (kg)", line=2.5)
  }
  
  invisible(Results1)  #return the biomass and NLL
}
