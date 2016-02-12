#===================================================================

# Script to call our model functions and make plots! 

# Kat Millage
# Last modified 2/3/16

#====================================================================

#===PART 1=======================================
# Plotting predicted and observed biomass
#================================================

catches <- read.csv('catches.csv', header=T, stringsAsFactors = F) %>%
  filter(Region == 'Guanacaste', Species %in% c('Pargo', 'Pargo Seda')) %>%
  select(-X, -(Jan:Dec)) %>%
  group_by(Year) %>%
  summarize(catch = sum(Total))

CPUE <-   read.csv('CPUE2.csv', header=T, stringsAsFactors = F) %>%
  select(-Trip.Num, -Date, -Captain, -Inpsector, -Location) %>%
  group_by(Year) %>%
  summarize(CPUE = mean(Number), CV = sd(Number))

x <- getNLL(par = c(0.25, 10000000, 0.00001), catches, CPUE, plot = T)

ggplot(x, aes(x=Year, y = Biomass))+
  labs(title = "CRSeafood Snapper Model", x="Year", y ="Biomass (kg)")+
  geom_line()+
  geom_point(aes(x=Year, y = ObsB), size = 3)+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14))+
  theme(axis.text.x = element_text(size=12), axis.title.x = element_text(size=14))+
  theme(plot.title = element_text (size=15, vjust=1, lineheight=1.1, face = "bold"))

#===PART 2=======================================
# Plotting likelihood function for r
#================================================

# Running your likelihood function
R.vec <- seq(0.1,0.4,0.001)
x <- Rprofile(R.vec=R.vec, lower=c(1000000, 0.000001), upper=c(10000000, 0.001))
data <- cbind(R.vec,x)
write.csv(data, file="RNLL.csv")

#calculate the likelihood profile and plot it
x.like <- vector(length=length(x))
for (i in 1:length(x)) {
  x.like[i] <- exp(min(x) - x[i])  
}
plot(x=R.vec, y=x.like, type="l",xaxs="i", yaxs="i", 
     las=1, ylab="Scaled likelihood", xlab="r values", 
     lwd=2, col="blue")   
abline(h=exp(-1.92), lty=2, col="gray50")

#===PART 3=======================================
# Plotting likelihood function for K
#================================================

# Running your likelihood function
K.vec <- seq(1000000,10000000,10000)
y <- Kprofile(K.vec=K.vec, lower=c(0.05, 0.000001), upper=c(0.6, 0.1))
data <- cbind(K.vec,y)
write.csv(data, file="KNLL.csv")

#calculate the likelihood profile and plot it
y.like <- vector(length=length(y))
for (i in 1:length(y)) {
  y.like[i] <- exp(min(y) - y[i])  
}
plot(x=K.vec, y=y.like, type="l",xaxs="i", yaxs="i", 
     las=1, ylab="Scaled likelihood", xlab="K values", 
     lwd=2, col="blue")   
abline(h=exp(-1.92), lty=2, col="gray50")

# Need likelihood profile for q. 

#===PART 4=======================================
# Projecting catches and biomass into the future using "BestModel" optimized parameters
#================================================

par <- c(0.351, 3524580, 0.00001)
z <- Project.Model(par, catches, CPUE, proj.yr.end=2050, E.proj=30, plot=T)
