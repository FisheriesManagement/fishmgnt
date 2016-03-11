library(dplyr)

percent.change=function(inital, end){
  
  
  
  pc= ((end-inital)/inital)*100
  pc[1]=0
   
  return(pc)
}


priceApp=read.csv('./final-presentation/prices.csv') %>%
  select(-X) %>%
  rename(Year=year) %>%
  mutate(price_yr_min1 = lag(corrected))%>%
  mutate(chgP= percent.change(price_yr_min1,corrected))

catchApp=read.csv('./data/For CRSeafood code/motioncatch.csv') %>%
  group_by(Year) %>%
  summarise(TotalCatch=sum(Catch)) %>%
  mutate(catch_yr_min1 = lag(TotalCatch)) %>%
  mutate(chgC= percent.change(catch_yr_min1,TotalCatch))

           
AppData= inner_join(priceApp, catchApp, by= "Year") 
  



saveRDS(AppData, file = "./CRsnapper-app/data/AppDATA.rds")
