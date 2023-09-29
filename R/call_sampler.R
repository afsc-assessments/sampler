library(RODBC)
library(dplyr)
library(lubridate)
library(tidyverse)
library(keyring) # then set usernames and passwords using keyring
library(ggplot2)
library(data.table)
library(xtable)
library(ggthemes)
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank())# element_line(colour="grey60", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=18)) + theme(axis.title.x=element_text(size=22) ,axis.title.y=element_text(size=22))
mytheme <- mytheme + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )
mytheme <- mytheme + theme( panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black", fill=NA, size=.5))

#Source other functions used:
GitDir<-"C:/GitProjects/sampler/R/"
#outdir<-"C:/Users/carey.mcgilliard/Work/SpatialGrowthAssessments/BSAI_NRS_sampler"
outdir<-"C:/Users/carey.mcgilliard/Work/SpatialGrowthAssessments/GOA_rex_sampler"
source("C:/GitProjects/sampler/R/sampler_functions.R", echo=TRUE)

#Connect to the database
# keyring::key_set_with_value(service="afsc", username="WILLIAMSB", password = "my_secret_pwd")
db <- "afsc"
AFSC <- odbcConnect(db,keyring::key_list(db)$username,keyring::key_get(db, keyring::key_list(db)$username)) #mcgilliardc

dbakfin<-"AKFIN"
AKFIN<-odbcConnect(dbakfin,keyring::key_list(dbakfin)$username,keyring::key_get(dbakfin, keyring::key_list(dbakfin)$username)) #mcgilliardc

FmpArea <- "500 and 544" #BSAI = "500 and 544" ; GOA = 
SpeciesCode<-"(104,120)" #and also 120
CatchSpeciesCode<-"'RSOL'"
CatchFmpArea<-"'BSAI'"
minage<-1
maxage<-20
minlen<-10
maxlen<-55

#Note: you can define strata based on other variables, just make a map like this that can be joined to the datasets by a variable that is in both.
#YFS StrataMap (strata are times of year here, but can also be NMFS_AREA or other)
StrataMap<-data.frame(STRATA =c(1,1,1,1,2,2,2,2,3,3,3,3),
                      MONTH = seq(from = 1,to = 12,by = 1)) #YFS: 3 strata over the months of the year

#NRS StrataMap (only one strata for BSAI NRS right now)
#StrataMap<-data.frame(STRATA =rep(1,n = 12),
#                      MONTH = seq(from = 1,to = 12,by = 1)) #NRS: 1 strata

#stuff from call_sampler - make adjustments later
#yrvec<-c(1992,1995,1999,2003,2005,2007,2009,2010,2012,2014,2015,2016,2017,2018,2019,2020)
setwd(outdir)

#source("C:/GitProjects/BSAI_NRS/R/sampler_NRS_functions.R", echo=TRUE)
SetBS(n=1000) #set n = 1 if doing no bootstraps, this writes out an input file for number of bootstraps (bs_setup.dat)
est = TRUE
io = TRUE


#Write the age data
ageinfo<-SamAge(AFSC=AFSC,outdir=outdir,FmpArea=FmpArea,SpeciesCode=SpeciesCode,StrataMap=StrataMap)
nage<-ageinfo$nages
years<-ageinfo$years
yrvec<-years


#Write the length data

nlen<-SamLength(AFSC=AFSC,outdir=outdir,FmpArea=FmpArea,SpeciesCode=SpeciesCode,StrataMap=StrataMap,years=years)

#Write the sam.dat files

SamDat(AKFIN=AKFIN,outdir=outdir,CatchFmpArea=CatchFmpArea,CatchSpeciesCode=CatchSpeciesCode,
       minage=minage,maxage=maxage,minlen=minlen,maxlen=maxlen,StrataMap=StrataMap,years=years,nage=nage,nlen=nlen)

#Can set up sampler run now (functions in sampler_NRS.R and Jim's code for processing is NRS_sampler.R, bring these files into NRS directory.)



#--------------------------------------------------------------
#start where call_sampler used to start


#Loop over years and run sam:
for (y in yrvec) {
ctl_file = paste0("sam",y,".dat")
if (est) {
  if (io)
    system(paste0("sam -nox -io -ind  ",ctl_file))
  else
    system(paste0("sam -ind  ",ctl_file))
}
}


# Sampler done running
##################

#Read in datafiles from separate years and aggregate the info
#--------------------------
ctmp<-wtmp<-NULL
for (i in yrvec) {
  print(i)
  wtmp <- rbind(wtmp,read_table(paste0("results/sex_wtage",i,".rep"),col_names=FALSE))
  ctmp <- rbind(ctmp,read_table(paste0("results/sex_catage",i,".rep"),col_names=FALSE))
}
names(ctmp) <- names(wtmp) <- c("bs","id","year","sex",minage:maxage)
wtage <- wtmp
catage <- ctmp

##################


#Aggregate catch-at-age data over bootstraps and format for fm.tpl data inputs  
#Look for results/catagesex.csv
cdf <- pivot_longer(catage,5:(maxage+4),names_to="age",values_to="catch") %>%
       mutate(sex=ifelse(sex==1,"F","M")) %>% filter(catch>0)
cdf <- cdf %>% group_by(year,sex,age) %>% summarise(catch=mean(catch)) %>% mutate(sex=as.factor(sex),age=as.numeric(age)) 
cdfp<-cdf #for plotting below

cdf<-cdf %>% mutate(age = replace(age, sex=="M",100+as.numeric(age)))
cdf<-cdf %>% group_by(year,age) %>% select(year,age,catch)

tot<-cdf %>% group_by(year) %>% summarise(tcatch=sum(catch))
cdf<-full_join(cdf,tot)  %>% mutate(catch = catch/tcatch)
cdf<-cdf %>% select(year,age,catch)


thegrid<-expand.grid(age=c(seq(from =minage,to=maxage,by=1),seq(from = 100+minage,to=100+maxage,by=1)),year = unique(cdf$year))
ExpandComp<-full_join(cdf,thegrid) 
ExpandComp<-ExpandComp %>% replace_na(list(catch=0))
WideComp<-ExpandComp %>% group_by(year,age) %>% select(year,age,catch) %>% spread(age,catch)
WideComp
write_csv(WideComp,"results/catagesex.csv")

#Plot the aggregated catch-at-age data
g<-ggplot(cdfp,aes(x=age,y=catch,fill=sex,color=sex)) + 
geom_bar(position="dodge2", stat='identity') + theme_few()+ facet_wrap(.~year)
ggsave(filename = "results/catageplot.png",plot = g,device = "png")

#Aggregate catch-at-age data over bootstraps and format for fm.tpl data inputs  
#Look for results/catagesex.csv
wdf <- pivot_longer(wtage,5:(maxage+4),names_to="age",values_to="weight") %>% 
       mutate(sex=ifelse(sex==1,"F","M")) %>% filter(weight>0)
wdf <- wdf %>% group_by(year,sex,age) %>% summarise(weight=mean(weight)) %>% mutate(sex=as.factor(sex),age=as.numeric(age)) 
wdfp<-wdf
wdf<-wdf %>% mutate(age = replace(age, sex=="M",100+as.numeric(age)))
wdf<-wdf %>% group_by(year,age) %>% select(year,age,weight)

thegrid<-expand.grid(age=c(seq(from =minage,to=maxage,by=1),seq(from = 100+minage,to=100+maxage,by=1)),year = unique(wdf$year))
ExpandWts<-full_join(wdf,thegrid) 
ExpandWts<-ExpandWts %>% replace_na(list(weight=0))
WideWts<-ExpandWts %>% group_by(year,age) %>% select(year,age,weight) %>% spread(age,weight)
WideWts

write_csv(WideWts,"results/wtagesex.csv")


# tswt <- pivot_wider(wdf,names_from=c(sex,age),values_from=weight)
# write_csv(tswt,"results/wtagesex.csv")

w<-ggplot(wdfp,aes(x=age,y=weight,fill=sex,color=sex)) + 
geom_line(size=2, stat='identity') + theme_few()+ facet_wrap(.~year)

ggsave(filename = "results/wtageplot.png",plot = w,device = "png")










