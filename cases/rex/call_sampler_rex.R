#Assumes input files already exist
library(ggplot2)
library(lubridate)
library(tidyverse)
library(data.table)
library(xtable)
library(ggthemes)
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank())# element_line(colour="grey60", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=18)) + theme(axis.title.x=element_text(size=22) ,axis.title.y=element_text(size=22))
mytheme <- mytheme + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )
mytheme <- mytheme + theme( panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black", fill=NA, size=.5))

#where to write input files for sam.tpl
outdir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2025/rex_cie_review/data/fishery"

#previously accepted run's fishery age data:
run_dir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2025/rex_cie_review/runs/run1_2021_accepted"

# rex sole call to make the input data
GitDir<-"C:/GitProjects/sampler/R/"
source(file.path(GitDir,"CallQueriesForSamplerData.R"))
info<-call_queries(GitDir = "C:/GitProjects/sampler/R/",outdir="C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2025/rex_cie_review/data/fishery",StrataMap = data.frame(STRATA = rep(1,12),MONTH = seq(from = 1,to = 12, by =1)), FmpArea = "600 and 650",SpeciesCode = "105", CatchSpeciesCode = "'REXS'" , CatchFmpSubArea =  "('CG','WG','SE','WY')", minage = 0, maxage = 20, minlen = 9, maxlen = 65)

minage =0
maxage = 20

setwd(outdir)
source("C:/GitProjects/sampler/R/sampler_functions.R", echo=TRUE)

#source("C:/GitProjects/BSAI_NRS/R/sampler_NRS_functions.R", echo=TRUE)
SetBS(n=1) #set n = 1 if doing no bootstraps, this writes out an input file for number of bootstraps (bs_setup.dat)
est = TRUE
io = TRUE

#Loop over years and run sam (assumes sam.exe source folder is in your account's environment path or in the outdir):
#Note: 1992 catches are not reported as domestic- sam1992.dat catch biomass filled manually
for (y in 1:length(info$years)) {
ctl_file = paste0("sam",info$years[y],".dat")
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
for (i in 1:length(info$years)) {
  print(info$years[i])
  wtmp <- rbind(wtmp,read_table(paste0("results/sex_wtage",info$years[i],".rep"),col_names=FALSE))
  ctmp <- rbind(ctmp,read_table(paste0("results/sex_catage",info$years[i],".rep"),col_names=FALSE))
}
names(ctmp) <- names(wtmp) <- c("bs","id","year","sex",minage:maxage)
wtage <- wtmp
catage <- ctmp

##################


#Aggregate catch-at-age data over bootstraps and format for fm.tpl data inputs  
#Look for results/catagesex.csv
cdf <- pivot_longer(catage,5:(length(minage:maxage)+4),names_to="age",values_to="catch") %>%
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
wdf <- pivot_longer(wtage,5:(length(minage:maxage)+4),names_to="age",values_to="weight") %>% 
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

#compare to the fishery age data from the previously accepted model
old_data<-read.csv(file.path(run_dir,"rex_fish_ages_2021_accepted.csv"))

new_data<-WideComp %>% select(-c('0'))
source("C:/GitProjects/sampler/cases/rex/compare_age_comps.R")
compare_age_comps(new_dat=new_data,old_dat=old_data,yrs_srv_age=info$years,maxage = maxage)







