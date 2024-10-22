#Call queries for sampler program and write data for sampler

library(RODBC)
library(dplyr)
library(lubridate)
library(tidyverse)
library(keyring) # then set usernames and passwords using keyring


#Source other functions used:
GitDir<-"C:/GitProjects/sampler/R/"
#outdir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2024/bsai_nrs/data/fishery"
outdir<-"C:/Users/carey.mcgilliard/Work/EBSpollock/2024/data/fishery"


source(file.path(GitDir,"QueryAgesForSampler.R"), echo=TRUE)
source(file.path(GitDir,"QueryLengthsForSampler.R"), echo=TRUE)
source(file.path(GitDir,"QueryCatchByStrata.R"), echo=TRUE)

#Connect to the database
# keyring::key_set_with_value(service="afsc", username="WILLIAMSB", password = "my_secret_pwd")
db <- "afsc"
AFSC <- odbcConnect(db,keyring::key_list(db)$username,keyring::key_get(db, keyring::key_list(db)$username)) #mcgilliardc

dbakfin<-"AKFIN"
AKFIN<-odbcConnect(dbakfin,keyring::key_list(dbakfin)$username,keyring::key_get(dbakfin, keyring::key_list(dbakfin)$username)) #mcgilliardc

FmpArea <- "500 and 530" #BS, #BSAI = "500 and 544" ; GOA = 
SpeciesCode<- "201" #201 = pollock, #NRS is "(104,120)" 
CatchSpeciesCode<- "'PLCK'" # "'RSOL'"
#CatchFmpArea<-"'BSAI'"
CatchFmpSubArea<-"'BS'" #"('BS','AI')" #"'BS'" ##"'BS'"
minage<-1
maxage<- 15 #20 = nrs
minlen<- 20 #10 = nrs
maxlen<- 80 #55 = nrs

#Note: you can define strata based on other variables, just make a map like this that can be joined to the datasets by a variable that is in both.

#NRS StrataMap (only one strata for NRS)
#StrataMap<-data.frame(STRATA = rep(1,12),
#                      MONTH = seq(from = 1,to = 12, by =1))

#YFS StrataMap (strata are times of year here, but can also be NMFS_AREA or other)
#StrataMap<-data.frame(STRATA =c(1,1,1,1,2,2,2,2,3,3,3,3),
#                      MONTH = seq(from = 1,to = 12,by = 1)) #YFS: 3 strata over the months of the year

#ebs pollock StrataMap (only one strata for BSAI NRS right now)
#carey made the columns AB and L170 within SamAge and SamLength and SamDat specifically for ebs pollock to make the strata simpler
#AB is A or B season (before June = A, after June = B)
#L170 is whether the area is NW of longitude 170 or SE of it.
  StrataMap<-data.frame(STRATA =c(1,1,2,3),
                        AB = c("A","A","B","B"),
                        L170 = c("NW","SE","NW","SE"))
  


#Write the age data
ageinfo<-SamAge(AFSC=AFSC,outdir=outdir,FmpArea=FmpArea,SpeciesCode=SpeciesCode,StrataMap=StrataMap,unsexed = TRUE)
nage<-ageinfo$nages
years<-ageinfo$years

#Write the length data

nlen<-SamLength(AFSC=AFSC,outdir=outdir,FmpArea=FmpArea,SpeciesCode=SpeciesCode,StrataMap=StrataMap,years=years,unsexed = TRUE)

#Write the sam.dat files

SamDat(AKFIN=AKFIN,outdir=outdir,CatchFmpSubArea=CatchFmpSubArea,CatchSpeciesCode=CatchSpeciesCode,
       minage=minage,maxage=maxage,minlen=minlen,maxlen=maxlen,StrataMap=StrataMap,years=years,nage=nage,nlen=nlen)

#Can set up sampler run now (functions in sampler_NRS.R and Jim's code for processing is NRS_sampler.R, bring these files into NRS directory.)
  