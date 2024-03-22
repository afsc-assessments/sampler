#Pull and format data for fishery age sampler
#separate data file for each year

#age file columns
#number of samples?
#a_tows (this means there was an aggregation over tows grouped by sex,age,weight,and length?)
#sex?
#age?
#weight?
#length?

# library(RODBC)
# library(dplyr)
# library(lubridate)
# library(tidyverse)
# 
# #Connect to the database
# AFSC <- odbcConnect("AFSC","","") #mcgilliardc
# FmpArea <- "500 and 544" 
# SpeciesCode<-"(104,120)" #and also 120
# 
# #YFS StrataMap (strata are times of year here, but can also be NMFS_AREA or other)
# #StrataMap<-data.frame(STRATA =c(1,1,1,1,2,2,2,2,3,3,3,3),
# #                      MONTH = seq(from = 1,to = 12,by = 1)) #YFS: 3 strata over the months of the year
# 
# #NRS StrataMap (only one strata for BSAI NRS right now)
# StrataMap<-data.frame(STRATA =rep(1,n = 12),
#                       MONTH = seq(from = 1,to = 12,by = 1)) #NRS: 1 strata
#   
#   
# outdir<-"C:/Users/carey.mcgilliard/Work/FlatfishAssessments/2022/NRS/Data/Fishery_Ages"

SamAge<-function(AFSC,outdir,FmpArea,SpeciesCode,StrataMap) {
#-------------------------------
#either need to add something to this query to only query otolith samples OR need to use the squash_sp_type table.
MyQuery<-paste0("SELECT to_char(OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.PORT_JOIN) as PORT_JOIN,\n ",
                "OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.AGE,\n ",
                "OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.GEAR,\n ",
                "OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.LENGTH,\n ",
                "OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.WEIGHT,\n ",
                "OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.NMFS_AREA,\n ",
                "OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.HAUL_OFFLOAD_DATE,\n ",           
                "OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.SEX,\n ",
                "OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.SPECIES,\n ",
                "OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.TYPE_1_OTOLITH,\n ",
                "OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.YEAR,\n ",
                "to_char(OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.HAUL_JOIN) as HAUL_JOIN,\n ",
                "SUBSTR(TO_CHAR(OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.HAUL_JOIN),9,19) AS HJ_LAST1,\n ",
                "SUBSTR(TO_CHAR(OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.HAUL_JOIN),1,8) AS HJ_FIRST1\n ",
                "FROM OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE\n ",
                "WHERE OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.NMFS_AREA BETWEEN ",FmpArea,"\n ",
                "AND OBSINT.DEBRIEFED_AGE_SQUASH_SP_TYPE.SPECIES in ",SpeciesCode)

AgeLength.df<-sqlQuery(AFSC,MyQuery)
#AgeLength.df$HAUL_JOIN <- as.character(AgeLength.df$HAUL_JOIN)
AgeLength.df$HJ_LAST1[complete.cases(AgeLength.df$HJ_LAST1)==F] = ""
AgeLength.df$HAUL_JOIN<-paste0(AgeLength.df$HJ_FIRST1,AgeLength.df$HJ_LAST1)
AgeLength.df$HAUL_JOIN[complete.cases(AgeLength.df$HJ_FIRST1)==F]<-NA
AgeLength.df$PORT_JOIN <- as.character(AgeLength.df$PORT_JOIN)
AgeLength.df$SEASON<-quarters(as.Date(AgeLength.df$HAUL_OFFLOAD_DATE))
AgeLength.df$MONTH<-month(as.Date(AgeLength.df$HAUL_OFFLOAD_DATE))

AgeLength.df<-AgeLength.df %>% drop_na(LENGTH,WEIGHT)
AgeLength.df<-AgeLength.df %>% mutate(SEXNO=ifelse(SEX=="F",1,2))
AgeLength.df<-AgeLength.df %>% mutate(AGE=ifelse(is.na(AGE),-9,AGE))
AgeLength.df<-AgeLength.df %>% mutate(COUNTAGES = ifelse(AGE==-9,0,1))

#needs to be done within year.
#AgeLength.df$MAKEHAUL<-ifelse(is.na(AgeLength.df$HAUL_JOIN),AgeLength.df$PORT_JOIN,AgeLength.df$HAUL_JOIN)            # Data come from at-sea or port
#AgeLength.df$HAULNO<-as.integer(as.factor(AgeLength.df$MAKEHAUL))

Ages.df<-full_join(AgeLength.df,StrataMap)

#save so you don't have to do the query every time
save(Ages.df,file = file.path(outdir,"BigFisheryAges.Rdata"))

years <-sort(unique(AgeLength.df$YEAR))
final_years<-vector("numeric",length = 1)
numrows<-vector(mode="numeric",length=1)
for (y in 1:length(years)) {
  agedata<-Ages.df %>% filter(YEAR==years[y]) %>% select(HAUL_JOIN,PORT_JOIN,STRATA,SEXNO,AGE,WEIGHT,LENGTH,COUNTAGES)
  if (sum(agedata$COUNTAGES!=0)) {
    agedata<-agedata %>% mutate(MAKEHAUL=ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN)  )
    agedata<-agedata %>% mutate(HAULNO=as.integer(as.factor(MAKEHAUL))) %>% select(STRATA,HAULNO,SEXNO,AGE,WEIGHT,LENGTH)
    write.table(agedata,file = file.path(outdir,paste0("age",years[y],".dat")),quote = FALSE,col.names = FALSE,row.names = FALSE)
    write.table(nrow(agedata),file = file.path(outdir,paste0("nages",years[y],".dat")),quote = FALSE,col.names = FALSE,row.names = FALSE)
    numrows<-append(numrows,nrow(agedata))
    final_years<-append(final_years,years[y])
  }
 }

ageinfo<-list()
ageinfo$years<-final_years[2:length(final_years)]
ageinfo$nages<-numrows[2:length(numrows)]
return(ageinfo)
}

#need a data file that looks like this for each year, named "age1991.dat" etc., one for each year:
#Suspected columns are:
#strata haulno sex age weight length 
# 1 31 1 6 0.12 26
# 1 6 1 8 0.36 33
# 1 10 1 3 0.05 18
# 1 62 2 8 0.4 31
# 1 62 2 11 0.95 43
# 1 62 1 12 1.25 44
# 1 62 1 12 1.4 45
# 1 63 2 9 0.4 33
# 1 63 2 11 0.6 37
# 1 63 1 10 0.95 40
# 1 63 1 14 1.35 45
# 1 67 1 12 0.65 39
# 1 65 2 8 0.28 29
# 1 18 1 15 1.26 43




