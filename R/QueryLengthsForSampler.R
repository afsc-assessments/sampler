# #Pull and format data for fishery age sampler
# #separate data file for each year
# 
# #age file columns
# #number of samples?
# #a_tows (this means there was an aggregation over tows grouped by sex,age,weight,and length?)
# #sex?
# #age?
# #weight?
# #length?
# 
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

SamLength<-function(AFSC,outdir,FmpArea,SpeciesCode,StrataMap,years) {
#-------------------------------
#either need to add something to this query to only query otolith samples OR need to use the squash_sp_type table.
MyQuery<-paste0("SELECT to_char(OBSINT.DEBRIEFED_LENGTH.PORT_JOIN) as PORT_JOIN,\n ",
                "OBSINT.DEBRIEFED_LENGTH.GEAR,\n ",
                "OBSINT.DEBRIEFED_LENGTH.LENGTH,\n ",
                "OBSINT.DEBRIEFED_LENGTH.FREQUENCY,\n ",
                "OBSINT.DEBRIEFED_LENGTH.NMFS_AREA,\n ",
                "OBSINT.DEBRIEFED_LENGTH.HAUL_OFFLOAD_DATE,\n ",           
                "OBSINT.DEBRIEFED_LENGTH.SEX,\n ",
                "OBSINT.DEBRIEFED_LENGTH.SPECIES,\n ",
                "OBSINT.DEBRIEFED_LENGTH.YEAR,\n ",
                "to_char(OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN) as HAUL_JOIN,\n ",
                "SUBSTR(TO_CHAR(OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN),9,19) AS HJ_LAST1,\n ",
                "SUBSTR(TO_CHAR(OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN),1,8) AS HJ_FIRST1\n ",
                "FROM OBSINT.DEBRIEFED_LENGTH\n ",
                "WHERE OBSINT.DEBRIEFED_LENGTH.NMFS_AREA BETWEEN ",FmpArea,"\n ",
                "AND OBSINT.DEBRIEFED_LENGTH.SPECIES in ",SpeciesCode)

Length.df<-sqlQuery(AFSC,MyQuery)
#Length.df$HAUL_JOIN <- as.character(Length.df$HAUL_JOIN)
Length.df$HJ_LAST1[complete.cases(Length.df$HJ_LAST1)==F] = ""
Length.df$HAUL_JOIN<-paste0(Length.df$HJ_FIRST1,Length.df$HJ_LAST1)
Length.df$HAUL_JOIN[complete.cases(Length.df$HJ_FIRST1)==F]<-NA
Length.df$PORT_JOIN <- as.character(Length.df$PORT_JOIN)
Length.df$SEASON<-quarters(as.Date(Length.df$HAUL_OFFLOAD_DATE))
Length.df$MONTH<-month(as.Date(Length.df$HAUL_OFFLOAD_DATE))

Length.df<-Length.df %>% drop_na(LENGTH,FREQUENCY)
Length.df<-Length.df %>% mutate(SEXNO=ifelse(SEX=="F",1,2))

#needs to be done within year.
#Length.df$MAKEHAUL<-ifelse(is.na(Length.df$HAUL_JOIN),Length.df$PORT_JOIN,Length.df$HAUL_JOIN)            # Data come from at-sea or port
#Length.df$HAULNO<-as.integer(as.factor(Length.df$MAKEHAUL))

Lengths.df<-full_join(Length.df,StrataMap)

#save so you don't have to do the query every time
save(Lengths.df,file = file.path(outdir,"BigFisheryLengths.Rdata"))

#years <-sort(unique(AgeLength.df$YEAR))
numrows<-vector(mode="numeric",length=length(years))
for (y in 1:length(years)) {
  lengthdata<-Lengths.df %>% filter(YEAR==years[y]) %>% select(HAUL_JOIN,PORT_JOIN,STRATA,SEXNO,LENGTH,FREQUENCY)
  lengthdata<-lengthdata %>% mutate(MAKEHAUL=ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN)  )
  lengthdata<-lengthdata %>% mutate(HAULNO=as.integer(as.factor(MAKEHAUL))) %>% select(STRATA,HAULNO,SEXNO,LENGTH,FREQUENCY)
  write.table(lengthdata,file = file.path(outdir,paste0("len",years[y],".dat")),quote = FALSE,col.names = FALSE,row.names = FALSE)
#  write.table(nrow(lengthdata),file = file.path(outdir,paste0("nlens",years[y],".dat")),quote = FALSE,col.names = FALSE,row.names = FALSE)
   numrows[y]<-nrow(lengthdata) 
}

return(numrows)
}

#need a data file that looks like this for each year, named "length1991.dat" etc., one for each year:
#Suspected columns are:
#strata haulno sex length frequency
# 2 2257 2 98 2
# 2 2218 2 97 1
# 2 5133 2 69 4
# 1 2278 2 74 4
# 2 5127 2 93 1
# 1 54 2 63 1
# 1 130 2 68 1
# 1 171 2 60 8
# 1 114 2 68 1
# 1 91 2 73 1
# 1 44 2 53 1
# 1 155 2 68 1
# 1 3191 1 62 3
# 1 2340 2 51 1
# 1 2340 2 74 1
# 1 5156 2 42 1