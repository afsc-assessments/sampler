# #Query catch by strata and write the main control file for sam.tpl
# #Must run QueryLengthsForSampler.R and QueryAgesForSampler.R first
# 
# 
# library(tidyverse)
# library(RODBC)
# library(lubridate)
#                 
# AKFIN<- odbcConnect("AKFIN","","") #cmcgilliard
# FmpArea <- 'BSAI' 
# SpeciesCode<-'RSOL' 
# minage<-1
# maxage<-20
# minlen<-10
# maxlen<-55
# 
# #YFS StrataMap (strata are times of year here, but can also be NMFS_AREA or other)
# #StrataMap<-data.frame(STRATA =c(1,1,1,1,2,2,2,2,3,3,3,3),
# #                      MONTH = seq(from = 1,to = 12,by = 1)) #YFS: 3 strata over the months of the year
# 
# #NRS StrataMap (only one strata for BSAI NRS right now)
# StrataMap<-data.frame(STRATA =rep(1,n = 12),
#                       MONTH = seq(from = 1,to = 12,by = 1)) #NRS: 1 strata

SamDat<-function(AKFIN,outdir,CatchFmpSubArea=c("'BS','AI'"),CatchSpeciesCode="'RSOL'",minage,maxage,minlen,maxlen,StrataMap,years,nage,nlen) {
  
NumStrata<-max(StrataMap$STRATA)

# MyQuery<-paste0("SELECT council.comprehensive_blend_ca.week_end_date,\n ",
# "council.comprehensive_blend_ca.catch_activity_date,\n ",
# "council.comprehensive_blend_ca.reporting_area_code,\n ",
# "council.comprehensive_blend_ca.agency_species_code,\n ",
# "council.comprehensive_blend_ca.species_group_code,\n ",
# "council.comprehensive_blend_ca.retained_or_discarded,\n ",
# "council.comprehensive_blend_ca.weight_posted,\n ",
# "council.comprehensive_blend_ca.agency_gear_code,\n ",
# "council.comprehensive_blend_ca.year,\n ",
# "council.comprehensive_blend_ca.fmp_area,\n ",
# "council.comprehensive_blend_ca.fmp_subarea,\n ",
# "council.comprehensive_blend_ca.fmp_gear,\n ",
# "council.comprehensive_blend_ca.species_name\n ",
# "FROM council.comprehensive_blend_ca\n ",
# "WHERE council.comprehensive_blend_ca.species_group_code = 'RSOL'","\n ",
# "AND council.comprehensive_blend_ca.fmp_area = 'BSAI'")







#Attention:
#You might also need the foreign catch data here.
print("the foreign catch data are not included in this query")

MyQuery<-paste0("SELECT council.comprehensive_blend_ca.week_end_date,\n ",
                "council.comprehensive_blend_ca.catch_activity_date,\n ",
                "council.comprehensive_blend_ca.reporting_area_code,\n ",
                "council.comprehensive_blend_ca.agency_species_code,\n ",
                "council.comprehensive_blend_ca.species_group_code,\n ",
                "council.comprehensive_blend_ca.retained_or_discarded,\n ",
                "council.comprehensive_blend_ca.weight_posted,\n ",
                "council.comprehensive_blend_ca.agency_gear_code,\n ",
                "council.comprehensive_blend_ca.year,\n ",
                "council.comprehensive_blend_ca.fmp_area,\n ",
                "council.comprehensive_blend_ca.fmp_subarea,\n ",
                "council.comprehensive_blend_ca.fmp_gear,\n ",
                "council.comprehensive_blend_ca.species_name\n ",
                "FROM council.comprehensive_blend_ca\n ",
                "WHERE council.comprehensive_blend_ca.species_group_code = ",CatchSpeciesCode," \n ",
                "AND council.comprehensive_blend_ca.fmp_subarea in ",CatchFmpSubArea)

catchbio<-sqlQuery(AKFIN,MyQuery)
catchbio$MONTH<-month(as.Date(catchbio$WEEK_END_DATE))
catchbio<-catchbio %>% mutate(AB = case_when(MONTH<6 ~ "A",
                                                     MONTH>=6 ~ "B"),
                                      L170 = case_when(REPORTING_AREA_CODE>=520 ~ "NW",
                                                       REPORTING_AREA_CODE < 520 ~ "SE"))

catchbio<-full_join(catchbio,StrataMap)
c.df<-catchbio %>% group_by(YEAR,STRATA) %>% summarise(cbio = sum(WEIGHT_POSTED))

#years<-sort(unique(AgeLength.df$YEAR))
for (y in 1:length(years)) {
  c_by_yr<-c.df%>% filter(YEAR==years[y])
  myvec<-c_by_yr$cbio
  #write.table(myvec,file = file.path(outdir,paste0("catchbystrata",years[y],".dat")),quote=FALSE,row.names = FALSE,col.names = FALSE)
  #nage<-read.table(file.path(outdir,paste0("nages",years[y],".dat")))
  #nlen<-read.table(file.path(outdir,paste0("nlens",years[y],".dat")))
  
  # mystuff<-paste(
  #  years[y],"\n",
  #  "age",years[y],".dat","\n",
  #  "len",years[y],".dat","\n",
  #  nage[y],"\n",
  #  nlen[y],"\n",
  #  minage,"\n",
  #  maxage,"\n",
  #  minlen,"\n",
  #  maxlen,"\n",
  #  NumStrata,"\n",
  #  myvec,"\n",
  #  "results/Est_",years[y],".dat",sep = "")
  
  mystuff<-paste(
   years[y],"\n",
   "age",years[y],".dat","\n",
   "len",years[y],".dat","\n",
   nage[y],"\n",
   nlen[y],"\n",
   minage,"\n",
   maxage,"\n",
   minlen,"\n",
   maxlen,"\n",
   NumStrata)
  
  morestuff<-paste(myvec)
  laststuff<-paste("results/Est_",years[y],".dat",sep = "")
  
  write(noquote(mystuff),file.path(outdir,paste0("sam",years[y],".dat")),ncolumns=100, append=F)
  write(noquote(morestuff),file.path(outdir,paste0("sam",years[y],".dat")),ncolumns=100, append=T)
  write(noquote(laststuff),file.path(outdir,paste0("sam",years[y],".dat")),ncolumns=100, append=T)
  
  #  write.table(years[y],file =file.path(outdir,paste0("sam",years[y],".dat")),quote=FALSE,row.names = FALSE,col.names = FALSE)
  
#  write.table(myvec,file = file.path(outdir,paste0("sam",years[y],".dat")),quote=FALSE,row.names = FALSE,col.names = FALSE,append = TRUE)
  
}
}

