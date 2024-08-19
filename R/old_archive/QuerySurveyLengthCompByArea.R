#read in survey length frequency information by area
#SurvFmpArea = "'GOA'"
#SurvSpeciesCode = "10200"

#GOA rex strata map
#StrataMap<-data.frame(STRATA = c(1,1,2),
#                      REGULATORY_AREA_NAME = c("WESTERN GOA","CENTRAL GOA","EASTERN GOA"))

#GOA NRS strata map
#StrataMap<-data.frame(STRATA = c(1,2,2),
#                      REGULATORY_AREA_NAME = c("WESTERN GOA","CENTRAL GOA","EASTERN GOA"))

SamSurveyLength<-function(AFSC,outdir,SurvFmpArea,SurvSpeciesCode,StrataMap,years) {
 
 l.query<-paste0("SELECT GOA.SIZECOMP_AREA.SURVEY,\n ",
                 "GOA.SIZECOMP_AREA.YEAR,\n ",
                 "GOA.SIZECOMP_AREA.SPECIES_CODE,\n ",
                 "GOA.SIZECOMP_AREA.REGULATORY_AREA_NAME,\n ",
                 "GOA.SIZECOMP_AREA.LENGTH,\n ",
                 "GOA.SIZECOMP_AREA.MALES,\n ",
                 "GOA.SIZECOMP_AREA.FEMALES,\n ",
                 "GOA.SIZECOMP_AREA.UNSEXED,\n ",
                 "GOA.SIZECOMP_AREA.TOTAL\n ",
                 "FROM GOA.SIZECOMP_AREA\n ",
                 "WHERE GOA.SIZECOMP_AREA.SURVEY     = ",SurvFmpArea,"\n ",
                 "AND GOA.SIZECOMP_AREA.SPECIES_CODE = ",SurvSpeciesCode)
 
 Lengths.df<- sqlQuery(AFSC,l.query)
 # Lengths.df$GrowthMorph<-NA
 # Lengths.df$GrowthMorph[Lengths.df$REGULATORY_AREA_NAME=="EASTERN GOA"]<-"eastern"
 # Lengths.df$GrowthMorph[Lengths.df$REGULATORY_AREA_NAME!="EASTERN GOA"]<-"noteastern"
 
 write.csv(Lengths.df,file.path(outdir,"LengthsByArea.csv"))
 
 Length.df<-full_join(Lengths.df,StrataMap) %>% select(-c(UNSEXED)) %>%
  pivot_longer(cols = c(MALES,FEMALES),names_to = "sex",values_to = "freq") %>%
  mutate(sexno=ifelse(sex=="FEMALES",1,2),length = LENGTH/10,haulno = 1) %>%
  select(YEAR,STRATA,haulno,sexno,length,freq)

 numrows<-vector(mode="numeric",length=length(years))
for (y in 1:length(years)) {
 lengthdata<-Length.df %>% filter(YEAR==years[y]) %>% select(-c(YEAR))
 write.table(lengthdata,file = file.path(outdir,paste0("len",years[y],".dat")),quote = FALSE,col.names = FALSE,row.names = FALSE)
 
}

 return(numrows)
}

#need a data file that looks like this for each year, named "length1991.dat" etc., one for each year:
#Suspected columns are:
#strata haulno sex age length frequency
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