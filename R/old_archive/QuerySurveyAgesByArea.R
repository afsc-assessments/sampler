#for creating sampler input for survey age data based on specimen file
#SurvFmpArea = "'GOA'"
#SurvSpeciesCode = "10200"

#GOA rex strata map
#StrataMap<-data.frame(STRATA = c(1,1,2),
#                      REGULATORY_AREA_NAME = c("WESTERN GOA","CENTRAL GOA","EASTERN GOA"))

#GOA NRS strata map
#StrataMap<-data.frame(STRATA = c(1,2,2),
# 
#SurvSpeciesCode<-10200 #Rex
#SurvFmpArea <-"GOA"
#outdir<-"C:/Users/carey.mcgilliard/Work/SpatialGrowthAssessments/GOA_rex_sampler"
#setwd(outdir)

SamSurvAge<-function(AFSC,outdir,SurvFmpArea,SurvSpeciesCode,StrataMap) {

if (SurvFmpArea=="GOA") {
  ALQuery<-paste0("SELECT RACEBASE.SPECIMEN.HAULJOIN,\n ",
                 "RACEBASE.SPECIMEN.REGION,\n ",
                 "RACEBASE.SPECIMEN.SPECIMENID,\n ",
                 "RACEBASE.SPECIMEN.BIOSTRATUM,\n ",
                 "RACEBASE.SPECIMEN.SPECIES_CODE,\n ",
                 "RACEBASE.SPECIMEN.LENGTH,\n ",
                 "RACEBASE.SPECIMEN.WEIGHT,\n ",
                 "RACEBASE.SPECIMEN.SEX,\n ",
                 "RACEBASE.SPECIMEN.AGE,\n ",
                 "RACEBASE.HAUL.START_TIME,\n ",
                 "RACEBASE.HAUL.BOTTOM_DEPTH,\n ",
                 "RACEBASE.HAUL.STRATUM,\n ",
                 "RACEBASE.HAUL.GEAR_TEMPERATURE,\n ",
                 "RACEBASE.HAUL.BOTTOM_TYPE,\n ",
                 "RACEBASE.HAUL.GEAR_DEPTH,\n ",
                 "RACEBASE.HAUL.PERFORMANCE,\n ",
                 "RACEBASE.HAUL.DURATION,\n ",
                 "RACEBASE.HAUL.DISTANCE_FISHED,\n ",
                 "RACEBASE.HAUL.NET_WIDTH,\n ",
                 "RACEBASE.HAUL.NET_HEIGHT,\n ",
                 "RACEBASE.HAUL.NET_MEASURED,\n ",
                 "RACEBASE.HAUL.START_LATITUDE,\n ",
                 "RACEBASE.HAUL.END_LATITUDE,\n ",
                 "RACEBASE.HAUL.START_LONGITUDE,\n ",
                 "RACEBASE.HAUL.END_LONGITUDE,\n ",
                 "RACEBASE.HAUL.SURFACE_TEMPERATURE,\n ",
                 "RACEBASE.HAUL.GEAR,\n ",
                 "RACEBASE.HAUL.HAULJOIN,\n ",
                 "RACEBASE.HAUL.BOTTOM_DEPTH,\n ",
                 "RACEBASE.HAUL.ABUNDANCE_HAUL,\n ",
                 "GOA.GOA_STRATA.INPFC_AREA,\n ",
                 "GOA.GOA_STRATA.MIN_DEPTH,\n ",
                 "GOA.GOA_STRATA.MAX_DEPTH,\n ",
                 "GOA.GOA_STRATA.DESCRIPTION,\n ",
                 "GOA.GOA_STRATA.REGULATORY_AREA_NAME,\n ",
                 "GOA.GOA_STRATA.STRATUM_TYPE\n ",
                 "FROM RACEBASE.SPECIMEN\n ",
                 "INNER JOIN RACEBASE.HAUL\n ",
                 "ON RACEBASE.SPECIMEN.HAULJOIN = RACEBASE.HAUL.HAULJOIN\n ",
                 "INNER JOIN GOA.GOA_STRATA\n ",
                 "ON RACEBASE.HAUL.STRATUM           = GOA.GOA_STRATA.STRATUM\n ",
                 "WHERE RACEBASE.SPECIMEN.REGION     = 'GOA'\n ",
                 "AND GOA.GOA_STRATA.SURVEY = 'GOA'\n ",
                 "AND RACEBASE.SPECIMEN.SPECIES_CODE = ",SurvSpeciesCode,"\n ",
                 "AND RACEBASE.HAUL.ABUNDANCE_HAUL   = 'Y'")
  }
  AgeLength.df<- sqlQuery(AFSC,ALQuery)

#Create some new columns to expand options for strata:
  AgeLength.df$Quarters<-quarters(as.Date(AgeLength.df$START_TIME))
  AgeLength.df$Months<-months(as.Date(AgeLength.df$START_TIME))
  
  #substring to pull out year - should not be the case! sheesh, then make numeric
  AgeLength.df$YEAR<-as.numeric(substr(AgeLength.df$START_TIME, 1, 4))
  AgeLength.df$Cohort<-AgeLength.df$YEAR - AgeLength.df$AGE
  
  #get rid of observations without lengths AND ages:
  #I can never remember how to do this without looking it up.
  AgeLength.df<-AgeLength.df[complete.cases(AgeLength.df$AGE) & complete.cases(AgeLength.df$WEIGHT),]
  AgeLength.df$GrowthMorph<-"DN"
  AgeLength.df$GrowthMorph[AgeLength.df$REGULATORY_AREA_NAME == "EASTERN GOA"]<-"EASTERN"
  AgeLength.df$GrowthMorph[AgeLength.df$REGULATORY_AREA_NAME != "EASTERN GOA"]<-"NOT_EASTERN"
  
  write.csv(AgeLength.df,file = "Age_Length.csv")
  
  #put data into sampler format:
  AgeLength.df<-AgeLength.df %>% drop_na(LENGTH,WEIGHT)
  AgeLength.df<-AgeLength.df %>% mutate(SEXNO=ifelse(SEX==1,2,1))
  AgeLength.df<-AgeLength.df %>% mutate(AGE=ifelse(is.na(AGE),-9,AGE))
  
  #needs to be done within year.
  #AgeLength.df$MAKEHAUL<-ifelse(is.na(AgeLength.df$HAUL_JOIN),AgeLength.df$PORT_JOIN,AgeLength.df$HAUL_JOIN)            # Data come from at-sea or port
  #AgeLength.df$HAULNO<-as.integer(as.factor(AgeLength.df$MAKEHAUL))
  
  Ages.df<-full_join(AgeLength.df,StrataMap)
  
  #save so you don't have to do the query every time
  save(Ages.df,file = file.path(outdir,"BigSurveyAges.Rdata")) 
  
  years <-sort(unique(AgeLength.df$YEAR))
  numrows<-vector(mode="numeric",length=length(years))

  for (y in 1:length(years)) {
   agedata<-Ages.df %>% filter(YEAR==years[y]) %>% select(HAULJOIN,STRATA,SEXNO,AGE,WEIGHT,LENGTH) %>%
            mutate(HAULNO=1,LENGTH = LENGTH/10,WEIGHT=WEIGHT/1000) %>% select(STRATA,HAULNO,SEXNO,AGE,WEIGHT,LENGTH)
   write.table(agedata,file = file.path(outdir,paste0("age",years[y],".dat")),quote = FALSE,col.names = FALSE,row.names = FALSE)
   write.table(nrow(agedata),file = file.path(outdir,paste0("nages",years[y],".dat")),quote = FALSE,col.names = FALSE,row.names = FALSE)
   numrows[y]<-nrow(agedata)
  }
  
  ageinfo<-list()
  ageinfo$years<-years
  ageinfo$nages<-numrows
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
