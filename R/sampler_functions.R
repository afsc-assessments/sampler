#' Sampler for catch-age from fishery data
#' 
#' Main code for merging data from obsint.debriefed_age and debriefed_length for sampler code
#' requires catch, age, and length frequency data in "import" directory with naming convention
#' catch2014.csv, len2014.csv, age2014.csv
#' 
#' @param yr for year of sample construction
#' @return estimates of absolute age compositions
#' @author J Ianelli
#' @export
#' 
#' setwd()
Sampler_NRS <- function(yr=2014,do_all=TRUE,est=TRUE,maxlen=55,io=FALSE,ctl_file){
  ctl_file  <- paste0("data/sam_",yr,".dat")
  if (do_all)
  {
   # cd <- read.csv(paste("imported/catch",yr,".csv",sep=""),as.is=T,header=T)
    # hdr_cat <- read.csv("imported/hdr_cat_short.csv",as.is=T,header=F)
    # names(cd) <- hdr_cat
    aout_file <- paste("data/age",yr,".dat",sep="")
    lout_file <- paste("data/len",yr,".dat",sep="")
    ctl_file<-paste("data/sam",yr,".dat",sep="")
    # Length data massage
    ldf       <- Get_LF(yr)
    adf       <- Get_Age(yr)
    #names(ldf)
    #glimpse(cd)
    lout <- ldf %>% transmute(strata,haul,sex,len,freq)
    write.table(lout,lout_file,quote=F,row.names=F,col.names=F)
    # old format:
    # aout <- adf %>% transmute(strata, area,cr=1,mm=1,dd=1,yy=yr,lat=55,lon=-165,age=ifelse(is.na(age),0,age),sex,spp=201,wt,len,x=1,y=1,haul) # New format:
     aout <- adf %>% transmute(strata, haul, sex, age, wt, len) 
    write.table(aout,aout_file,quote=F,row.names=F,col.names=F)
  }
    #----------------------------------
    # Catch data massage names(ad) ;dim(ad)
    # n records of age data and length data
    lad <- dim(aout)[1]
    lld <- dim(lout)[1]
    # nstrata <- 1
    # cdf <- cd %>% filter(FMP_Subarea=="BS",Year==yr) %>%
    #   transmute(
    #     area=NMFS_Area,month=trunc(WED/100),
    #     # note that in 1998 no NMFS area available for some catch so going on month for stratum 1 (Aseason), 
    #     #  and 3 if not; simple trimester
    #     strata=1., #ifelse(month<5,1,ifelse(month<9,2,3)), 
    #     catch=Catch
    #     )  %>% select(strata, catch) %>%
    #   group_by(strata) %>% summarise(catch=sum(catch))
    #   #sum(catch)
    # catch <- cdf$catch
    # #catch[1] <-  #Catch in stratum 1, A season all areas
    # #catch[2] <-  #Catch in stratum 2, B season areas 520 and higher (NW)
    # #catch[3] <-  #Catch in stratum 3, B season area 519 and lower (SE)
    # cat(sep= " \n",
    #     yr , aout_file , lout_file , lad , lld ,
    #     1 , 20 , 10 , maxlen , nstrata ,
    #     catch[1], # catch[2], catch[3],
    #     paste("results/Est_",yr,".dat",sep=""),
    #     file = ctl_file
    # )
    # run sampler from commandline
    if (est) {
        if (io)
          system(paste0("../../src/sam -nox -io -ind  ",ctl_file))
        else
          system(paste0("../../src/sam -ind  ",ctl_file))
    }
}

#' @param Number of bootstraps (1=just estimation
#' @return bootstrap control file used by sam.tpl  
#' @author J Ianelli
#' @export
#' 
SetBS <- function(n=1,sam_int=c(1,1,1,1)){
  cat(sep= " \n",
        "# Number of bootstrap sampling" , 
        n , 
        sam_int[1], # Number of age hauls
        sam_int[2],
        sam_int[3], # Number of age hauls
        sam_int[4],
        file = "bs_setup.dat"
     )
  }

Get_LF <- function(yr){
  #----------------------------------
  # Create Length data
  # Strata definitions 1, 2, 3 are A-season, B_NW, B_SE
  #ldf <- ld %>% filter(SPECIES_CODE == 202,NMFS_AREA<540)  %>%
  #ldf <- ld %>% filter(NMFS_AREA<540)  %>%
  ld <- read.csv(paste0("imported/len",yr,".csv"),as.is=T,header=F)
  hdr_len <- read.csv("imported/hdr_len.csv",as.is=T,header=F)
  names(ld) <- hdr_len
  #ldf <- ld %>% filter(NMFS_AREA>500)  %>%
  ldf <- ld %>% filter(NMFS_AREA<540)  %>%
    transmute(
      haul = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),            # Data come from at-sea or port
      month = month(parse_date_time(HAUL_OFFLOAD_DATE,orders="dmy")), # get month on fly
      seas = ifelse(month>5, 2, 1),                                   # Season 2 is B, uses month
      strata=1. ,#ifelse(month<5,1,ifelse(month<9,2,3)), 
      # strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519 , 2, 3)),      # Strata >519 one, < another
      len = LENGTH, sex = ifelse(SEX=="F",1,2), freq = FREQUENCY,     # recodes sex etc
      haul = as.integer(as.factor(haul))                              # Sets hauls/offloads to unique integer values
    )
  return(ldf)
}

Write_LF <- function(LF.df, yr){
    #----------------------------------
    # Write Length data
    # Strata definitions 1, 2, 3 are A-season, B_NW, B_SE
    #ldf <- ld %>% filter(SPECIES_CODE == 202,NMFS_AREA<540)  %>%
    #ldf <- ld %>% filter(NMFS_AREA<540)  %>%
    ld <- read.csv(paste0("imported/len",yr,".csv"),as.is=T,header=F)
    hdr_len <- read.csv("imported/hdr_len.csv",as.is=T,header=F)
    names(ld) <- hdr_len
    #ldf <- ld %>% filter(NMFS_AREA>500)  %>%
    ldf <- LF.df %>% filter(NMFS_AREA<540,YEAR=yr)  %>%
      transmute(
        haul = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),            # Data come from at-sea or port
        month = month(parse_date_time(HAUL_OFFLOAD_DATE,orders="dmy")), # get month on fly
        seas = ifelse(month>5, 2, 1),                                   # Season 2 is B, uses month
        #strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519 , 2, 3)),      # Strata >519 one, < another
        strata = 1., #ifelse(month<5,1,ifelse(month<9,2,3)), 
        len = LENGTH, sex = ifelse(SEX=="F",1,2), freq = FREQUENCY,     # recodes sex etc
        haul = as.integer(as.factor(haul))                              # Sets hauls/offloads to unique integer values
      )
      return(ldf)
}
Get_Age <- function(yr)
{
   # ad <- read_csv2(paste("imported/age",yr,".csv",sep=""),col_names=F)
    ad <- read.csv(paste("imported/age",yr,".csv",sep="") ,as.is=T,header=F)
    hdr_age <- read.csv("imported/hdr_age.csv",as.is=T,header=F)
    names(ad) <- hdr_age
    ad$HAUL_OFFLOAD_DATE <-   dmy(ad$HAUL_OFFLOAD_DATE)
    #----------------------------------
    # Age data massage names(ad) ;dim(ad)
    # Need to filter for sample type
    # Strata definitions 1, 2, 3 are A-season, B_NW, B_SE
    #distinct(SPECIMEN_NUMBER) %>%
    #adf <- ad %>% dplyr::filter(NMFS_AREA>500) %>%
    adf <- ad %>% dplyr::filter(NMFS_AREA<540) %>%
      dplyr::transmute(
        #haul   = ifelse((HAUL_JOIN==""),PORT_JOIN,HAUL_JOIN),
        haul   = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),
        haul = as.integer(as.factor(haul)),
        month  = month(HAUL_OFFLOAD_DATE),
        seas   = ifelse(month>5, 2, 1), 
        strata=1., #ifelse(month<5,1,ifelse(month<9,2,3)), 
        #strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519, 2, 3)), 
        sex = ifelse(SEX=="F",1,2) ,
        len = LNGTH , 
        age = ifelse(AGE==0,-9,AGE),
        wt = ifelse(is.na(WEIGHT) | WEIGHT==0,-9,WEIGHT) , 
        age=ifelse(is.na(age),-9,age ) )
      return(adf)
 }   


#' @param AFSC database to use
#' @param outdir where to write the input files for sampler
#' @param FmpArea Fisheries management plan area such as "500 and 544" for the BSAI
#' @param SpeciesCode Specify the species such as "(104,120)"  for BSAI Northern rock sole
#' @param StrataMap Specify strata by number and column delineations
#' @return age input files for sampler by year 
#' @author Carey McGilliard
#' @export
#' 
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
 
 #needs to be done within year.
 #AgeLength.df$MAKEHAUL<-ifelse(is.na(AgeLength.df$HAUL_JOIN),AgeLength.df$PORT_JOIN,AgeLength.df$HAUL_JOIN)            # Data come from at-sea or port
 #AgeLength.df$HAULNO<-as.integer(as.factor(AgeLength.df$MAKEHAUL))
 
 Ages.df<-full_join(AgeLength.df,StrataMap)
 
 #save so you don't have to do the query every time
 save(Ages.df,file = file.path(outdir,"BigFisheryAges.Rdata"))
 
 years <-sort(unique(AgeLength.df$YEAR))
 numrows<-vector(mode="numeric",length=length(years))
 for (y in 1:length(years)) {
  agedata<-Ages.df %>% filter(YEAR==years[y]) %>% select(HAUL_JOIN,PORT_JOIN,STRATA,SEXNO,AGE,WEIGHT,LENGTH)
  agedata<-agedata %>% mutate(MAKEHAUL=ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN)  )
  agedata<-agedata %>% mutate(HAULNO=as.integer(as.factor(MAKEHAUL))) %>% select(STRATA,HAULNO,SEXNO,AGE,WEIGHT,LENGTH)
  write.table(agedata,file = file.path(outdir,paste0("age",years[y],".dat")),quote = FALSE,col.names = FALSE,row.names = FALSE)
  write.table(nrow(agedata),file = file.path(outdir,paste0("nages",years[y],".dat")),quote = FALSE,col.names = FALSE,row.names = FALSE)
  numrows[y]<-nrow(agedata)
 }
 
 ageinfo<-list()
 ageinfo$years<-years
 ageinfo$nages<-numrows
 return(ageinfo)
}



#' @param AFSC database to use
#' @param outdir where to write the input files for sampler
#' @param FmpArea Fisheries management plan area such as "500 and 544" for the BSAI
#' @param SpeciesCode Specify the species such as "(104,120)"  for BSAI Northern rock sole
#' @param StrataMap Specify strata by number and column delineations
#' @param years Years of length data needed
#' @return length input files for sampler by year 
#' @author Carey McGilliard
#' @export
#' 
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

#' @param AKFIN database to use already connected
#' @param outdir where to write the input files for sampler
#' @param CatchFmpArea Fisheries management plan area such as "'BSAI'"
#' @param CatchSpeciesCode Specify the species such as "'RSOL'"  for BSAI Northern rock sole
#' @param minage Minimum age for the species
#' @param maxage Plus group for the species
#' @param minlen Minimum length bin for the species
#' @param maxlen Maximum length bin for the species
#' @param StrataMap Specify strata by number and column delineations
#' @param years Years of length data 
#' @param nage Number of ages
#' @param nlen Number of lengths

#' @return AKFIN catch input files for sampler by year 
#' @author Carey McGilliard
#' @export
SamDat<-function(AKFIN,outdir,CatchFmpArea="'BSAI'",CatchSpeciesCode="'RSOL'",minage,maxage,minlen,maxlen,StrataMap,years,nage,nlen) {
 
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
                 "AND council.comprehensive_blend_ca.fmp_area = ",CatchFmpArea)
 
 catchbio<-sqlQuery(AKFIN,MyQuery)
 catchbio$MONTH<-month(as.Date(catchbio$WEEK_END_DATE))
 
 catchbio<-full_join(catchbio,StrataMap)
 c.df<-catchbio %>% group_by(YEAR,STRATA) %>% summarise(cbio = sum(WEIGHT_POSTED))
 
 #years<-sort(unique(AgeLength.df$YEAR))
 for (y in 1:length(years)) {
  c<-c.df%>% filter(YEAR==years[y])
  myvec<-c$cbio
  #write.table(myvec,file = file.path(outdir,paste0("catchbystrata",years[y],".dat")),quote=FALSE,row.names = FALSE,col.names = FALSE)
  #nage<-read.table(file.path(outdir,paste0("nages",years[y],".dat")))
  #nlen<-read.table(file.path(outdir,paste0("nlens",years[y],".dat")))
  
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
   NumStrata,"\n",
   myvec,"\n",
   "results/Est_",years[y],".dat",sep = "")
  
  write(noquote(mystuff),file.path(outdir,paste0("sam",years[y],".dat")),ncolumns=100, append=F)
  #  write.table(years[y],file =file.path(outdir,paste0("sam",years[y],".dat")),quote=FALSE,row.names = FALSE,col.names = FALSE)
  
  #  write.table(myvec,file = file.path(outdir,paste0("sam",years[y],".dat")),quote=FALSE,row.names = FALSE,col.names = FALSE,append = TRUE)
  
 }
}

#Pull the length comps by area

#Pull the survey specimen data
