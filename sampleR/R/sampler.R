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
#' 
Sampler <- function(yr=2014){
  ad <- read.csv(paste("imported/age",yr,".csv",sep=""),as.is=T)
  ld <- read.csv(paste("imported/len",yr,".csv",sep=""),as.is=T)
  cd <- read.csv(paste("imported/catch",yr,".csv",sep=""),as.is=T)
  aout_file <- paste("data/age",yr,".dat",sep="")
  lout_file <- paste("data/len",yr,".dat",sep="")
  # Length data massage
  names(ld)
  # Strata definitions 1, 2, 3 are A-season, B_NW, B_SE
  #distinct(SPECIMEN_NUMBER) %>%
  ldf <- ld %>% filter(SPECIES == 201) %>%
    transmute(
      haul = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),
      month = month(parse_date_time(HAUL_OFFLOAD_DATE,orders="dmy")),
      seas = month>5, seas = ifelse(month , 2, 1), 
      strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519 , 2, 3)), 
      len = LENGTH, sex = ifelse(SEX=="F",1,2), freq = FREQUENCY,
      haul = as.integer(as.factor(haul))
    )
  lout <- ldf %>% transmute(strata,haul,sex,len,freq)
  write.table(lout,lout_file,quote=F,row.names=F,col.names=F)
  # Age data massage
  # names(ad) dim(ad)
  # Need to filter for sample type
  # t <- ad %>% distinct(SPECIMEN_NUMBER)
  # t <- ad %>% distinct(HAUL_JOIN)
  # Strata definitions 1, 2, 3 are A-season, B_NW, B_SE
  #distinct(SPECIMEN_NUMBER) %>%
  adf <- ad %>% 
    transmute(
      haul = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),
      month = month(parse_date_time(HAUL_OFFLOAD_DATE,orders="dmy")),
      seas = month>5, seas = ifelse(month , 2, 1), 
      strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519 , 2, 3)), 
      area = NMFS_AREA,
      len = LENGTH, wt = WEIGHT, age = AGE, sex = ifelse(SEX=="F",1,2),
      PORT_JOIN,HAUL_JOIN, SPECIMEN_NUMBER,
      haul = as.integer(as.factor(haul))
    )
  aout <- adf %>% transmute(strata,area,cr=1,mm=1,dd=1,yy=2014,lat=55,lon=-165,age=ifelse(is.na(age),0,age),sex,spp=201,wt,len,x=1,y=1,haul)
  write.table(aout,aout_file,quote=F,row.names=F,col.names=F)
  lad <- dim(aout)[1]
  lld <- dim(lout)[1]
  nstrata <- 3
  cdf <- cd %>% filter(FMP.Area=="BSAI",Year==2014,Species.Group.Name=="Pollock") %>%
    transmute(area=Reporting.Area.Code,month=trunc(WED..mmdd./100),strata=ifelse(month<6,1,ifelse(area>519,2,3)),
              catch=SUM.Weight.Posted..Detail..) %>% select(strata, catch) %>%
    group_by(strata) %>% summarise(catch=sum(catch))
  tbl_df(cdf)
  catch <- cdf$catch
  #catch[1] <-  #Catch in stratum 1, A season all areas
  #catch[2] <-  #Catch in stratum 2, B season areas 520 and higher (NW)
  #catch[3] <-  #Catch in stratum 3, B season area 519 and lower (SE)
  cat(sep= " \n",
      yr , aout_file , lout_file , lad , lld ,
      1 , 15 , 20 , 80 , nstrata ,
      catch[1], catch[2], catch[3],
      paste("results/Est_",yr,".dat",sep=""),
      file = "sam.dat"
  )
  # run sampler from commandline
  system("./sam -nox")
}
