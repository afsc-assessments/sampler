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
#' setwd()
Sampler <- function(yr=2014){
  ad <- read.csv(paste("imported/age",yr,".csv",sep=""),as.is=T,header=F)
  ld <- read.csv(paste("imported/len",yr,".csv",sep=""),as.is=T,header=F)
  cd <- read.csv(paste("imported/catch",yr,".csv",sep=""),as.is=T,header=F)
 #tbl_df(cd) 
  hdr_age <- read.csv("imported/hdr_age.csv",as.is=T,header=F)
  hdr_len <- read.csv("imported/hdr_len.csv",as.is=T,header=F)
  hdr_cat <- read.csv("imported/hdr_cat.csv",as.is=T,header=F)
  aout_file <- paste("data/age",yr,".dat",sep="")
  lout_file <- paste("data/len",yr,".dat",sep="")
  ctl_file  <- paste("data/sam_",yr,".dat",sep="")
  # Length data massage
  names(ld) <- hdr_len
  names(ad) <- hdr_age
  names(cd) <- hdr_cat
  #----------------------------------
  # Create Length data
  # Strata definitions 1, 2, 3 are A-season, B_NW, B_SE
  ldf <- ld %>% filter(SPECIES_CODE == 201,NMFS_AREA<540)  %>%
    transmute(
      haul = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),            # Data come from at-sea or port
      month = month(parse_date_time(HAUL_OFFLOAD_DATE,orders="dmy")), # get month on fly
      seas = ifelse(month>5, 2, 1),                                   # Season 2 is B, uses month
      strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519 , 2, 3)),      # Strata >519 one, < another
      len = LENGTH, sex = ifelse(SEX=="F",1,2), freq = FREQUENCY,     # recodes sex etc
      haul = as.integer(as.factor(haul))                              # Sets hauls/offloads to unique integer values
    )
  names(ldf)
  lout <- ldf %>% transmute(strata,haul,sex,len,freq)
  write.table(lout,lout_file,quote=F,row.names=F,col.names=F)
  #----------------------------------
  # Age data massage names(ad) ;dim(ad)
  # Need to filter for sample type
  # Strata definitions 1, 2, 3 are A-season, B_NW, B_SE
  #distinct(SPECIMEN_NUMBER) %>%
  adf <- ad %>% filter(NMFS_AREA<540) %>%
    transmute(
      haul   = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),
      month  = month(parse_date_time(HAUL_OFFLOAD_DATE,orders="dmy")),
      seas   = ifelse(month>5, 2, 1), 
      strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519, 2, 3)), 
      area   = NMFS_AREA,
      len    = LENGTH, wt = WEIGHT, age = AGE, sex = ifelse(SEX=="F",1,2),
      PORT_JOIN,HAUL_JOIN, SPECIMEN_NUMBER,
      haul = as.integer(as.factor(haul))
    )
  aout <- adf %>% transmute(strata,
                            area,cr=1,mm=1,dd=1,yy=yr,lat=55,lon=-165,age=ifelse(is.na(age),0,age),sex,spp=201,wt,len,x=1,y=1,haul)
  # New format:
  # aout <- adf %>% transmute(strata, haul, sex, age, wt, len) 
  write.table(aout,aout_file,quote=F,row.names=F,col.names=F)
  #----------------------------------
  # Catch data massage names(ad) ;dim(ad)
  # n records of age data and length data
  lad <- dim(aout)[1]
  lld <- dim(lout)[1]
  nstrata <- 3
  cdf <- cd %>% filter(FMP.Area=="BSAI",Year==yr,Species.Group.Name=="Pollock") %>%
    transmute(
      area=Reporting.Area.Code,month=trunc(WED/100),
      strata=ifelse(month<6,1,ifelse(area>519,2,3)), 
      catch=wt_recorded
      )  %>% select(strata, catch) %>%
    group_by(strata) %>% summarise(catch=sum(catch))
  tbl_df(cdf)
  sum(cdf$catch)
  catch <- cdf$catch
  #catch[1] <-  #Catch in stratum 1, A season all areas
  #catch[2] <-  #Catch in stratum 2, B season areas 520 and higher (NW)
  #catch[3] <-  #Catch in stratum 3, B season area 519 and lower (SE)
  cat(sep= " \n",
      yr , aout_file , lout_file , lad , lld ,
      1 , 15 , 20 , 80 , nstrata ,
      catch[1], catch[2], catch[3],
      paste("results/Est_",yr,".dat",sep=""),
      file = ctl_file
  )
  # run sampler from commandline
  system(paste0("./sam -nox -ind  ",ctl_file))
}
