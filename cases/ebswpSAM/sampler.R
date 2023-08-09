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
#yr=1999;do_all=TRUE;est=TRUE;maxlen=80;io=FALSE

Sampler <- function(iyr=2014,do_all=TRUE,est=TRUE,maxlen=80,io=FALSE){
  ctl_file  <- paste0("data/sam_",yr,".dat")
  if (do_all)
  {
    #cd <- read.csv(paste("imported/catch",yr,".csv",sep=""),as.is=T,header=T)
    # hdr_cat <- read.csv("imported/hdr_cat_short.csv",as.is=T,header=F)
    # names(cd) <- hdr_cat
    aout_file <- paste("data/age",iyr,".dat",sep="")
    lout_file <- paste("data/len",iyr,".dat",sep="")
    # Length data massage
    ldf <- ldt %>% filter(NMFS_AREA<540,YEAR==iyr)  %>%
      transmute(
      haul = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),            # Data come from at-sea or port
      month = month(parse_date_time(HAUL_OFFLOAD_DATE,orders="dmy")), # get month on fly
      seas = ifelse(month>5, 2, 1),                                   # Season 2 is B, uses month
      strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519 , 2, 3)),      # Strata >519 one, < another
      len = LENGTH, sex = ifelse(SEX=="F",1,2), freq = FREQUENCY,     # recodes sex etc
      haul = as.integer(as.factor(haul))                              # Sets hauls/offloads to unique integer values
    )
    adf <- adt %>% filter(YEAR==iyr,NMFS_AREA<540) %>%
      transmute(
        #haul   = ifelse((HAUL_JOIN==""),PORT_JOIN,HAUL_JOIN),
        haul   = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),
        haul = as.integer(as.factor(haul)),
        month  = month(HAUL_OFFLOAD_DATE),
        seas   = ifelse(month>5, 2, 1), 
        strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519, 2, 3)), 
        sex = ifelse(SEX=="F",1,2) ,
        len    = LNGTH , age = ifelse(AGE==0,-9,AGE),
        wt = ifelse(is.na(WEIGHT) | WEIGHT==0,-9,WEIGHT) , age=ifelse(is.na(age),-9,age ) ,
        Year=YEAR
        )
    #ldf       <- Get_LF(yr)
    #adf       <- Get_Age(yr)
    #names(ldf)
    #glimpse(adf)
    lout <- ldf %>% transmute(strata,haul,sex,len,freq)
    write.table(lout,lout_file,quote=F,row.names=F,col.names=F)
    # old format:
    # aout <- adf %>% transmute(strata, area,cr=1,mm=1,dd=1,yy=yr,lat=55,lon=-165,age=ifelse(is.na(age),0,age),sex,spp=201,wt,len,x=1,y=1,haul)
    # New format:
     aout <- adf %>% transmute(strata, haul, sex, age, wt, len) 
    write.table(aout,aout_file,quote=F,row.names=F,col.names=F)
 }
    #----------------------------------
    # Catch data massage names(ad) ;dim(ad)
    # n records of age data and length data
    lad <- dim(aout)[1]
    lld <- dim(lout)[1]
    nstrata <- 3
    cdf <- cd %>% filter(FMP_Subarea=="BS",Year==iyr) %>%
      transmute(
        area=NMFS_Area,month=trunc(WED/100),
        # note that in 1998 no NMFS area available for some catch so going on month for stratum 1 (Aseason), 
        #  and 3 if not
        strata=ifelse(is.na(area),ifelse(month<6,1,3),ifelse(month<6,1,ifelse(area>519,2,3))), 
        catch=Catch
        )  %>% select(strata, catch) %>%
      group_by(strata) %>% summarise(catch=sum(catch))
      #sum(catch)
    catch <- cdf$catch
    #catch[1] <-  #Catch in stratum 1, A season all areas
    #catch[2] <-  #Catch in stratum 2, B season areas 520 and higher (NW)
    #catch[3] <-  #Catch in stratum 3, B season area 519 and lower (SE)
    cat(sep= " \n",
        iyr , aout_file , lout_file , lad , lld ,
        1 , 15 , 20 , maxlen , nstrata ,
        catch[1], catch[2], catch[3],
        paste("results/Est_",iyr,".dat",sep=""),
        file = ctl_file
    )
    # run sampler from commandline
    if (est) {
        if (io)
          system(paste0("./sam -nox -io -ind  ",ctl_file))
        else
          system(paste0("./sam -ind  ",ctl_file))
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

Get_LF <- function(iyr){
  #----------------------------------
  # Create Length data
  # Strata definitions 1, 2, 3 are A-season, B_NW, B_SE
  #ldf <- ld %>% filter(SPECIES_CODE == 202,NMFS_AREA<540)  %>%
  #ldf <- ld %>% filter(NMFS_AREA<540)  %>%
  ld <- read.csv(paste0("imported/len",iyr,".csv"),as.is=T,header=F)
  hdr_len <- read.csv("imported/hdr_len.csv",as.is=T,header=F)
  names(ld) <- hdr_len
  #ldf <- ld %>% filter(NMFS_AREA>500)  %>%
  ldf <- ld %>% filter(NMFS_AREA<540)  %>%
    transmute(
      haul = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),            # Data come from at-sea or port
      month = month(parse_date_time(HAUL_OFFLOAD_DATE,orders="dmy")), # get month on fly
      seas = ifelse(month>5, 2, 1),                                   # Season 2 is B, uses month
      strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519 , 2, 3)),      # Strata >519 one, < another
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
        strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519 , 2, 3)),      # Strata >519 one, < another
        len = LENGTH, sex = ifelse(SEX=="F",1,2), freq = FREQUENCY,     # recodes sex etc
        haul = as.integer(as.factor(haul))                              # Sets hauls/offloads to unique integer values
      )
      return(ldf)
}
Get_Age <- function(yr)
{
   # ad <- read_csv2(paste("imported/age",yr,".csv",sep=""),col_names=F)
    ad <- read_csv(paste("imported/age",yr,".csv",sep="") ,as.is=T,header=F)
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
        strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519, 2, 3)), 
        sex = ifelse(SEX=="F",1,2) ,
        len    = LNGTH , age = ifelse(AGE==0,-9,AGE),
        wt = ifelse(is.na(WEIGHT) | WEIGHT==0,-9,WEIGHT) , age=ifelse(is.na(age),-9,age ) )
      return(adf)
 }   