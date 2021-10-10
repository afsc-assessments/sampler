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
yr=2015
PSCampler <- function(yr=2014){
  ad <- read.csv("imported/bogus_age.csv"),as.is=T,header=F)
  ld <- read.csv("~/Dropbox/Halibut/Assessment Data/hal_length_export.csv",as.is=T )
  cd <- read.csv("imported/PSC_catch.csv",as.is=T,header=T)
 tbl_df(cd) 
  hdr_age <- read.csv("imported/hdr_age.csv",as.is=T,header=F)
  #hdr_len <- read.csv("imported/hdr_hal_len.csv",as.is=T,header=F)
  hdr_cat <- read.csv("imported/hdr_psc_cat.csv",as.is=T,header=F)
  aout_file <- paste("data/age",yr,".dat",sep="")
  lout_file <- paste("data/hal_len",yr,".dat",sep="")
  ctl_file  <- paste("data/sam_",yr,".dat",sep="")
  # Length data massage
  #names(ld) <- hdr_len
  names(ld)
  tt <- ld %>% filter(gear==2) %>% group_by(yr) %>% summarise(sum(freq))
  glimpse(tt)
  View(tt)
  dim(tt)
  tbl_df(ld)
  names(ad) <- hdr_age
  names(cd) <- hdr_cat
  #----------------------------------
  # Create Length data
  # Strata definitions 1, 2, 3 are A-season, B_NW, B_SE
  ldf <- ld %>% filter(!is.na(gear),yr>1990,yr<2016,SPECIES== 101 , area<550)  %>%
      transmute(
      haul   = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),            # Data come from at-sea or port
      yr     = yr,
      gear   = ifelse(gear==1,"Trawl","Fixed"),
      month  = month(parse_date_time(date,"ymd")), # get month on fly
      qtr    = quarter(parse_date_time(date,"ymd")), # get month on fly
      zone   = ifelse(area>519 , "NW", "AI+SE")                     ,    # .1 is Aleutians and SE, .2 is NW, 
      seas   = ifelse(qtr<3,1,2),
      strata = (paste(gear,"Qtr",qtr,"Area",zone, sep="_") ),
      strata1= as.factor(qtr  + ifelse(area>519 , .2, .1) )                    ,    # .1 is Aleutians and SE, .2 is NW, 
      strata2= as.factor(seas + ifelse(area>539 , .3,ifelse(area>519,.2,.1))),      # .3 is Aleutian, .2 is NW, .1 SE
      strata3= as.factor(seas + ifelse(area>519 , .2, .1)                   )  ,    # .1 is Aleutians and SE, .2 is NW, 
      strata4= as.factor(ifelse(qtr==1|qtr==4,1,2) + ifelse(area>519 , .2, .1)),    # 1=Winter-fall 2=spring-summer, .1 is Aleutians and SE, .2 is NW, 
      len    = len, sex = ifelse(SEX=="F",1,2), freq = freq,     # recodes sex etc
      haul   = as.integer(as.factor(haul)),                              # Sets hauls/offloads to unique integer values
      wtfreq = freq * 4.15E-06 * len^3.241067735  # Set wt frequency for summing
    )

ls()
glimpse(ldf)
  # summarize by gear and strata
# Preliminaries
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(colour="grey80", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=18)) + theme(axis.title.x=element_text(size=22) ,axis.title.y=element_text(size=22))
mytheme <- mytheme + theme(panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black",fill=NA,size=1.2))
  ldf %>% filter(!is.na(gear))%>% group_by(yr,gear,strata2) %>% summarise(Nsamples = sum(freq)) %>% ggplot(aes(x=yr,y=Nsamples)) +
    geom_bar(aes(fill=gear),stat="identity",colour="black",position="dodge")  + facet_grid(strata2 ~ ., scales="free_y") + mytheme
    geom_bar(aes(fill=strata2),stat="identity",colour="black")  + facet_grid(gear ~ .) + mytheme
  ldf %>% filter(!is.na(gear))%>% group_by(yr,gear,strata) %>% summarise(Nsamples = sum(freq)) %>% ggplot(aes(x=yr,y=Nsamples)) +
    geom_bar(aes(fill=gear),stat="identity",colour="black",position="dodge")  + facet_wrap(~ strata , scales="free_y") + mytheme
  ldf %>% filter(!is.na(gear))%>% group_by(yr,gear,strata1) %>% summarise(Nsamples = sum(freq)) %>% ggplot(aes(x=yr,y=Nsamples)) +
    geom_bar(aes(fill=gear),stat="identity",colour="black",position="dodge")  + facet_wrap(~ strata1, scales="free_y") + mytheme
  ldf %>% filter(!is.na(gear))%>% group_by(yr,gear,strata3) %>% summarise(Nsamples = sum(freq)) %>% ggplot(aes(x=yr,y=Nsamples)) +
    geom_bar(aes(fill=gear),stat="identity",colour="black",position="dodge")  + facet_wrap(~ strata3, scales="free_y") + mytheme
  ldf %>% filter(!is.na(gear))%>% group_by(yr,gear,strata4) %>% summarise(Nsamples = sum(freq)) %>% ggplot(aes(x=yr,y=Nsamples)) +
    geom_bar(aes(fill=gear),stat="identity",colour="black",position="dodge")  + facet_wrap(~ strata4, scales="free_y") + mytheme

  ldf %>% filter(!is.na(gear))%>% group_by(yr,gear,qtr,zone) %>% summarise(Nsamples = sum(freq)) %>% ggplot(aes(x=yr,y=Nsamples)) +
    geom_bar(aes(fill=gear),stat="identity",colour="black",position="dodge")  + facet_wrap(~ qtr+zone+gear, scales="free_y") + xlab("Year") + mytheme

  lout <- ldf %>% transmute(strata,haul,sex,len,freq)
  write.table(lout,lout_file,quote=F,row.names=F,col.names=F)

  # Now for catch data 
  tbl_df(cd)
  cdf <- cd %>% filter(!is.na(gear),Year>1990,Year<2016,Spp=="Halibut" , area<550)  %>%
      transmute(
      yr   = as.integer(Year),
      gear   = ifelse(gear=='NPT'|gear=='TRW'|gear=='PTR',"Trawl","Fixed"),
      qtr    = qtr ,
      month  = trunc(WED/100),                                      # get month on fly
      zone   = ifelse(area>519 , "NW", "AI+SE")                     ,    # .1 is Aleutians and SE, .2 is NW, 
      seas   = ifelse(qtr<3,1,2) ,
      strata = (paste(gear,"Qtr",qtr,"Area",zone, sep="_") ),
      strata1= as.factor(qtr  + ifelse(area>519 , .2, .1)                    ),    # .1 is Aleutians and SE, .2 is NW, 
      strata2= as.factor(seas + ifelse(area>539 , .3,ifelse(area>519,.2,.1)) ),    # .3 is Aleutian, .2 is NW, .1 SE
      strata3= as.factor(seas + ifelse(area>519 , .2, .1)                    ),    # .1 is Aleutians and SE, .2 is NW, 
      strata4= as.factor(ifelse(qtr==1|qtr==4,1,2) + ifelse(area>519, .2,.1) ),    # 1=Winter-fall 2=spring-summer, .1 is Aleutians and SE, .2 is NW, 
      Bycatch= Bycatch
    )
  cdf %>% filter(!is.na(gear))%>% group_by(yr,gear,qtr,zone) %>% summarise(Bycatch = sum(Bycatch)) %>% ggplot(aes(x=yr,y=Bycatch)) +
    geom_bar(aes(fill=gear),stat="identity",colour="black",position="dodge")  + facet_wrap(~ qtr+zone+gear, scales="free_y") + ylab("Bycatch (metric t)") + mytheme

  cdf %>% group_by(yr,strata) %>% mutate(C=sum(Bycatch)) %>% mutate(C2=sum(C))  %>% select(yr,strata,C,C2)
      group_by(yr) %>% summarise(sum(C2)) #%>%select(yr,C,strata,Bycatch) %>% group_by(yr,strata) %>%
        mutate(pc = sum(Bycatch)/C) %>% group_by(yr) %>% summarise(sum(pc))

# Compute length and weight frequencies          
sldf <- ldf %>% select(len,wtfreq,freq,yr,strata) %>% group_by(yr,strata) %>% summarise(wtfreq=sum(wtfreq))# %>% 
      mutate(len_str=sum(freq),wt_str=sum(wt)) %>% group_by(yr,strata,len) %>% 
      summarise(len_str=len_str,wt_str,plf=freq/len_str,pwf=wt/wt_str,wt=wt,freq=freq) 
names(sldf)
# Check to make sure proportions add to one...
sldf %>% group_by(yr,strata) %>% summarise(sum(wtfreq))
tbl_df(sldf)
tbl_df(scdf)
scdf <- cdf %>% group_by(yr,strata) %>% summarise(Bycatch = sum(Bycatch))
sldf <- ldf %>% select(len,wtfreq,freq,yr,strata) %>% group_by(yr,strata) %>% summarise(wtfreq=sum(wtfreq))# %>% 
#sldf <- ldf %>% filter(!is.na(gear)) %>% group_by(yr,strata,len) %>% summarise(freq = sum(freq),wt = sum(wt))
jdf <- full_join(sldf, scdf) %>%mutate(mult = Bycatch/wtfreq) %>% select(-wtfreq,-Bycatch)# %>% na.omit() 
dim(jdf)
View(jdf)
glimpse(jdf)
wjdf <- (full_join(ldf,jdf) %>% select(yr,gear,qtr,zone,strata,len,freq,wtfreq,mult) %>% mutate(wt2 = mult*wtfreq) )
# Check that re-calculated wtfrequencies sum to actual catches (they do...but include directed fishery observations too, mostly in 2008)
# tt <- wjdf %>% group_by(yr) %>% summarise(wt = sum(wt2))
# tt <- cdf %>% group_by(yr) %>% summarise(byc = sum(Bycatch))
# wlf <- wjdf %>% mutate(wlf = mult*freq) %>% group_by(yr,len) %>% summarise(LF = sum(wlf))
# tt <- wjdf %>% mutate(wlf = mult*freq) %>% group_by(gear,len) %>% summarise(LF = sum(wlf))
wlf <- wjdf %>% group_by(yr,gear,len) %>% summarise(LF = sum(mult*freq,na.rm=TRUE),lf=sum(freq,na.rm=TRUE))
wlf$wt <- 4.15E-06*wlf$len^3.241067735
# tt <- wjdf %>% group_by(gear,len) %>% summarise(LF = sum(freq))

#--- Survey data
btslf <- read.table("~/Dropbox/Halibut/Assessment Data/shelflf.dat",header=T)

btslf$wt <- 4.15E-06*btslf$len^3.241067735

swlf <- btslf %>% select(yr,len,freq,wt)
swlf$gear <- "Trawl survey"

#New corrected LF from fishery observer data
glimpse(wlf)
# Overall avg wt by gear
wlf %>% mutate(wtf = wt*LF) %>% group_by(gear) %>% summarise(awt = sum(wtf)/sum(LF),nf=sum(LF))
# Overall avg wt by year and gear
tt <- wlf %>% mutate(wtf = wt*LF) %>% group_by(yr,gear) %>% summarise(awt = sum(wtf)/sum(LF),nf=sum(LF))
tt
stt <- swlf %>% mutate(wtf = wt*freq) %>% group_by(yr,gear) %>% summarise(awt = sum(wtf)/sum(freq),nf=sum(freq)) 
tt <- rbind(stt,tt) #%>% mutate(wtf = wt*freq) %>% group_by(yr,gear) %>% summarise(awt = sum(wtf)/sum(freq),nf=sum(freq)) 
names(stt)

tt %>% filter(gear=="Trawl"|gear=='Fixed'|gear=="Trawl survey") %>% 
   ggplot(aes(x=yr,y=awt,col=gear,fill=gear)) + geom_line(size=.75) + expand_limits(y=0) + geom_smooth(size=2) +
   xlab('Year') + ylab('Average halibut weight (kg)')+ mytheme

tt %>% filter(gear=="Trawl"|gear=='Fixed'|gear=="Trawl survey") %>% 
   ggplot(aes(x=len,y=awt,col=gear,fill=gear)) + geom_line(size=.75) + expand_limits(y=0) + geom_smooth(size=2) +
   xlab('Year') + ylab('Average halibut weight (kg)')+ mytheme
   
# Since 1991 by gear and year including survey
slf <- dlf %>% filter(yr>1990,gear==1|gear==8|gear==2) %>% select(yr,gear,len,freq) %>% 
       group_by(yr, gear) %>% mutate(Nyr = sum(freq),prop=freq/Nyr) %>% 
       group_by(yr, gear,len) %>% summarise(freq=sum(freq),proportion=sum(prop)) %>%
       select(yr, gear,len,freq,proportion) %>% arrange(yr, gear,len) %>% group_by(yr, gear) %>% 
       mutate(cp = cumsum(proportion))
str(slf)
tbl_df(slf)

#msdlf <- sdlf %>% select(yr,len,freq) %>% mutate(len=len/10) %>% 
msdlf <- sdlf %>% select(yr,len,freq) %>%  group_by(yr) %>%
      mutate(N = sum(as.numeric(freq)),prop=freq/N) %>% group_by(yr,len) %>%
      summarise(proportion=sum((prop))) %>% mutate(cp = cumsum(proportion)) 
    msdlf
#ssdlf <- sdlf %>% filter(yr>2007) %>% select(len,freq) %>% mutate(len=len/10) %>% 
ssdlf <- sdlf %>% filter(yr>1990) %>% select(yr,len,freq) %>% 
      group_by(yr) %>% mutate(N = sum(as.numeric(freq)),prop=freq/N) %>% group_by(yr,len) %>%
      summarise(proportion=sum(prop)) %>% mutate(cp = cumsum(proportion)) 
ssdlf$gear = 10
tslf <- bind_rows(slf,ssdlf)
tbl_df(tslf)
slf$gear <-as.factor(slf$gear)
tslf$gear <-as.factor(tslf$gear)

# Size distribution comparison survey and trawl=======================================
names(ptslf)
tt <- tslf %>% filter(gear==1|gear==8|gear==10,len>9) %>% group_by(gear,len) %>% summarise(proportion = sum(proportion)) 
p  <-ggplot(tt) + geom_line(aes(x=len,y=proportion,col=gear),stat="identity",size=1) + xlim(c(0,120)) +
     scale_colour_manual(values=c("red","blue","green") ,labels=c("Trawl fishery","Longline fishery", "survey")  ) + mytheme +
     ylab("proportion of halibut") + xlab("Length (cm)") # + facet_grid(~ gear) 
print(p)



# Seasonal differences
wlf %>% filter(gear=="Trawl"|gear=='Fixed') %>% 
   mutate(wtf = LF) %>% group_by(week,gear) %>% summarise(awt = sum(wtf)/sum(freq),nf=sum(freq))  %>%
   ggplot(aes(x=week,y=awt,col=gear,fill=gear)) + geom_line(size=.75) + expand_limits(y=0) + geom_smooth(size=2) +
   xlab('Week') + ylab('Average halibut weight (kg)')+ mytheme

tt <- wlf %>% mutate(wtf = wt*freq) %>% group_by(week,gear) %>% summarise(awt = sum(wtf)/sum(freq),nf=sum(freq)) 
# Trawl avg wt by week
 tt %>% filter(gear=="Trawl"|gear=='Longline') %>% ggplot(aes(x=yr,y=awt,col=gear,fill=gear)) + geom_line(size=.75) + expand_limits(y=0) + geom_smooth(size=2) +
# Longline avg wt by week
 tt %>% filter(gear=='Longline'|gear=='Trawl') %>% ggplot(aes(x=week,y=awt,col=gear,fill=gear)) + geom_line(size=.75) + expand_limits(y=0) + geom_smooth(size=2) +
   xlab('Week of year') + ylab('Average halibut weight (kg)')+ mytheme



View(tt)
write.csv(tt,"shit2.csv")
names(wjdf)
View(wlf)

tbl_df(jdf)
names(wjdf)
dim(jdf)
names(ldf)
dim(sldf)
dim(scdf)
names(sldf)
names(scdf)
tbl_df(scdf)
tbl_df(sldf)
tbl_df(jdf)
names(ldf)
names(cdf)
dim(sldf)
d1 <- data_frame( x = letters[1:8], y = LETTERS[1:8], a = rnorm(8) )
full_join(d1, d2, by = c("x" = "x2", "y" = "y2"))
tbl_df(ldf)
t1 <- slice(ldf,1:1000)
t2 <- slice(cdf,1:1000)
t<- merge(t1,t2,by="strata")
dim(t)
names(t)
?merge
q
names(scdf[1,]) <-"yr"
full_join(sldf, scdf, by = c("x" = "", "y" = "y2"))

# Compute weights by strata to inflate by         
tt <- ldf %>% select(wt,freq,yr,strata) %>% group_by(yr,strata) %>% summarise_each(funs(sum(wt))) %>% 
      group_by(yr) %>% mutate(wt_str=sum(wt),p=wt/wt_str) %>%  arrange(strata)
tbl_df(tt)
# Check to make sure proportions add to one...
tt %>% group_by(yr) %>% summarise(sum(p))

tt %>% group_by(yr,strata) %>% summarise_each(funs(sum(wt_str))) %>% 
tt %>% group_by(yr,strata) %>% summarise(sum(plf),sum(pwf))






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
