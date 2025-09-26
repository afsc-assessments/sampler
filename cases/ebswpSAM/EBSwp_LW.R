radian
library(tidyverse)
#install.packages("data.table")
library(data.table)
library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)
#old_dir <- getwd()
doc_dir <- ("~/_mymods/ebswp/doc/")
doc_dir <- ("~/_mymods/afsc-assessments/ebs_pollock_safe/doc/")
#setwd("~/Onedrive/models/Atka/2016/LengthWeight")

#setwd("~/Onedrive/sampler/cases/ebswp")
source("../../sampleR/R/sampler.R")
Get_Age(2019)
# Read in raw dataframe rdf
#rdf <- read_csv2("~/OneDrive/sampler/cases/ebswp/imported/poll_age.csv")
#rdf <- read.csv("~/OneDrive/sampler/cases/ebswp/imported/norpac_age_reportpoll_age.csv",as.is=T,header=T)
#rdf <- read.csv("imported/pollbs.csv",as.is=T,header=T)
#rdf <- read.csv("~/OneDrive/sampler/cases/ebswpSAM/imported/norpac_age_report.csv",as.is=T,header=T)
#names(rdf)
#summary(rdf$Year)
yr=1991
    #----------------------------------
    # Strata definitions 1, 2, 3 are A-season, B_NW, B_SE
Get_Age <- function(yr) {
    ad <- read.csv(paste("imported/age",yr,".csv",sep="") ,as.is=T,header=F)
    hdr_age <- read.csv("imported/hdr_age.csv",as.is=T,header=F)
    names(ad) <- hdr_age
    ad$HAUL_OFFLOAD_DATE <-   dmy(ad$HAUL_OFFLOAD_DATE)
    adf <- ad %>% dplyr::filter(NMFS_AREA<540) %>%
      dplyr::transmute(
      	year   = yr,
        haul   = ifelse(is.na(HAUL_JOIN),PORT_JOIN,HAUL_JOIN),
        haul   = as.integer(as.factor(haul)),
        lat    = (LATDD_START  + LATDD_END )/2,
        lon    = (LONDD_START  + LONDD_END )/2,
        area   = NMFS_AREA, 
        month  = month(HAUL_OFFLOAD_DATE),
        seas   = ifelse(month>5, 2, 1), 
        strata = ifelse(seas==1, "A-season", ifelse(NMFS_AREA>519, "B-season, NW", "B-season, SE")), 
        sex = ifelse(SEX=="F",1,2) ,
        len    = LNGTH , age = ifelse(AGE==0,-9,AGE),
        wt = ifelse(is.na(WEIGHT) | WEIGHT==0,-9,WEIGHT) , age=ifelse(is.na(age),-9,age ) )
      return(adf)
 }   
 i=1991
 i=2019
 adf <- Get_Age(i)
 adf
names(adf) 
for (i in 1992:2024)
 adf <- rbind(adf, Get_Age(i))

#adf <- read_csv("imported/poll_age_all.csv")
adf <- read_csv("imported/akfin_age.csv")
# Stuff I like for plot look...
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(colour="grey80", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=18)) + theme(axis.title.x=element_text(size=22) ,axis.title.y=element_text(size=22))
mytheme <- mytheme + theme(panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black",fill=NA,size=1.2))
adf<- janitor::clean_names(adf)
summary(adf$year)
names(adf)
adf <- adf  |> select(year,haul_offload,haul_offload_date,nmfs_area,gear, fmp_subarea,gear,length=length_cm,weight=weight_kg,age,sex)
glimpse(adf)
summary(adf)
library(lubridate)
adftmp<- adf |> mutate(
haul   = as.integer(as.factor(haul_offload)),
#lat    = (LATDD_START  + LATDD_END )/2,
#lon    = (LONDD_START  + LONDD_END )/2,
area   = nmfs_area, 
month  = month(dmy_hm(haul_offload_date)),
seas   = ifelse(month>5, 2, 1), 
strata = ifelse(seas==1, "A-season", ifelse(nmfs_area>519, "B-season, NW", "B-season, SE")), 
sex = ifelse(sex=="F",1,2) ,
len    = length, 
age = ifelse(age==0,-9,age),
wt = ifelse(is.na(weight)|weight==0,-9,weight)
)
ymd_hm(adf$haul_offload_date)

glimpse(adftmp)
#age=ifelse(is.na(age),-9,age ) )


# Suss out what's needed
# select area, rename etc
#df <- as.data.frame(filter(rdf,"Weight (kg)">0,"NMFS Area"<540)) %>% select(age=Age,sex=Sex,Length="Length (cm)",wt="Weight (kg)",yr=Year,dt="Haul Offload Date", mo )
# Now narrown to main data range to be less than 45 cm and make new field 
#df <- filter(df,Length>=30, Length<=45,wt<1.8) %>% group_by(Length) %>% transmute(w_bar_L=mean(wt),wtstd = wt/w_bar_L,wt,mo,yr)

glimpse(adf)
names(adf)
glimpse(adftmp)
summary(adftmp)
str(adf)
names(adf) <- tolower( names(adf))
# Screen out pre 1997 data 
df <- as.data.frame(filter(adf,weight>0,year>1996) %>% 
  mutate(month=month(mdy_hm(haul_offload_date)))  |> 
  select(age=age,sex=sex,Length=length,wt=weight,year,month ) )
# or not 
df <- as.data.frame(filter(adftmp,weight>0          ) %>% mutate(month=month(mdy_hm(haul_offload_date)))  |>  select(age=age,sex=sex,Length=length,weight,year,month,strata ) )
summary(df)
# Screen out outliers
df <- filter(df,Length>=35, Length<=60,wt<3.0) %>% group_by(Length) %>% transmute(w_bar_L=mean(wt),wtstd = wt/w_bar_L,wt,month,year,Year=as.factor(year),sex)
dim(df)
#df <- group_by(df, Length) %>% transmute(w_bar_L=mean(wt),wtstd = wt/w_bar_L,wt,month,yr,Year)
#DT <- data.table(df)[Length>20 & Length<=65 & wt < 3., .(wt,month,yr,wtstd=wt/mean(wt)), Length]
#df <- DT
names(df)
# Look by year
p <- ggplot(df) + 
     geom_hline(yintercept=1,size=1) +
     scale_x_discrete(breaks=c(seq(1997,2024,2))) +
     geom_boxplot(aes(x=Year,y=wtstd,fill=Year),outlier.size=.2,width=.7,notch=TRUE,alpha=.3)  + ylim(c(0.85,1.15)) + xlab('Year') + 
     ylab('Standardized weight given length') + mytheme + guides(fill=FALSE) ;p
ggsave(paste0(doc_dir,"/figs/fsh_lw_anom_yr_box.pdf"),p,width=12,height=9)
doc_dir

library(ggthemes)
     p <- df %>% group_by(year) %>% summarise(Mean=mean(wtstd)) %>% ggplot(aes(x=year,y=Mean)) + geom_smooth() + geom_point(color="red",size=4) +
     scale_x_continuous(breaks=c(seq(1990,2024,2))) + ylab("Mean body mass anomaly") +
     geom_hline(yintercept=1,size=1) + mytheme + ylim(c(0.925,1.075)) ;p
ggsave(paste0(doc_dir,"/figs/fsh_lw_anom_yr_mean.pdf"),p,width=10,height=5)

# Look by strata/year
p <- ggplot(df) + 
     geom_hline(yintercept=1,size=1) +
     geom_boxplot(aes(x=year,y=wtstd,fill=year),outlier.size=.2,width=.7,notch=TRUE,alpha=.3)  + ylim(c(0.85,1.15)) + xlab('Year') + 
     scale_x_discrete(breaks=c(seq(1996,2024,2))) +
     facet_wrap(~ strata,nrow=3) + 
     ylab('Standardized weight given length') + mytheme + guides(fill=FALSE) 
p
ggsave(paste0(doc_dir,"/figs/fsh_lw_anom_str_yr_box.pdf"),p,width=10,height=10)

# Look means by strata/year
df.m <- df %>% group_by(strata,Year) %>% summarise(wtstd=mean(wtstd)) 
df.m$Year <- as.numeric(df.m$Year)+1990
p<- ggplot(df.m,aes(x=Year,y=wtstd,color=strata,shape=strata)) + geom_point(size=4) + 
     geom_smooth(span=.3) + mytheme + 
     scale_x_continuous(breaks=c(seq(1990,2020,2))) +
     ylab('Standardized weight given length') + facet_wrap(~ strata,nrow=3) + 
     geom_hline(yintercept=1,size=1) 
     p
ggsave(paste0(doc_dir,"/figs/lw_anom_str_mean.pdf"),p,width=10,height=10)

# Look means by month/year
df.m <- df %>% filter(month!=5&month!=11&month!=12) %>% group_by(month,Year) %>% summarise(wtstd=mean(wtstd)) 
df.m$Year <- as.numeric(df.m$Year)+1990
df.m$month <- as.factor(df.m$month)
p<- ggplot(df.m,aes(x=Year,y=wtstd,color=month,shape=month)) + geom_point(size=4) + 
     geom_smooth(span=.3) + mytheme + ylim(c(.8,1.2)) +
     scale_x_continuous(breaks=c(seq(1990,2020,2))) +
     ylab('Standardized weight given length') + facet_wrap(~ month,nrow=3) + 
     geom_hline(yintercept=1,size=1) 
     p
ggsave(paste0(doc_dir,"/figs/lw_anom_str_mean_month.pdf"),p,width=10,height=10)


# Look by strata/year/sex
df.m <- df %>% group_by(strata,Year,sex) %>% summarise(wtstd=mean(wtstd)) 
df.m$Year <- as.numeric(df.m$Year)+1990
df.m$sex <- as.factor(df.m$sex)
p<- ggplot(df.m,aes(x=Year,y=wtstd,color=sex,shape=strata)) + geom_point(size=4) + 
     geom_smooth(span=.3) + mytheme + 
     scale_x_continuous(breaks=c(seq(1990,2018,2))) +
     ylab('Standardized weight given length') + facet_wrap(~ strata,nrow=3) + 
     geom_hline(yintercept=1,size=1) 
p
ggsave(paste0(doc_dir,"/figs/lw_anom_str_mean_sex.pdf"),p,width=10,height=10)



ggplot(df.m,aes(x=Year,y=wtstd,fill=strata,color=strata)) + geom_line() + geom_smooth() + mytheme

str(df.m)
# Look by strata/year
p <- ggplot(df) + 
     geom_hline(yintercept=1,size=1) +
     geom_boxplot(aes(x=Year,y=wtstd,fill=Year),outlier.size=.2,width=.7,notch=TRUE,alpha=.3)  + ylim(c(0.85,1.15)) + xlab('Year') + 
     facet_wrap(~ strata,nrow=3) + ylab('Standardized weight given length') + mytheme + guides(fill=FALSE) 
     p
# Look by length
ggplot(df) + geom_boxplot(aes(x=as.factor(Length),y=wtstd,fill=as.factor(Length))) + ylim(c(0,2)) + xlab('Length (cm)') + ylab('Standardized weight (30-45 cm)') + mytheme +
geom_boxplot(outlier.size=.2)  

# Look by month (but order good!)
mymonths <- c("Jan","Feb","Mar", "Apr","May","Jun", "Jul","Aug","Sep", "Oct","Nov","Dec")
df$mnth <- factor(mymonths[df$month] , levels=month.abb)
p <- df %>% mutate(wtstd=ifelse(month==5,NA,ifelse(month>10,NA,wtstd))) %>% ggplot( aes(x=(mnth),y=wtstd,fill=as.factor(month)))+ 
    geom_hline(yintercept=1,size=2) + geom_violin(draw_quantiles=0.5)   + ylim(c(0.5,1.5)) + xlab('Month') + ylab('Standardized weight (30-65 cm)') + mytheme + guides(fill=FALSE)
p
ggsave(paste0(doc_dir,"figs/lw_anom_month.pdf",p,width=6,height=4)

# Look by year/month (but order good!)
mymonths <- c("Jan","Feb","Mar", "Apr","May","Jun", "Jul","Aug","Sep", "Oct","Nov","Dec")
df
df$mnth <- factor(mymonths[df$month] , levels=month.abb)
df$yrmnth <- df$year + df$month
p <- df %>% filter(year>2000) %>% mutate(wtstd=ifelse(month==5,NA,ifelse(month>10,NA,wtstd))) %>% 
    ggplot( aes(x=as.factor(yrmnth),y=wtstd,fill=mnth))+ 
    geom_hline(yintercept=1,size=2) + geom_violin(draw_quantiles=0.5)   + ylim(c(0.5,1.5)) + xlab('Month') + ylab('Standardized weight (30-65 cm)') + mytheme + guides(fill=FALSE)
    p
ggsave(paste0(doc_dir,"figs/lw_anom_month.pdf",p,width=6,height=4)

df$mnth <- factor(mymonths[df$month] , levels=month.abb)
df.m <- df %>% group_by(strata,month) %>% summarise(wtstd=mean(wtstd)) 
df.m$Year <- as.numeric(df.m$Year)+1990
str(df.m)
p<- ggplot(df.m,aes(x=month,y=wtstd,color=strata,shape=strata)) + geom_point(size=4) + 
     geom_smooth(span=.9) + mytheme + ylim(c(.85,1.15)) + 
     scale_x_continuous(breaks=c(seq(1,12,2))) +
     ylab('Standardized weight given length') + facet_wrap(~ strata,nrow=3) + 
     geom_hline(yintercept=1,size=1) 
     p
ggsave(paste0(doc_dir,"/figs/lw_anom_str_mean.pdf"),p,width=10,height=10)
names(df)
##############################
# For a-season find out what's driving early period bigger wt-age
##############################
dft <- df %>% filter(strata=="A-season") %>% mutate(mnmnth=mean(month)) %>% group_by(year) %>%summarise(wtstd=mean(wtstd),Month=mean(month/mnmnth)) 
p <-    ggplot(dft,aes(x=year,y=wtstd)) + mytheme +  ylab('') + xlab("Year") + scale_x_continuous(breaks=c(seq(1990,2020,2))) + ylim(c(0.6,1.4))+ geom_hline(yintercept=1,size=2,color="brown") 
   p <- p+ geom_point(color="blue",size=3) + geom_smooth(span=.5) 
p
dft
##############################
# Shows mean month anomaly
p <-    ggplot(dft,aes(x=year,y=Month)) + mytheme +  ylab('Month anomaly') + xlab("Year") + scale_x_continuous(breaks=c(seq(1990,2020,2))) + ylim(c(0.6,1.4))+ geom_hline(yintercept=1,size=2,color="brown")  +
       geom_point(aes(x=year,y=Month),color="red",size=3)+ geom_smooth(aes(x=year,y=Month),span=.5,color="red") 
     #ylab('Standardized weight given length') + xlab("Year") + 
p   
##############################
p <- ggplot(df,aes(x=strata,y=wtstd,fill=strata)) + 
  scale_y_continuous(limits=c(0.3,2)) + xlab("Strata") + ylab("Pollock body mass anomaly") +
   geom_boxplot(outlier.size=.2) + geom_hline(yintercept=1,size=2,color="brown") + guides(fill=FALSE) + mytheme

   # points try
p <- ggplot(df,aes(x=strata,y=wtstd,fill=strata)) + 
  scale_y_continuous(limits=c(0.85,1.15)) + xlab("Strata") + ylab("Pollock body mass anomaly") +
   geom_boxplot(outlier.size=.2) + geom_hline(yintercept=1,size=2,color="brown") + guides(fill=FALSE) + mytheme
p
# Find out if 2020 is odd, add a field designate to dataframe
glimpse(df)
df <- df %>% mutate(period=ifelse(year==2020,"2020","1991-2019"))
p <- ggplot(df,aes(x=period,y=wtstd,fill=period)) + 
  scale_y_continuous(limits=c(0.85,1.15)) + xlab("Strata") + ylab("Pollock body mass anomaly") +
   geom_boxplot(outlier.size=.2) + geom_hline(yintercept=1,size=2,color="brown") + guides(fill=FALSE) + mytheme + facet_wrap(strata~.)
p + coord_flip()
p


ggsave(paste0(doc_dir,"figs/lw_anom_str.pdf",p,width=6,height=4)
getwd()
doc_dir
ggsave(paste0(doc_dir,"figs/lw_anom_mo.pdf",p,width=8,height=4)
?geom_violin
ggsave("lw_anom_mo.pdf",p,width=8,height=4)
dim(df)
# 
ggplot(df,aes(x=Length,y=wt)) + geom_boxplot(aes(x=as.factor(Length),y=wt,colour=as.factor(yr))) +ylab('Weight (kg)') + mytheme

df <- adf %>%                  group_by(strata) %>% mutate(mwt = wt/mean(wt),mlen=len/(mean(len)))
df
p <- ggplot(df,aes(x=strata,y=wtstd,fill=strata)) + 
  scale_y_continuous(limits=c(0.3,2)) + xlab("Strata") + ylab("Pollock body mass anomaly") +
   geom_boxplot(outlier.size=.2) + geom_hline(yintercept=1,size=2,color="brown") + guides(fill=FALSE) + mytheme
p
ggsave("lw_anom1.png")


kkkkkkk
library(tidyr)
library(maps)
library(mapproj)
library(mapdata)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)
source("~/Downloads/ADFGareas.R")
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=18)) + theme(axis.title.x=element_text(size=24) ,axis.title.y=element_text(size=24))
mytheme <- mytheme + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
mytheme <- mytheme + theme(panel.background = element_rect(fill="white"),panel.border = element_rect(size=2))
mytheme <- mytheme + theme( panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black", fill=NA, size=1))
#a Select and summarize age data
#########################
# Compute some normalized mean weights
# NOTE this is mean weight in the CATCH not by length)
names(adft)

df.m <- adft %>% filter(wt>0) %>% group_by(strata,year) %>% summarise(mwt_anom=mean(wt/mwt)) 
df.m
names(adft)
p<- ggplot(df.m,aes(x=year,y=wt,color=strata,shape=strata)) + geom_point(size=4) + 
     geom_smooth(span=.3) + mytheme + 
     scale_x_continuous(breaks=c(seq(1990,2018,2))) +
     ylab('Standardized weight given length') + facet_wrap(~ strata,nrow=3) + 
     geom_hline(yintercept=1,size=1) 
p
ggsave(paste0(doc_dir,"/figs/lw_anom_str_mean.pdf"),p,width=10,height=10)
adf$area <- as.factor(adf$area)
adft <- adf %>%  na.omit() %>%   group_by(age,strata) %>% mutate(mwt = wt/mean(wt),mlen=len/(mean(len)))
adft <- adf %>%                  group_by(age,strata) %>% mutate(mwt = wt/mean(wt),mlen=len/(mean(len)))
p <- adft %>%  na.omit() %>% ggplot(aes(x=as.factor(year),y=mwt,fill=strata)) + facet_wrap(~ strata,nrow=3) +
  scale_y_continuous(limits=c(0.3,2)) + xlab("Year") + ylab("Pollock body mass anomaly") +
   geom_boxplot(outlier.size=.2) + geom_hline(yintercept=1) + mytheme
p
ggsave("wt_age_anom.png")
adft <- adf %>%                  group_by(age,strata) %>% mutate(mwt = wt/mean(wt),mlen=len/(mean(len)))
p <- adft %>%  na.omit() %>% ggplot(aes(x=as.factor(year),y=mwt,fill=strata)) + facet_wrap(~ strata,nrow=3) +
  scale_y_continuous(limits=c(0.3,2)) + xlab("Year") + ylab("Pollock body mass anomaly") +
   geom_boxplot(outlier.size=.2) + geom_hline(yintercept=1) + mytheme
#########################

adft <- adf %>%  group_by(age,area) %>% mutate(mwt = wt/mean(wt),mlen=len/(mean(len)))
p <- ggplot(adft,aes(x=as.factor(year),y=mlen))+  facet_wrap(~ area,nrow=3) + scale_x_discrete(breaks=c(seq(1982,2014,2))) +
  scale_y_continuous(limits=c(0.6,1.4)) + xlab("Year") + ylab("Pollock length anomaly") +
   geom_boxplot(aes(fill=area),outlier.size=.2) + geom_hline(yintercept=1) + mytheme
p
#########################
names(ad)

# By adfg
adfg <- ad %>%  filter(haul_type==3,yr>1981,age<=25,age>0,!is.na(age),!is.na(gear_temp)) %>% 
  mutate(long=(beg_lon+end_lon)/2,lat=((beg_lat+end_lat)/2),adfg=getADFG(long,lat)) %>% 
  select(yr,adfg,area,age,len,wt,haul_type,gear_temp) 
adfgm <- adfg %>%  group_by(age) %>% mutate(mwt = wt/mean(wt),mlen=len/(mean(len))) 
adfgm <- adfgm %>%  group_by(adfg) %>% summarise(Length_anomaly=median(mlen))
names(adfgm)
dim(adfgm)
(adfgm)

# By latitude #########################
adf <- ad %>%  filter(haul_type==3,yr>1981,age<=25,age>0,!is.na(age),!is.na(gear_temp),area=="BS"|area=="GOA") %>% 
  mutate(lat=trunc((beg_lat+end_lat)/2)) %>% select(yr,lat,area,age,len,wt,haul_type,gear_temp) 
adftbs <- adf %>%  group_by(age) %>% mutate(mwt = wt/mean(wt),mlen=len/(mean(len))) 
adftbs
p <- ggplot(adftbs,aes(x=as.factor(lat),y=mlen))+  
  scale_y_continuous(limits=c(0.8,1.2)) + xlab("Latitude") + ylab("Pollock length anomaly") + mytheme + geom_hline(yintercept=1) +
   geom_boxplot(width=.6,aes(fill=area),outlier.size=.2,notch=T,notch.width=.5,colour="brown")  #+geom_vline(xintercept=(1.5:9.5),colour="grey",linetype="solid",linewidth=2) 
p <- p + coord_flip()
p # + ggtitle("Weight-at-Age (grams)")  + mytheme
dim(adftbs)
# Check data used...
write.csv(adftbs,"bs_agelen.csv")

## Growth patterns by region 
adf <- ad %>%  filter(haul_type==3,yr>1981,age<=10,age>0,!is.na(age),!is.na(gear_temp),area!="HBS",area!="HG") %>% 
  select(yr,area,age,len,wt,haul_type,gear_temp) 
names(adf)
mytheme <- mytheme + theme(text=element_text(size=28)) + theme(axis.title.x=element_text(size=32) ,axis.title.y=element_text(size=32))
p <- ggplot(adf,aes(x=as.factor(age),y=wt))+  
  scale_y_continuous(limits=c(0,15e2)) + xlab("Age") + ylab("Body mass of pollock (grams)") +
   geom_boxplot(width=.6,aes(fill=area),outlier.size=.2,notch=T,notch.width=.5,colour="brown")+  #+geom_vline(xintercept=(1.5:9.5),colour="grey",linetype="solid",linewidth=2) 
   mytheme
p
?colour
# + ggtitle("Weight-at-Age (grams)")  + mytheme

