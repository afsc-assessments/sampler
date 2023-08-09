radian
library(ggthemes)
library(scales)
library(ggridges)
library(lubridate)
library(tidyverse)
#install.packages("scatterpie")
#install.packages("data.table")
library(scatterpie)
library(data.table)
library(xtable)
mytheme <- theme_few()
doc_dir <- "~/_mymods/ebswp/doc"
#setwd("~/Onedrive/ebswp/data/sampler/cases/ebswp")
# source sampler.R
source("../../sampleR/R/sampler_EBSwp.R")

#####Catch compilation#############################################################
# Old format for catch
#cd <- (read.csv(paste("imported/akfincat.csv",sep=""),as.is=T,header=F))
#hdr_cat <- read.csv("imported/hdr_cat_short.csv",as.is=T,header=F)
# New format for catch from saved akfin run
cd        <- (read_csv("imported/akfin_cat.csv")) #,as.is=T,header=T))
hdr_cat   <- read.csv("imported/hdr_cat.csv",as.is=T,header=F)
names(cd) <- hdr_cat[c(1,20,21,22,15,16,17,23,19,27,4 )]
glimpse(cd)
cd %>% filter(FMP_Subarea=="BS",Species_Group=="Pollock") %>%group_by(Year) %>% summarise(sum(Catch)) %>% print(n=Inf)
#head(cd)
#hdr_cat   <- read.csv("imported/hdr_cat_short.csv",as.is=T,header=F)
#names(cd) <- hdr_cat
# Write all the catch files from R instead of awk...
#   subset data.table to be columns used by sampler
#dd <- data.table(cd[,c(1,20,21,22,15,16,17,23,19,27,4 )])
#cdf <- cd %>% filter(Year==2019,FMP_Area=="BSAI",FMP_Subarea=="BS",Species_Group=="Pollock") %>% transmute(
cdf <- cd %>% filter(Year==2021,FMP_Area=="BSAI",FMP_Subarea=="BS",Species_Group=="Pollock") %>% transmute(
    area=NMFS_Area,month=as.integer(as.numeric(WED)/100), strata=ifelse(month<6,1,ifelse(area>519,2,3)), catch=Catch )  %>% 
    select(strata, catch) %>% group_by(strata) %>% summarise(catch=sum(catch))
glimpse(cdf)
#####END of Catch compilation#############################################################

#Get adt (age data tibble)--------------------------
#adt <- vroom::vroom("imported/akfin_age.csv",guess_max=50000)
adt <- vroom::vroom("imported/poll_age.csv",guess_max=50000)

adt$HAUL_OFFLOAD_DATE<-dmy(adt$HAUL_OFFLOAD_DATE)
?dmy
glimpse(adt)
adt %>% filter(`FMP_SUBAREA`=="BS",!is.na(AGE)) %>%group_by(YEAR) %>% summarise(n()) %>% print(n=Inf)

#Get ldt (length data tibble)--------------------------
#ldt1 <- vroom::vroom("imported/akfin_len.csv",guess_max=50000)
#ldt2 <- vroom::vroom("imported/norpac_len.csv",guess_max=50000)
ldt <- vroom::vroom("imported/poll_len.csv",guess_max=50000)
#ldt$HAUL_OFFLOAD_DATE<-lubridate::dmy(ldt$HAUL_OFFLOAD_DATE)
glimpse(ldt)
#glimpse(ldt2)
#ldt <- ldt2
#--------------------------

#--------------------------
# Now run the model script  
#--------------------------
SetBS(n=1000)
# SetBS(n=1)
 i=2021;Sampler(iyr=i,io=T)
for (i in 2021:2020) Sampler(iyr=i)
# Idea for getting a 2021 age comp, change all the age data >0 to 2021...and leave 2021 data alone for LW
names(adt)
#adt <- 
adt <- adt1
unique(adt$AGE)
# Get just age data (historically)
yrtest=2020
yrtest=2021
t1 <- adt %>%mutate(AGE=ifelse(is.na(AGE),-9,AGE))%>% filter(YEAR!=yrtest&AGE>0) 
# Get year of data for testing (2021)  and replace all data as if it was only that year
t1 <- adt %>% filter(YEAR==yrtest) %>% rbind(t1) %>% mutate(YEAR=yrtest)
summary(t1)
adt <- t1
Sampler(iyr=yrtest)
adt <- adt1

library(ggthemes)
#--------------------------
# Read in wt-age bootstraps
#--------------------------
  # Stratified
  wsdt <- NULL
for (i in 2021:1991) wsdt <- rbind(wsdt,read_table(paste0("results/str_wtage",i,".rep"),col_names=FALSE)) 
names(wsdt) <- c("bs","hdr","yr","strata",1:15)
dim(wsdt)
wsdtl <- wsdt %>% pivot_longer( cols = 5:19, values_to = "wt",names_to="age") %>% mutate(age=as.numeric(age))
  #ggplot(aes(x=age,y=reorder(Year, desc(Year)),height=Mean_wt)) + 
#Plot by strata         
wsdtl %>% group_by(Year=as.factor(yr),strata=as.factor(strata),age) %>% summarise(Mean_wt=mean(wt)) %>%
  ggplot(aes(x=age,y=reorder(Year,desc(Year)),height=Mean_wt)) + 
    geom_density_ridges(stat = "identity",scale = 8, alpha = .2, fill="salmon",color="black") + theme_few() +
         xlim(c(1,15))+ ylab("Year") + 
         xlab("Age") + facet_wrap(.~strata) 
#Plot by strata         
wsdtl %>% group_by(strata=as.factor(strata),age) %>% summarise(Mean_wt=mean(wt)) %>%
  ggplot(aes(x=age,y=Mean_wt,color=strata)) + 
    geom_line(stat = "identity",size = 3) + theme_few() +
         xlim(c(1,15))+ ylab("Mean weight (kg)") + xlab("Age") #+ facet_wrap(.~strata) 

wsdt <- wsdt %>% mutate(strata=ifelse(strata==1,"A-season",ifelse(strata==2,"B-season-NW",ifelse(strata==3,"B-season-SE","Annual"))))
wsdtl <- wsdt %>% pivot_longer(cols=5:19,names_to="age",values_to="wt") %>% mutate(age=as.numeric(age))
glimpse(wsdtl)
summary(wsdtl)
wsdtl %>% group_by(strata,age) %>% summarise(mwt=mean(wt)) %>% ggplot(aes(x=age,y=mwt,col=strata)) + geom_line() + theme_few() + geom_point()
wsdtl %>% group_by(strata,age) %>% ggplot(aes(x=as.factor(age),y=wt,fill=strata)) + geom_boxplot() + theme_few() 
wsdtl %>% filter(yr==2019,age>2,age<10) %>% group_by(strata,Age=as.factor(age)) %>% ggplot(aes(x=Age,y=wt,fill=strata)) + geom_boxplot() + theme_few() 

  # Aggregated, should be same as 9999?
wdt <- NULL
for (i in 2021:1991){
  wdd <- (read_table(paste0("results/wtage",i,".rep"),col_names=FALSE))
  names(wdd) <- c("bs","str","yr",1:15)
  wdd <- wdd[-1001,]
  wdt <- rbind(wdt,wdd)
} 
names(wdt)
#--Fishery weights-at-age
mnwt<-wdt %>% pivot_longer(names_to="age",values_to="wt",cols=4:18) %>%
       transmute(yr,age=as.numeric(age),wt) %>% group_by(yr,age) %>% summarise(wt=mean(wt)) %>%
       pivot_wider(names_from=age,values_from=wt)
write_csv(mnwt,file="wdt.csv")
sdwt<-wdt %>% pivot_longer(names_to="age",values_to="wt",cols=4:18) %>%
       transmute(yr,age=as.numeric(age),wt) %>% group_by(yr,age) %>% summarise(wt=sd(wt)) %>%
       pivot_wider(names_from=age,values_from=wt)
write_csv(sdwt,file="sdwdt.csv")
#--Survey weights-at-age
sage <- read_csv("imported/srvage.csv",guess_max=5000,col_names=FALSE)
names(sage) <- c( "CJ","HJ","REGION","VESS","CRUISE","HAUL", "SPECIMENID", "BIOSTRATUM", "SPECIES_CODE", "LENGTH", "SEX", "WEIGHT", "AGE", "MATURITY", "MATURITY_TABLE", "GONAD_WT")
glimpse(sage)
mnwt<- sage %>% filter(REGION=='BS',AGE>0,WEIGHT>0) %>% transmute(AGE,WEIGHT=WEIGHT/1000,year=floor(CRUISE/100)) %>%
group_by(year,AGE) %>% summarize(mn_wt=mean(WEIGHT)) 
mnwt
# Matrix of sd's 
sage %>% filter(REGION=='BS',AGE>0,WEIGHT>0) %>% transmute(AGE,WEIGHT=WEIGHT/1000,year=floor(CRUISE/100)) %>%
group_by(year,AGE) %>% summarize(sd_wt=sd(WEIGHT)) %>% print(n=Inf) %>%
filter(!is.na(sd_wt)) %>%
filter(year>1991,AGE<16,AGE>2) %>%
pivot_wider(names_from=AGE,values_from=sd_wt) %>% print(n=Inf)
#group_by(AGE) %>% summarise(mean(sd_wt))%>% print(n=Inf) %>% 


sage %>% filter(REGION=='BS',AGE>0,WEIGHT>0) %>% transmute(AGE,WEIGHT=WEIGHT/1000,year=floor(CRUISE/100)) %>%
group_by(year,AGE) %>% summarize(mn_wt=mean(WEIGHT), sd_wt=sd(WEIGHT),cv=sd_wt/mn_wt) %>% print(n=Inf) %>%
filter(!is.na(sd_wt)) %>%
filter(year>1991,AGE<16,AGE>2) %>%

mnwt %>% pivot_wider(names_from=AGE,values_from=mn_wt) %>% print(n=Inf)
getwd()

wds <- wd[as.numeric(age)>2 & yr>2016,.(avg=mean(wt),cv=sd(wt)/mean(wt)),.(yr,age)] 
wds <- wd[as.numeric(age)>2 ,.(avg=mean(wt),cv=sd(wt)/mean(wt)),.(yr,age)] 
wds$Year <- as.factor(wds$yr)
ggplot(wds,aes(x=age,y=avg,shape=Year)) + mytheme + geom_line()
wds
wds <- wd[as.numeric(age)<11,.(avg=mean(wt),cv=sd(wt)/mean(wt)),.(yr,age)] 

wds$Year <- as.factor(wds$yr)
wds.m    <- dcast(wds,Year~age,value.var="avg")
write.csv(wds.m,"wtage_mean.csv")
wds.m    <- dcast(wds,Year~age,value.var="cv")
write.csv(wds.m,"wtage_cv.csv")
wds.tab  <- xtable(wds.m,caption="test",digits=3)
str(wds.tab)
print(wds.tab, caption.placement = "top", include.rownames = FALSE)

tab <- xtable( dcast(wds,Year~age,value.var="cv"))
print(tab, caption.placement = "top", include.rownames = FALSE)

#--------------------------
# Read in catch-age estimates for sex catch age
# NOTE THIS IS DONE IN "Do_Plots.R in R directory of SAFE"
#--------------------------
  # Stratified
csdt <- NULL
#for (i in 2021:1991) csdt <- rbind(csdt,read_table(paste0("results/str_catage",i,".rep"),col_names=FALSE)) 
#names(csdt) <- c("bs","hdr","yr","strata",1:15)
#csdtl <- csdt %>% pivot_longer( cols = 5:19, values_to = "catch",names_to="age") %>% mutate(age=as.numeric(age))
csdt <- NULL
for (i in 2021:1991) csdt <- rbind(csdt,read_table(paste0("results/catage",i,".rep"),col_names=FALSE)) 
dim(csdt)
names(csdt) <- c("bs","hdr","yr",1:15)
csdt

csdtl <- csdt %>% pivot_longer( cols = 4:18, values_to = "catch",names_to="age") %>% mutate(age=as.numeric(age))
csdtl
  #ggplot(aes(x=age,y=reorder(Year, desc(Year)),height=Mean_wt)) + 
#csdtl %>% group_by(Year=as.factor(yr),age) %>% summarise(Catch=sum(catch)) %>%
csdtl %>% mutate(Year=as.factor(yr)) %>% group_by(Year,age) %>% summarise(Catch=sum(catch)) %>%
  ggplot(aes(x=age,y=reorder(Year,desc(Year)),height=Catch)) + 
   coord_cartesian(clip = "off") +
    geom_density_ridges(stat = "identity",scale = 4, alpha = .2, fill="salmon",color="black") + theme_few() +
         xlim(c(1,10))+ ylab("Year") + xlab("Age") #+ facet_wrap(.~strata) 

edt <- NULL
for (i in 2021:1991){
  edt <- rbind(edt, read_table(paste0("results/Est_",i,".dat")))
}
tail(edt)
edt %>% filter(type=="N",sex<3,year<2021,stratum<4) %>% mutate(season=ifelse(stratum==1,"A","B"),sex=ifelse(sex==1,"Male",ifelse(sex==2,"Female","Total"))) %>% group_by(year,sex,season) %>% summarize(Catch=sum(value)) %>% 
        ggplot(aes(x=year,y=Catch,color=season,shape=sex,linetype=season)) + theme_few(base_size=18) + 
        geom_point(size=4) + geom_line(size=1.4) + expand_limits(y=0) + 
        scale_x_continuous(breaks=seq(1990,2021,by=2)) + xlab("Year") + ylab("Catch in numbers")

#--------------------------
# Read in catch-age bootstraps
#--------------------------
i=2021
cdt <- (read.table(paste0("results/catage",i,".rep"),header=F))
names(cdt) <- c("bs","str","yr",1:15)
cdt <- cdt[-1001,]
cdt
for (i in 2020:1991){
  cdd <- (read.table(paste0("results/catage",i,".rep"),header=F))
  names(cdd) <- c("bs","str","yr",1:15)
  cdd <- cdd[-1001,]
  cdt <- rbind(cdt,cdd)
} 
summary(cdt)
head(cdt)
write.csv(cdt,file="cdt.csv")
cd <- data.table(melt(cdt, id.vars = c("bs","yr"), measure.vars = 4:18, variable.name = "age", value.name = "Catch"))
cd
#cd[FMP_Subarea=="BS",sum(Catch),.(Year)]
cds <- cd[as.numeric(age)>0 & yr>210,.(avg=mean(Catch),cv=sd(Catch)/mean(Catch)),.(yr,age)] 
cds$Year <- as.factor(cds$yr)
cds
ggplot(cds,aes(x=age,y=avg,shape=Year)) + theme_few() + geom_point()
names(cd )
cds <- cd[,.(avg=mean(Catch),cv=sd(Catch)/mean(Catch)),.(yr,age)] 
cds$Year <- as.factor(cds$yr)
cds.m    <- dcast(cds,Year~age,value.var="avg")
write.csv(cds.m,"catage.csv")
cds.cv   <- dcast(cds,Year~age,value.var="cv")
write.csv(cds.cv,"catage_cv.csv")
cds.m
tt <- cd %>% pivot_wider(names_from = age, values_from = Catch)
head(tt)
cd
write.csv(tt,"catage.csv")
write.csv(cd,"catage.csv")
tmp <- cd %>% filter(yr==2021) %>% group_by(bs)  %>% mutate(p=Catch/sum(Catch))
tmp %>% group_by(age) %>% summarize(mean(p))

 #fish_encounters
     #fish_encounters %>%
       #pivot_wider(names_from = station, values_from = seen)
     # Fill in missing values
     #fish_encounters %>% #pivot_wider( #names_from = station, #values_from = seen,

dvar_vector rtmp = elem_div((pobs-phat),sqrt(elem_prod(phat,(1-phat))));
  double vtmp;
  vtmp = value(norm2(rt20)/size_count(rtmp));
  return 1/vtmp;
  // Eq. 6
  av = 3:15  # Age vector
  mobs = sum(pobs*av) # Mean obs
  mhat = sum(phat*av);
  rtmp = mobs-mhat;
  stmp = (sqrt(sum(av*av*pobs) - mobs*mobs));
  stmp*stmp/(rtmp*rtmp)

# This can be modified to make catch-age table...
cds.tab  <- xtable(cds.m,caption="test",digits=3)
cds.tab
str(cds.tab)
print(cds.tab, caption.placement = "top", include.rownames = FALSE)

# table of length data by yr/strata/sex
library(lubridate)
names(ldt)
ldf <- ldt %>% transmute(
      Year=YEAR,
      haul = HAUL_OFFLOAD,            # Data come from at-sea or port
      month = month(parse_date_time(HAUL_OFFLOAD_DATE,orders="dmy")), # get month on fly
      seas = ifelse(month>5, 2, 1),                                   # Season 2 is B, uses month
      strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519 , 2, 3)),      # Strata >519 one, < another
      len = LENGTH, sex = ifelse(SEX=="F",1,2), freq = FREQUENCY) %>% 
      mutate(haul=as.integer(as.factor(haul)) )
# table of LW data by yr/strata/sex
ldf%>% group_by(Year,strata,sex) %>% summarize(n=sum(freq)) %>% 
       pivot_wider(names_from=2:3,values_from=n) %>% xtable()
#number of hauls and measurements
ldf %>% group_by(Year) %>% 
  summarise(haulcount = n_distinct(haul),N_lengths=sum(freq)) %>% print(n=Inf) 
adf <- adt %>% filter(NMFS_AREA<540) %>%
      transmute(
        #haul   = ifelse((HAUL_JOIN==""),PORT_JOIN,HAUL_JOIN),
        haul   = HAUL_OFFLOAD,
        month  = month(HAUL_OFFLOAD_DATE),
        seas   = ifelse(month>5, 2, 1), 
        strata = ifelse(seas==1, 1, ifelse(NMFS_AREA>519, 2, 3)), 
        #lat    = (LATDD_START+LATDD_END)/2,
        #lon    = (LONDD_START+LONDD_END)/2,
        sex    = ifelse(SEX=="F",1,2) ,
        len    = LENGTH , age = ifelse(AGE==0,-9,AGE),
        wt = ifelse(is.na(WEIGHT) | WEIGHT==0,-9,WEIGHT) , 
        age=ifelse(is.na(age),-9,age ) ,
        year=YEAR
        ) %>% mutate( haul = as.integer(as.factor(haul)) )

       #mutate(w_bar_L=mean(wt)) %>% ungroup()
       adf %>% filter(wt>0,year>2000,len<60,wt<2) %>% ggplot(aes(x=wt),fill="yellow") + geom_density()
       adf %>% filter(wt>0,year>2000,wt<20) %>% ggplot(aes(x=len),fill="yellow") + geom_density()

df <- adf %>% filter(wt>0,!is.na(wt),year>2000,len>=35, len<=60,wt<2.0) %>% 
       group_by(len) %>% 
       transmute(w_bar_L=mean(wt), wtstd = wt/w_bar_L,wt,month,year,
        Year=as.factor(year),strata,sex) %>% 
       filter(wtstd<2,wtstd>.5) %>%
       transmute(w_bar_L=mean(wt), wtstd = wt/w_bar_L,wt,month,year,
       Year,strata,sex) 

       glimpse(df);df %>% group_by(len) %>% summarise(mean(wtstd)) %>% print(n=Inf)
       df %>% ggplot(aes(x=w_bar_L)) + geom_density()
summary(df)
mytheme=theme_few()
mean.data <- function(x){
  out <- mean(x)
  names(out) <- c("y")
  return(out) }
median.quartile <- function(x){
  out <- quantile(x, probs = c(0.25,0.5,0.75))
  names(out) <- c("ymin","y","ymax")
  return(out) }

df %>% group_by(year) %>% summarise(N=n()) %>% ggplot(aes(x=year,y=N)) + geom_line() + theme_few()

p <- ggplot(df) + 
     geom_boxplot(aes(x=Year,y=wtstd),fill="lemonchiffon",color="salmon",
      outlier.size=.1,width=.8,notch=TRUE,notchwidth=.1,alpha=.8)  + ylim(c(0.85,1.15)) + xlab('Year') + 
     stat_summary(aes(x=Year,y=wtstd),fun=mean.data,geom='point',size=2,shape=19,color="blue") +
     geom_hline(yintercept=1,size=1) +
     scale_x_discrete(breaks=c(seq(1997,2021,2))) +
     ylab('Standardized weight given length') + mytheme + guides(fill="none") ;p
# check collapts years     
p <- ggplot(df,aes(y=wtstd)) + 
     geom_boxplot(fill="lemonchiffon",color="salmon") + #ylim(c(0.85,1.15)) + xlab('Year') + 
     geom_hline(yintercept=1,size=1) +
     ylab('Standardized weight given length') + mytheme + guides(fill="none") ;p

     geom_boxplot(aes(x=Year,y=wtstd,fill=Year),outlier.size=.2,width=.7,notch=TRUE,alpha=.3)  + ylim(c(0.85,1.15)) + xlab('Year') + 
     geom_violin(aes(x=Year,y=wtstd),fill="lemonchiffon",color="red",width=1.,
      #draw_quantiles = c(0.05, 0.5, 0.95))  + ylim(c(0.5,1.5)) + xlab('Year') + 
ggsave(paste0(doc_dir,"/figs/fsh_lw_anom_yr_box.pdf"),p,width=10,height=5)

library(ggthemes)
df
wtstd
     p <- df %>% group_by(year) %>% summarise(Mean=mean(wtstd)) %>% 
     ggplot(aes(x=year,y=Mean)) + geom_smooth() + geom_point(color="red",size=4) +
     scale_x_continuous(breaks=c(seq(1990,2022,2))) + ylab("Mean body mass anomaly") +
     geom_hline(yintercept=1,size=1) + mytheme + ylim(c(0.925,1.075)) ;p
ggsave(paste0(doc_dir,"/figs/fsh_lw_anom_yr_mean"),p,width=10,height=5)

# Look by strata/year
p <- ggplot(df) + 
     geom_hline(yintercept=1,size=1) +
     geom_boxplot(aes(x=Year,y=wtstd,fill=Year),outlier.size=.2,width=.7,notch=TRUE,alpha=.3)  + ylim(c(0.85,1.15)) + xlab('Year') + 
     scale_x_discrete(breaks=c(seq(1996,2022,2))) +
     facet_wrap(~ strata,nrow=3) + 
     ylab('Standardized weight given length') + mytheme + 
     guides(fill="none") ;p
ggsave(paste0(doc_dir,"/figs/fsh_lw_anom_str_yr_box.pdf"),p,width=10,height=10)

# Look means by strata/year
df.m <- df %>% group_by(strata,year) %>% summarise(wtstd=mean(wtstd)) 
df.m$Year <- df.m$year
df.m$strata <- as.factor(df.m$strata)
df.m
p<- ggplot(df.m,aes(x=Year,y=wtstd,color=strata,shape=strata)) + geom_point(size=4) + 
     geom_smooth(span=.3) + mytheme + 
     scale_x_continuous(breaks=c(seq(2000,2022,2))) +
     ylab('Standardized weight given length') + facet_wrap(~ strata,nrow=3) + 
     geom_hline(yintercept=1,size=1) ;p
ggsave(paste0(doc_dir,"/figs/lw_anom_str_mean.pdf"),p,width=10,height=10)

# Look means by month/year
df.m <- df %>% filter(month!=5&month!=11&month!=12) %>% group_by(month,year) %>% summarise(wtstd=mean(wtstd)) 
df.m$Year <- df.m$year
df.m$month <- as.factor(df.m$month)
p<- ggplot(df.m,aes(x=Year,y=wtstd,color=month,shape=month)) + geom_point(size=4) + 
     geom_smooth(span=.3) + mytheme + ylim(c(.8,1.2)) +
     scale_x_continuous(breaks=c(seq(1990,2022,5))) +
     ylab('Standardized weight given length') + facet_wrap(~ month,nrow=3) + 
     geom_hline(yintercept=1,size=1);p
ggsave(paste0(doc_dir,"/figs/lw_anom_str_mean_month.pdf"),p,width=10,height=10)


# Look by strata/year/sex
df.m <- df %>% group_by(strata,year,sex) %>% summarise(wtstd=mean(wtstd)) 
df.m$Year <- df.m$year
df.m$sex <- as.factor(df.m$sex)
df.m$strata <- as.factor(df.m$strata)
p<- ggplot(df.m,aes(x=Year,y=wtstd,color=sex,shape=strata)) + geom_point(size=4) + 
     geom_smooth(span=.3) + mytheme + 
     scale_x_continuous(breaks=c(seq(2000,2022,2))) +
     ylab('Standardized weight given length') + facet_wrap(~ strata,nrow=3) + 
     geom_hline(yintercept=1,size=1) ;p
ggsave(paste0(doc_dir,"/figs/lw_anom_str_mean_sex.pdf"),p,width=10,height=10)



ggplot(df.m,aes(x=Year,y=wtstd,fill=strata,color=strata)) + geom_line() + geom_smooth() + mytheme

str(df.m)
# Look by strata/year
p <- ggplot(df) + 
     geom_hline(yintercept=1,size=1) +
     geom_boxplot(aes(x=Year,y=wtstd,fill=Year),outlier.size=.2,width=.7,notch=TRUE,alpha=.3)  + ylim(c(0.85,1.15)) + xlab('Year') + 
     facet_wrap(~ strata,nrow=3) + ylab('Standardized weight given length') + mytheme + guides(fill="none") ;p
# Look by length
ggplot(df) + geom_boxplot(aes(x=as.factor(Length),y=wtstd,fill=as.factor(Length))) + ylim(c(0,2)) + xlab('Length (cm)') + ylab('Standardized weight (30-45 cm)') + mytheme +
geom_boxplot(outlier.size=.2)  

# Look by month (but order good!)
mymonths <- c("Jan","Feb","Mar", "Apr","May","Jun", "Jul","Aug","Sep", "Oct","Nov","Dec")
df$mnth <- factor(mymonths[df$month] , levels=month.abb)
p <- df %>% filter(Year>=2016) %>% mutate(wtstd=ifelse(month==5,NA,ifelse(month>10,NA,wtstd))) %>% 
ggplot( aes(x=(mnth),y=wtstd,fill=as.factor(month),group=Year))+ 
    geom_hline(yintercept=1,size=2) + geom_violin(draw_quantiles=0.5)   + ylim(c(0.5,1.5)) + xlab('Month') + 
    ylab('Standardized weight (30-65 cm)') + mytheme + guides(fill=FALSE) + facet_grid(Year~.)
p
ggsave(paste0(doc_dir,"figs/lw_anom_month.pdf",p,width=6,height=4)

######################################################
# Age -growth data 
agdf <-adf %>% filter(Year>2010,age>0,!is.na(age))
library(tidyverse)
write_delim(agdf,"agdf.dat")

# Mapping of cohorts
library(ggmap)
library(scales)
library(viridis)
register_google(key="AIzaSyCQu3tASptLB1EFx3E8BXPc2cMDNFL5FCw")
akmap <- get_googlemap(center=c(lon=-170,lat=57) ,color="bw", maptype='hybrid' ,zoom=5.0) #, xlim=c(-190,-130), ylim=c(51,63)) # set up mapframe
akmap <- get_googlemap(center=c(lon=-170,lat=57) ,color="color", maptype='hybrid' ,zoom=5.0) #, xlim=c(-190,-130), ylim=c(51,63)) # set up mapframe

# Map out cohorts
names(adt)
cohdf
adf
cohdf <- adf %>% filter(year>2012,!is.na(lon),age>0,!is.na(age)) %>% 
      mutate(Cohort=as.factor(year-age)) %>% filter(Cohort %in% c(2013,2012))
      #-- 2018
cohdf <- adf %>% filter(year>2012,!is.na(lon),age>0,!is.na(age)) %>% 
      mutate(Cohort=as.factor(year-age)) %>% filter(Cohort %in% c(2018))

mndfa<-  cohdf %>% group_by(Cohort,age) %>% summarise(lat=mean(lat),lon=mean(lon))
mndf <-  cohdf %>% group_by(Cohort) %>% summarise(lat=mean(lat),lon=mean(lon))

# Cohort by nmfs area
mndfna <-  cohdf %>% filter(strata>1) %>% group_by(Cohort,area=strata) %>% summarise(lat=mean(lat),lon=mean(lon))
mndfna
p1 <- ggmap(akmap) + 
  geom_point(aes(x= lon, y=lat, color=Cohort,shape=Cohort), size=8, alpha=1,data=mndfna) +
  geom_point(aes(x= lon, y=lat, color=Cohort ,shape=Cohort), size=6, alpha=1,data=mndfa) +
  geom_text(aes(x= lon, y=lat, label=age), size=3,alpha=1,data=mndfa) +
  xlab("Latitude") + ylab("Longitude") + theme_few(base_size=12)  +
  scale_x_continuous(limits = c(-180.5, -162.0), expand = c(0, 0)) +
  scale_y_continuous(limits = c(53.70, 60.50), expand = c(0, 0)); p1
ggsave(paste0("~/_mymods/ebswp/doc/figs/fsh_cohort_locales_noposition.pdf"),p1,width=7.5,height=5) 
p1 <- ggmap(akmap) + 
  geom_point(aes(x= lon, y=lat, color=Cohort,shape=Cohort ), size=.81, alpha=.2,data=cohdf) +
  geom_point(aes(x= lon, y=lat, color=Cohort ), size=12, shape=1,alpha=1,data=mndf) +
  geom_point(aes(x= lon, y=lat, color=Cohort ), size=6, shape=19,alpha=1,data=mndfa) +
  geom_text(aes(x= lon, y=lat, label=age), size=3,alpha=1,data=mndfa) +
  xlab("Latitude") + ylab("Longitude") + theme_few(base_size=12)  +
  scale_x_continuous(limits = c(-180.5, -162.0), expand = c(0, 0)) +
  scale_y_continuous(limits = c(53.70, 60.50), expand = c(0, 0)) #+ scale_color_viridis() + scale_fill_viridis()
  p1
  ggsave(paste0("~/_mymods/ebswp/doc/figs/fsh_cohort_locales.pdf"),p1,width=7.5,height=5) 

sagedf <- read_csv("~/_mymods/ebswp/data/bts/raceage.csv",col_names=FALSE) 
names(sagedf) <- c( "hauljoin", "region", "performance","date","latitude", "longitude","species_code","length","sex", "wt", "age","gonad_wt"  )
sagedf <- sagedf %>% mutate(length=length/10, date = dmy(date), year=year(date), Cohort=as.factor(year-age))
sagedf %>% ggplot(aes(x=as.factor(age),y=length,color=year)) + geom_violin(alpha=.5)
cohsdf <- sagedf %>% filter(Cohort %in% c(2012,2013))%>%print(n=Inf)
mcohsdf <- cohsdf %>% group_by(Cohort) %>% summarise(latitude=mean(latitude),longitude=mean(longitude))
mcohasdf <- cohsdf %>% group_by(Cohort,age) %>% summarise(latitude=mean(latitude),longitude=mean(longitude))
  #geom_point(aes(x= jitter(longitude), y=jitter(latitude), color=Cohort ), size=6, shape=19,alpha=1,data=cohsdf) +
  #geom_text(aes(x= jitter(longitude, y=latitude, label=age), size=3,alpha=1,data=cohsdf) +
p1 <- ggmap(akmap) + 
  geom_point(aes(x= jitter(longitude,120), y=jitter(latitude,60), color=Cohort,shape=Cohort ), size=1.2, alpha=.5,data=cohsdf) +
  geom_point(aes(x= jitter(longitude), y=jitter(latitude), color=Cohort ), size=9, shape=19,alpha=1,data=mcohsdf) +
  geom_point(aes(x= longitude, y=latitude, fill=Cohort ), size=3.4, shape=21,alpha=1,data=mcohasdf) +
  geom_text(aes(x= longitude, y=latitude, label=age), size=2,alpha=1,data=mcohasdf) +
  xlab("Latitude") + ylab("Longitude") + theme_few(base_size=12)  +
  scale_x_continuous(limits = c(-180.5, -160.0), expand = c(0, 0)) +
  scale_y_continuous(limits = c(53.70, 62.50), expand = c(0, 0)) ;p1
  ggsave(paste0("~/_mymods/ebswp/doc/figs/fsh_cohort_locales_survey.pdf"),p1,width=7.5,height=5) 

  #scale_color(name = "Cohort") #+
  #ggtitle("2021 survey catch rate difference from mean") + xlab("Longitude") +ylab("Latitude") + 
# # scale_color_gradient2(low = ("red"), mid="white",high = ("blue"),midpoint=0) + theme_few() +

adf %>% filter(len>0,wt>0) %>% group_by(Year,strata,sex) %>% 
        tally() %>% pivot_wider(names_from=2:3,values_from=n) #%>% xtable()
unique(adf$strata)
#adf %>% filter(Year>2010,age %in% c(1:10)) %>% group_by(age) %>% summarise(Length=mean(len),N=n()) %>% print(n=Inf) %>%
adf %>% filter(Year>2010,age %in% c(1:10)) %>% 
  ggplot(aes(x=as.factor(age),y=len)) + geom_violin(fill= "salmon",color="grey") + theme_few()
# table of age data by yr/strata/sex
adf %>% filter(age>0,Year>2010) %>% group_by(Year) %>% tally() 
adf %>% filter(age>0,Year>200) %>% group_by(Year) %>% summarise(nhauls=n_distinct(haul),Naged=n()) %>%print(n=Inf)
#%>% pivot_wider(names_from=2:3,values_from=n) %>% xtable()
adf %>% filter(age>0,Year>1999) %>% group_by(Year,strata,sex) %>% tally() %>% pivot_wider(names_from=2:3,values_from=4) %>% xtable()
  adf %>% filter(age>0.1) %>% group_by(Year) %>% summarise(nhauls=n_distinct(haul),N=n()) %>% print(n=Inf)
  adf %>% group_by(Year) %>% summarise(nhauls=n_distinct(haul),N=n()) %>% print(n=Inf)
  # NEED TO compute number of unique hauls
  tt <- ldt %>% filter(YEAR==2007) #%>% mutate(n_distinct(haul)) 
  names(tt)
  names(adt)
  glimpse(tt)


  unique(tt$HAUL_OFFLOAD)
 # ,[age>0,.(N_haul_ages=max(haul,na.rm=TRUE ),N_ages=.N),yr], ldt[yr<2018,.(N_haul_lengths=max(haul),N_lengths=sum(freq)/20),yr])
names(aldt) <- c("yr","N_haul_ages","N_ages","yr2",  "N_haul_lengths","N_lengths"     )
names(aldt)
aldt
al <- data.table(melt(aldt, measure.vars = c(2,3,5,6), variable.name = "measure", value.name = "N"))
is.factor(al$measure)
ggplot(al,aes(x=yr,y=N,colour=measure,shape=measure)) +  geom_point() + geom_line() + mytheme + xlab("Year") + ggtitle("Fishery samples")+
scale_colour_discrete(labels=c("Number of age-hauls ", "Number of ages", "Number of length-hauls","Number of lengths / 20")) +
scale_shape_discrete(labels=c("Number of age-hauls ", "Number of ages", "Number of length-hauls","Number of lengths / 20")) + guides(guide_legend(title=NULL))

al
al[,
ggplot(al,aes(x=yr,y=N,colour=measure,shape=measure)) +  geom_point() + geom_line() + mytheme + xlab("Year") + ggtitle("Fishery samples")+
scale_colour_discrete(labels=c("Number of age-hauls ", "Number of ages", "Number of length-hauls","Number of lengths / 20")) +
scale_shape_discrete(labels=c("Number of age-hauls ", "Number of ages", "Number of length-hauls","Number of lengths / 20")) + guides(guide_legend(title=NULL))


#--------------------------
# For survey look see file ebs_survey.R
#--------------------------
# Look at tally of lengths etc
dt <- NULL
for (i in 2021:1991) 
  dt <- rbind(dt,    data.table(read_table(paste0("results/LF_",i,".rep")) ) )
#Sample sizes by year
dt
# Nlengths table
dt %>% group_by(stratum,sex,year) %>% summarise(N=sum(nlen_samples)) %>% pivot_wider(names_from=1:2,values_from=4) %>%xtable(digits=0,big.mark=c("",","))
# NLW table
dt %>% group_by(stratum,sex,year) %>% summarise(N=sum(nwt_samples)) %>% pivot_wider(names_from=1:2,values_from=4) %>%xtable(digits=0,big.mark=c("",","))
# Nages table
dt %>% group_by(stratum,sex,year) %>% summarise(N=sum(naged)) %>% pivot_wider(names_from=1:2,values_from=4) %>%xtable(digits=0,big.mark=c("",","))
# Length frequency figure
#csdtl %>% mutate(Year=as.factor(yr)) %>% group_by(Year,age) %>% summarise(Catch=sum(catch)) %>%
dt
p1 <- dt %>% filter(year>1991) %>%group_by(Year=year,length) %>% summarise(freq=sum(freq)) %>%
  ggplot(aes(x=length,y=reorder(Year,desc(Year)),height=freq)) + 
   coord_cartesian(clip = "off") +
    geom_density_ridges(stat = "identity",scale = 3.8, alpha = .2, fill="salmon",color="salmon") + theme_few() +
         xlim(c(25,60))+ ylab("Year") + xlab("Length (cm)") #+ facet_wrap(.~strata) 
         p1
write_csv(p1$data,"Fsh_LFreq.csv")
# By season..........
dt %>% filter(year>1991) %>% mutate(season=ifelse(stratum==1,"A","B")) %>% group_by(season,Year=year,length) %>% summarise(freq=sum(freq)) %>%
  ggplot(aes(x=length,y=reorder(Year,desc(Year)),height=freq)) + 
   coord_cartesian(clip = "off") +
   geom_vline(xintercept=40,color="gold") +
    geom_density_ridges(stat = "identity",scale = 3.0, alpha = .3, fill="salmon",color="grey") + theme_few() +
         xlim(c(25,60))+ ylab("Year") + xlab("Length (cm)") + facet_wrap(.~season) 

dt <- NULL
for (i in 2021:1991) 
  dt <- rbind(dt,    (read_table(paste0("results/Est_",i,".dat")) ) )
names(dt)
dt <- dt %>% filter(year>2010,age>0)
unique(dt$year)



dt[,.(nlen=sum(nlen_samples), nwt=sum(nwt_samples),nage=sum(naged)),.(year,stratum,sex)]
dt[,.(nlen=sum(nlen_samples), nwt=sum(nwt_samples),nage=sum(naged)),.(year)]/1000
tally1 <- dt[,.(nlen=sum(nlen_samples), nwt=sum(nwt_samples),nage=sum(naged)),.(year,stratum)] %>% group_by(year)%>%summarise(sum(nlen),sum(nage),sum(nwt))
tally1 %>% print(n=Inf)

#--------------------------
# Compare Weights
#--------------------------
# read in last year's values
lywt <- as.data.frame(read.table("wtage2015.dat",header=FALSE))
mnwt <- as.data.frame(read.table("wtage2016.dat",header=FALSE))
lywt <- as.data.frame(read.table("wtage2016.dat",header=FALSE))
mnwt <- as.data.frame(read.table("wtage2017.dat",header=FALSE))
lywt <- as.data.frame(read.table("wtage2017.dat",header=FALSE))
mnwt <- as.data.frame(read.table("wtage2018.dat",header=FALSE))
lywt <- as.data.frame(read.table("wtage2018.dat",header=FALSE))
mnwt <- as.data.frame(read.table("wtage2019.dat",header=FALSE))
lywt <- as.data.frame(read.table("wtage2019.dat",header=FALSE))
mnwt <- as.data.frame(read.table("wtage2020.dat",header=FALSE))
lywt <- as.data.frame(read.table("wtage2020.dat",header=FALSE))
mnwt <- as.data.frame(read.table("wtage2020.dat",header=FALSE))

write.table(as.data.frame(mnwt)[,2:14],file="ttt.dat")
yr=2016
names(lywt) <- 3:15;lywt$yr <- yr; lywt$ayr="2015"
mnwt <- res
mnwt$ayr="2016"
#names(mnwt) <- read.table("hdr.dat",as.is=TRUE,header=FALSE);mnwt$yr <- yr; mnwt$ayr="2016"
dtmp <- data.table(rbind(mnwt,lywt))
dtmp
res <- data.table(melt(dtmp, measure.vars = 2:14, variable.name = "age", value.name = "wt"))
res 
res[as.numeric(age)==4&yr==2014]
str(res)
res[as.numeric(age)<10] %>% ggplot(aes(x=yr,y=wt,colour=as.factor(ayr))) + geom_line() + facet_grid(age~.,scales="free_y") + 
     ylab("Average weight (kg)") +  expand_limits(y=0) +mytheme+ labs(x="Year",colour="Assessment \n year") + scale_y_continuous(breaks=c(0.5,1.0,1.5)) #scale_colour_discrete(guide=FALSE) 

yr = 1991:2018
#--------------------------
# Fishery wts
#--------------------------
i=1991
wadt <- data.table(read.table(paste0("results/wtage",i,".rep"),header=TRUE) )
#wadt <- data.table(read.table(paste0("mainresults/wtage",i,".rep"),header=TRUE) )
names(wadt) <- c("bs","sam","yr",1:15)
for (i in 1992:2021) {
  wadt <- rbind(wadt,data.table(read.table(paste0("results/wtage",i,".rep"),header=TRUE)) ,use.names=FALSE)
}
summary(wadt)
# wide to long
wadt <- data.table(wadt)
wadt
wadt.m <- melt(wadt, measure.vars = 4:18, variable.name = "age", value.name = "wt")
wadt.m$age <-as.numeric(wadt.m$age )
dtmp <- wadt.m[,.(bs,yr,wt,mnwt=mean(wt)),.(age)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp <- dtmp[,.(yr,bs,age,wt,mnwt,swt=wt/mnwt)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp
library(ggthemes) 
#theme(panel.grid.major.y = element_line(colour="grey",linetype="dashed",size=0.5) ) theme(panel.grid.major.x = element_line(colour="grey",linetype="dashed",size=0.5) )  + theme(panel.border = element_rect(colour="grey",size=0.5) ) ,panel.grid.major.x = element_blank() )
mytheme <- mytheme + theme(panel.grid.major.y = element_line(colour="grey",linetype="dotted",size=1) ) + 
theme(panel.grid.major.x = element_blank()) 
##########
# unstratified results wt-age
##########
i = 2020
i = 2000
for (i in c(1996,2001,2006,2011,2016 ,2021)) {
  p <- dtmp[yr %between% c(i-4,i) &age>2&age<11] %>% ggplot(aes(x=as.factor(age),y=swt,fill=as.factor(yr-age))) +
       geom_hline(yintercept=1,color="brown",size=1) + scale_fill_discrete(guide=FALSE) +
       geom_violin() + facet_grid(yr~.) + xlab("Age") + ylab("Body weight relative to mean") + 
       mytheme + scale_y_continuous(limits=c(0.7,1.3),breaks=c(0.8,1.0,1.2)) #+ scale_fill_brewer(palette="Pastel2")    # scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
  print(p)
  #p <- p + coord_flip()
 #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
  ggsave(paste0("~/_mymods/ebswp/doc/figs/fsh_wtage_comb_",i,".pdf"),p,width=5,height=8)
}
ggsave("~/OneDrive/ebswp/EBSpollock/doc/figs/fsh_wtage_comb.pdf",p,width=5,height=8)

#swadt.m %>% filter(wt>0) %>% group_by(age) %>%summarise(mean(wt))
#swadt.m %>% filter(wt>0,age<16) %>% pivot_wider(names_from=str,values_from=wt)#group_by(age,str) %>% mutate()
#----------------------------------------------------------------------------------

names(wadt.m)
wadt.m
# Long to wide
res <- dcast(wadt.m[,.(mean(wt)),.(yr,age),],yr~age,value.var="V1")
res <- dcast(wadt.m[age>2,.(mean(wt)),.(yr,age),],yr~age,value.var="V1")
res
write.table(res, file="mnwt.dat")
res <- dcast(wadt.m[,.(median(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="mdnwt.dat")
res <- dcast(wadt.m[,.(sd(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="sdwt.dat")
res <- dcast(wadt.m[,.(median(wt)),.(yr,age),],yr~age,value.var="V1")
wadt.m[,.(mean(wt)),.(yr,age),]
dcast(wadt.m, yr ~ age, wt=value.var = "wt")
#---------------------
# Survey weights
#---------------------
i=1982
i=1982
wadt <- data.table(read.table(paste0("results/wtagesrv_",i,".rep"),header=TRUE) )
names(wadt) <- c("bs","sam","yr",1:15)
for (i in 1983:2015) 
{
  wadt <- rbind(wadt,data.table(read.table(paste0("results/wtagesrv_",i,".rep"),header=TRUE)) ,use.names=FALSE)
}
# wide to long
wadt.m <- melt(wadt, measure.vars = 4:18, variable.name = "age", value.name = "wt")
substr(wadt.m$yr,5,8) 
str(wadt.m)
wadt.m$age <-as.numeric(wadt.m$age )
wadt.m$yr <-as.numeric(substr(wadt.m$yr,5,8) )
i=2015
for (i in c(1985,1990,1995,2000,2005,2010 ,2015))
{
  p <- wadt.m[yr %between% c(i-4,i)&age>2&age<11] %>% ggplot(aes(x=as.factor(age),y=wt,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_grid(yr~.) + xlab("Age") + geom_point(data=rwadt.m,aes(x=as.factor(age),y=wt)) + mytheme 
  p <- p + scale_fill_discrete(guide=FALSE) + scale_fill_identity() + scale_y_continuous(limits=c(0,1.6),breaks=c(0.5,1.0,1.5))
 #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
  print(p)
}
names(wadt.m)
wadt.m
# Long to wide
res <- dcast(wadt.m[,.(mean(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="mnwt.dat")
res <- dcast(wadt.m[,.(median(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="mdnwt.dat")
res <- dcast(wadt.m[,.(sd(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="sdwt.dat")
res <- dcast(wadt.m[,.(median(wt)),.(yr,age),],yr~age,value.var="V1")
wadt.m[,.(mean(wt)),.(yr,age),]
dcast(wadt.m, yr ~ age, wt=value.var = "wt")
#-------------------------------------------------
# Stratified weights
#-------------------------------------------------
i=1991
#swadt <- data.table(read.table(paste0("mainresults/str_wtage",i,".rep"),header=TRUE) )
swadt <- (read.table(paste0("results/str_wtage",i,".rep"),header=TRUE) )
swadt <- NULL
for (i in 1991:2021) {
  swadt <- (read.table(paste0("results/str_wtage",i,".rep"),header=TRUE) )
  #swadt <- rbind(swadt,(read_table(paste0("results/str_wtage",i,".rep")))) 
}
names(swadt) <- c("bs","sam","yr","str",1:15)
dim(swadt)

# wide to long
# Set strata labels
(swadt$str[swadt$str==1]="A-season")
(swadt$str[swadt$str==2]="B-season, NW")
(swadt$str[swadt$str==3]="B-season, SE")
(swadt$str[swadt$str==999]="Combined")
glimpse(swadt)
swadt.m <- data.table::melt(swadt, measure.vars = 5:19, variable.name = "age", value.name = "wt")
swadt.m$age <-as.numeric(swadt.m$age )
swadt.m
mytheme <- mytheme + theme(panel.grid.major.y = element_line(colour="grey",linetype="dashed"))
#dt1 <- swadt.m[str=="Combined",.(bs,str,yr,wt,mnwt=mean(wt)),.(age)] 
#dt2 <- swadt.m[str!="Combined",.(mnwt=mean(wt)),.(age)]
library(data.table)
dt1 <- swadt.m %>% filter(str=="Combined") %>% group_by(age) %>% 
   summarise(mnwt=mean(wt)) 
dt1 <- data.table(dt1)
#dt1 <- swadt.m[str==999,.(mnwt=mean(wt)),.(age)] 
dt2 <- data.table(swadt.m) #[,.(age,bs,str,yr,wt)] 
dt2
setkey(dt2,age)
setkey(dt1,age)
dtmp <- merge(dt1,dt2)
dtmp
#dt2 <- swadt.m[.(bs,str,yr,wt,mnwt=mean(wt)),.(age)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp <- dtmp[,.(yr,str,bs,age,wt,mnwt,swt=wt/mnwt)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp
# Plot over all years just by strata
  dtmp %>% filter(age>2,age<11,str!="Combined") %>% 
  ggplot(aes(x=as.factor(age),y=swt,fill=str)) 
  p <- dtmp[age>2&age<11&str!="Combined"] %>% ggplot(aes(x=as.factor(age),y=swt,fill=str)) 
  p <- p + geom_violin() + facet_grid(~str) + xlab("Age") + mytheme + 
       geom_hline(yintercept=1,size=1.0,color="brown") + ylab("Body weight relative to mean") +
  scale_fill_discrete(guide=FALSE) + scale_y_continuous(limits=c(0.4,1.6),breaks=c(0.5,.75,1.0,1.25,1.5))
       p
  #ggsave("~/OneDrive/ebswp/EBSpollock/doc/figs/fsh_wtage.pdf",plot=p,width=8,height=6)
  ggsave("~/_mymods/ebswp/doc/figs/fsh_wtage.pdf",plot=p,width=8,height=6)

i=2021
summary(dtmp)
for (i in c(1996,2001,2006,2011 ,2016)) {
  p <- dtmp[yr %between% c(i-4,i)&age>2&age<11&str!=999&str!="Combined"] %>% ggplot(aes(x=as.factor(age),y=swt,fill=as.factor(yr-age))) 
  # with combined
  #p <- dtmp[yr %between% c(i-4,i)&age>2&age<11] %>% ggplot(aes(x=as.factor(age),y=swt,fill=as.factor(yr-age))) 
  #p <- swadt.m[yr %between% c(i-4,i)&age>2&age<11&str!=999] %>% ggplot(aes(x=as.factor(age),y=wt,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_grid(yr~str) + xlab("Age") + mytheme + ylab("Body weight relative to mean")
  p <- p + scale_fill_discrete(guide=FALSE) + scale_fill_identity() + scale_y_continuous(limits=c(0.6,1.6),breaks=c(0.7,1.0,1.3))
  # ylab("Average weight (kg)")
  #scale_y_continuous(limits=c(0,1.6),breaks=c(0.5,1.0,1.5))
  #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
  ggsave(paste0("~/_mymods/ebswp/doc/figs/rel_bodywt_",i,".pdf"),plot=p,width=8,height=6)
}
p
i=2018
  p <- dtmp[yr %between% c(i-5,i)&age>2&age<11&str=="Combined"] %>% ggplot(aes(x=as.factor(age),y=swt,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_wrap(~yr,nrow=6 ) + xlab("Age") + theme_bw(base_size=13) +
       ylab("Body weight relative to mean") + geom_hline(yintercept=1)
  p <- p + scale_fill_discrete(guide=FALSE) + scale_y_continuous(limits=c(0.6,1.4),breaks=c(0.75,1.0,1.25)) #+coord_flip()   
p
#--------------------------
# collapse over all years and show season/area effects
library(ggthemes)
swadt.m <- data.table(swadt.m)

dt1 <- swadt.m[str=="Combined",.(mnwt=mean(wt)),.(age)] 
dt1 <- swadt.m[str==999,.(mnwt=mean(wt)),.(age)] 
dt1
dt2 <- swadt.m[,.(age,bs,str,wt)] 
setkey(dt2,age)
setkey(dt1,age)
dtmp <- merge(dt1,dt2)
dt2 <- swadt.m[.(bs,str,yr,wt,mnwt=mean(wt)),.(age)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dt2
str(dtmp)
dtmp <- dtmp[,.(str,bs,age,wt,mnwt,swt=wt/mnwt)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp$str <- as.factor(dtmp$str)
  p <- dtmp[age>2&age<11&str!=999] %>% ggplot(aes(x=str,y=swt,fill=as.numeric(str))) 
  p <- p + geom_violin(width=.6) + facet_grid(age~.) + xlab("Strata") + mytheme + ylab("Body weight relative to mean")
  p <- p + scale_fill_discrete(guide=FALSE) + scale_y_continuous(limits=c(0.25,1.75),breaks=c(0.5,1.0,1.5))
  print(p)
  ggsave("~/OneDrive/ebswp/EBSpollock/doc/figs/fsh_wtage_comb.pdf",plot=p1,width=8,height=4,units="in")
  p <- dtmp[age>2&age<11&str!=999] %>% ggplot(aes(x=as.factor(age),y=swt,fill=(str))) + theme_base()
  p <- p + geom_violin(width=.9) + facet_grid(.~str) + xlab("Age") + ylab("Body weight relative to mean")
  p <- p + scale_fill_discrete(guide=FALSE) + scale_y_continuous(limits=c(0.25,1.75),breaks=c(0.5,1.0,1.5)) + geom_hline(yintercept=1,size=1.0)
  print(p)
  ggsave("~/OneDrive/ebswp/EBSpollock/doc/figs/fsh_wtage_comb.pdf",plot=p,width=8,height=4,units="in")
  # ylab("Average weight (kg)")
  #scale_y_continuous(limits=c(0,1.6),breaks=c(0.5,1.0,1.5))
  #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
}

#--------------------------
# Now process results in a nice way...

i=2016
adt <- data.table(read.table(paste0("data/age",i,".dat"),header=FALSE) )
names(adt) <- c("str","haul","sex","age","wt","len")
setkey(adt,age)
adt[(age>0&wt>0),.(N=.N,mean=mean(wt),sd=sd(wt)),age] %>% filter(N>6) %>% transmute(age,N,mean,cv=sd/mean) %>% arrange(age)
adt

i=1991
cadt <- data.table(read.table(paste0("results/catage",i,".rep"),header=TRUE) )
names(cadt) <- c("bs","sam","yr",1:15)
for (i in 1992:2016) 
{
  cadt <- rbind(cadt,data.table(read.table(paste0("results/catage",i,".rep"),header=TRUE)) ,use.names=FALSE)
}
cadt
# wide to long
cadt.m <- melt(cadt, measure.vars = 4:18, variable.name = "age", value.name = "catch")
cadt.m$age <-as.numeric(cadt.m$age )
cadt.m
t1 <- cadt.m[,.(catch,age,sc=sum(catch)),.(yr)][,.(yr,p=catch/sc,age),yr][,.(v=sum(age*age*p)-(sum(age*p)^2)),yr]
t2 <- cbind(t1,cadt.m[,.(sa=sum(age*catch),sc=sum(catch)),.(bs,yr)][,.(yr,avg_age=mean(sa/sc),v_age=var(sa/sc)),yr])
t2[,.(yr,avg_age,cv_avg_age=v_age^.5/avg_age,EffN=v/v_age)]
for (i in c(1996,2001,2006,2011,2016))
{
  p <- cadt.m[yr %between% c(i-4,i)&age>2&age<10] %>% ggplot(aes(x=as.factor(age),y=catch,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_grid(yr~.) + xlab("Age") + mytheme + ylim(0,1200000) + ylab("Catch in numbers")
  p <- p + scale_fill_discrete(guide=FALSE) + scale_fill_identity() + scale_y_continuous(breaks=NULL)
 #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
  print(p)
}


getwd()

# Stratified catch at age
i=1991
scadt <- data.table(read.table(paste0("mainresults/str_catage",i,".rep"),header=TRUE) )

names(scadt) <- c("bs","sam","yr","str",1:15)
for (i in 1992:2015) 
{
  scadt <- rbind(scadt,data.table(read.table(paste0("mainresults/str_catage",i,".rep"),header=TRUE)) ,use.names=FALSE)
}
# wide to long
# Set strata labels
(scadt$str[scadt$str==1]="A-season")
(scadt$str[scadt$str==2]="B-season, NW")
(scadt$str[scadt$str==3]="B-season, SE")
# (scadt$str[scadt$str==999]="Combined")
str(scadt)
scadt.m <- melt(scadt, measure.vars = 5:19, variable.name = "age", value.name = "wt")
scadt.m$age <-as.numeric(scadt.m$age )
scadt.m[bs==1,.(wt,N=.N,p=wt/sum(wt),catch=sum(wt)),.(yr,str)]
mytheme <- mytheme + theme(panel.grid.major.y = element_line(colour="grey",linetype="dashed"))
scadt.m
for (i in c(1995,2000,2005,2010 ,2015))
{
  p <- scadt.m[yr %between% c(i-4,i)&age>2&age<11&str!=999] %>% ggplot(aes(x=as.factor(age),y=wt,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_grid(yr~str) + xlab("Age") + mytheme + ylab("Catch in numbers")
  p <- p + scale_fill_discrete(guide=FALSE) + scale_fill_identity() + scale_y_continuous(limits=c(0,5e5),breaks=c(0.20e6,.4e6))
 #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
  print(p)
}


i=1991
wadt <- data.table(read.table(paste0("results/wtage",i,".rep"),header=TRUE) )
names(wadt) <- c("bs","sam","yr",1:15)
for (i in 1992:2015) 
{
  wadt <- rbind(wadt,data.table(read.table(paste0("results/wtage",i,".rep"),header=TRUE)) ,use.names=FALSE)
}
# wide to long
wadt.m <- melt(wadt, measure.vars = 4:18, variable.name = "age", value.name = "wt")
wadt.m$age <-as.numeric(wadt.m$age )
for (i in c(1995,2000,2005,2010 ,2015))
{
  p <- wadt.m[yr %between% c(i-4,i)&age>2&age<11] %>% ggplot(aes(x=as.factor(age),y=wt,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_grid(yr~.) + xlab("Age") + mytheme 
  p <- p + scale_fill_discrete(guide=FALSE) + scale_fill_identity() + scale_y_continuous(limits=c(0,1.6),breaks=c(0.5,1.0,1.5))
 #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
  print(p)
}

names(wadt.m)
wadt.m
# Long to wide
res <- dcast(wadt.m[,.(mean(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="mnwt.dat")
res <- dcast(wadt.m[,.(median(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="mdnwt.dat")
res <- dcast(wadt.m[,.(sd(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="sdwt.dat")
res <- dcast(wadt.m[,.(median(wt)),.(yr,age),],yr~age,value.var="V1")
wadt.m[,.(mean(wt)),.(yr,age),]
dcast(wadt.m, yr ~ age, wt=value.var = "wt")
# Stratified weights
i=1991
swadt <- data.table(read.table(paste0("results/str_wtage",i,".rep"),header=TRUE) )
names(swadt) <- c("bs","sam","yr","str",1:15)
for (i in 1992:2021) 
{
  swadt <- rbind(swadt,data.table(read.table(paste0("results/str_wtage",i,".rep"),header=TRUE)) ,use.names=FALSE)
}
# wide to long
# Set strata labels
(swadt$str[swadt$str==1]="A-season")
(swadt$str[swadt$str==2]="B-season, NW")
(swadt$str[swadt$str==3]="B-season, SE")
(swadt$str[swadt$str==999]="Combined")
str(swadt)
swadt.m <- melt(swadt, measure.vars = 5:19, variable.name = "age", value.name = "wt")
swadt.m$age <-as.numeric(swadt.m$age )
mytheme <- mytheme + theme(panel.grid.major.y = element_line(colour="grey",linetype="dashed"))
i=2021
summary(swadt.m)
cmnwt <- swadt.m %>% filter(wt>0,str=="Combined") %>% group_by(age) %>% summarise(mnwt=mean(wt))
cmnwt
for (i in c(1996,2001,2006,2011 ,2016,2021))
{
  p <- 
  swadt.m[yr %between% c(i-4,i)&age>2&age<11&str!="Combined"] %>% 
  filter(wt>0) %>% inner_join(cmnwt) %>% mutate(swt=wt/mnwt) %>% filter(swt<2,swt>.5) %>%
  ggplot(aes(x=as.factor(age),y=swt,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_grid(yr~str) + xlab("Age") + mytheme + ylab("Average weight (kg)")
  p <- p + scale_fill_discrete(guide=FALSE) + scale_fill_identity() + scale_y_continuous(limits=c(0,1.6),breaks=c(0.5,1.0,1.5))
  ggsave(paste0("~/_mymods/ebswp/doc/figs/fsh_wtage_str_",i,".pdf"),p,width=5,height=8)
 #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
  print(p)
}


edf <- read.table("results/t.dat",header=T)
edf %>% filter(type=select(year, age))
names(edf)



#----Catch age variance-----------------------
cdf <- read.table("catage_all_str.csv",header=T)
cdf <- read.table("catage_all.csv",header=F)
cdf <- data.frame(cdf)
names(cdf) <- c("yr","sim",1:15)
head(cdf)
# set Plus group 10+
cdf[,12] <- 
rowsum(cdf[,12:17])
?rowsum
tbl_df(cdf)
tdf <- gather(cdf,Age,catch,3:17) %>% filter(as.integer(Age)>2,as.integer(Age)<11) %>% transmute(yr,sim,Age,catch,cohort=as.factor(yr-as.integer(Age)))
tdf$yr <- factor(tdf$yr,levels=c(seq(from=1991,to=2011,by=5),seq(from=1992,to=2012,by=5),seq(from=1993,to=2013,by=5),seq(from=1994,to=2014,by=5),seq(from=1995,to=2015,by=5)))
tbl_df(tdf)
p <- ggplot(tdf,aes(x=Age,y=catch,fill=cohort)) #,group=Strata))
p <- p + geom_violin(aes(colour=cohort),width=1.1) + scale_colour_identity() + scale_fill_identity() #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
p <- p + facet_wrap(~yr) + mytheme
p

p <- p + coord_flip()
p <- p + geom_line(aes(x=as.integer(age),y=(value),col=case))
p <- p + geom_bar(aes(x=(age),y=(value),col=case))

getwd()
runs <- c("runs","runs05","runs10","runs50","runs99")
for (j in 4:4){
  system("rm Est*.dat")
  for (i in 10:23){
  	ccc <- paste("cut -f ",i, " data/",runs[j],".dat >sam.dat;./sam",sep="")
  	print(ccc)
	  system(ccc)
  }
  system(paste("cat Est*.dat > ",runs[j],"_est.rep",sep=""))
}

names(df0)
df0  <- cbind(case="base",read.table("runs_est.rep",header=T))
df05 <- cbind(case="5% to A",read.table("runs05_est.rep",header=T))
df10 <- cbind(case="10% to A",read.table("runs10_est.rep",header=T))
df50 <- cbind(case="50% to A",read.table("runs50_est.rep",header=T))
df99 <- cbind(case="99% to A",read.table("runs99_est.rep",header=T))
mdf  <- rbind(df0,df05,df10,df50,df99)
reshape(mdf,idvar=case,direction="wide")
?aggregate

# "year"    "type"    "stratum" "sex"     "age"     "value"  
df <- mdf %>% filter(type=="N") %>% filter(sex==99) %>% 
              filter(stratum==99) %>% filter(age<11)
p <- ggplot(df,aes(x=(age),y=value,col=case))
p <- p + geom_bar(aes(position="dodge",col=case))
p <- p + geom_line(aes(x=(age),y=(value),col=case))
p <- p + geom_bar(aes(x=(age),y=(value),col=case))

p <- p + facet_wrap(~year)
print(p)
summary(df )

#---------------------------------------
#---------------------------------------
# set up simulations
# read in real data
ldf <- data.table(read.table("data/lensim.dat",as.is=TRUE))
names(ldf)<-c("str","haul","sex","len","frq")
length(ldf$haul)
ldf$haul <-  as.integer(1+ldf$haul/130)
max(ldf$haul)
min(ldf$haul)

adf <- data.table(read.table("data/agesim.dat",as.is=TRUE))
names(adf)<-c("str","haul","sex","age","wt","len")
max(adf$haul)
adf$haul <-  as.integer(1+adf$haul/28)
adf$haul <-  as.integer(1+adf$haul/1.03)
setkey(adf,len)
# Sets up wt given length to means ("recycled")
t<- adf[,.(str,haul,sex,age,mwt=mean(wt),len),len]
adf[,.(mwt=mean(wt),len),len]
t<- t[,.(str,haul,sex,age,mwt,len)]
write.table(t,file="data/t.dat",row.names=FALSE, col.names=FALSE)
getwd()
?write.table
plot(adf[,(len,wt=mean(wt))])
#---------------------------------------

lfdt <- data.table(read.table(paste0("results/LF_",i,".rep"),header=TRUE) )
for (i in 1992:2015) 
{
  lfdt <- rbind(lfdt,data.table(read.table(paste0("results/LF_",i,".rep"),header=TRUE)) )
}
