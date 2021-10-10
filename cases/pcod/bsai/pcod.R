R
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
q()
n
setwd("~/Onedrive/ebswp/data/sampler/cases/pcod/bsai")
source("../../../sampleR/R/sampler.R")
ad <- read.csv(paste0("imported/pcod_age.csv"),as.is=T,header=F)
cd <- read.csv(paste0("imported/akfincat.csv"),as.is=T,header=F)
names(cd) <- hdr_cat
cd <- data.table(cd)
cd
setkey(cd,Year)
names(cd)
scd <- cd[AGE>0,.N,.(YEAR,AGE)]
scd <- cd[FMP.Subarea=="BS",sum(3*wt_recorded),.(Year)]
scd

names(ad) <- hdr_age
names(ad)
ad <- data.table(ad)
setkey(ad,YEAR)
sad <- ad[YEAR>2007&FMP_SUBAREA=="BS"&WEIGHT>0,.(wt=mean(WEIGHT)),.(LNGTH,YEAR)]
sad
ggplot(sad,aes(x=LNGTH,y=wt,colour=as.factor(YEAR))) + geom_line() + ylim(c(0,7))+ xlim(c(20,80))
+ facet_grid(YEAR ~ .) + ylim(c(0,10))+ xlim(c(20,100))
ls()
ggplot(sad,aes(x=YEAR,y=N,col=AGE)) + geom_point() + ylim(c(0,200))
ggplot(sad,aes(x=YEAR,y=N)) + geom_point() #+ ylim(c(0,200))
i=2011
Sampler(i,maxlen=120)
Sampler(i,do_all=FALSE,maxlen=120)
for (i in 2008:2011) Sampler(yr=i,maxlen=120)
for (i in 2008:2011) Sampler(i,do_all=FALSE,maxlen=120)
for (i in 1991:2015) Sampler(i,do_all=FALSE,maxlen=120)
edf <- read.table("results/t.dat",header=T)
edf %>% filter(type=select(year, age))
names(edf)
tbl_df(cd)

cd <- read.csv(paste("imported/akfincat.csv",sep=""),as.is=T,header=F)
  names(cd) <- hdr_cat
cdf <- cd %>% filter(FMP.Area=="BSAI",Species.Group.Name=="Pollock") %>%
  transmute(
    area=Reporting.Area.Code,month=trunc(WED/100),
    strata=ifelse(month<6,1,ifelse(area>519,2,3)), 
    catch=wt_recorded
  )  %>% select(strata, catch) %>%
  group_by(strata) %>% summarise(catch=sum(catch))
tbl_df(cdf)
cdf$catch/sum(cdf$catch )
#----Catch age variance-----------------------
cdf <- read.table("catage_all_str.csv",header=T)
cdf <- read.table("catage_all.csv",header=F)
cdf <- data.frame(cdf)
names(cdf) <- c("yr","sim",1:15)
# set Plus group 10+
cdf[,12] <- 
rowsum(cdf[,12:17])
?rowsum
tbl_df(cdf)
tdf <- gather(cdf,Age,catch,3:17) %>% filter(as.integer(Age)>2,as.integer(Age)<11) %>% transmute(yr,sim,Age,catch,cohort=as.factor(yr-as.integer(Age)))
tdf$yr <- factor(tdf$yr,levels=c(seq(from=1991,to=2011,by=5),seq(from=1992,to=2012,by=5),seq(from=1993,to=2013,by=5),seq(from=1994,to=2014,by=5),seq(from=1995,to=2015,by=5)))
tbl_df(tdf)
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank())# element_line(colour="grey60", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=18)) + theme(axis.title.x=element_text(size=22) ,axis.title.y=element_text(size=22))
mytheme <- mytheme + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )
mytheme <- mytheme + theme( panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black", fill=NA, size=1))
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
