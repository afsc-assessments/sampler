radian
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tidyverse)
library(data.table)
library(xtable)
#mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank())# element_line(colour="grey60", linetype="dashed"))
#mytheme <- mytheme + theme(text=element_text(size=18)) + theme(axis.title.x=element_text(size=22) ,axis.title.y=element_text(size=22))
#mytheme <- mytheme + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )
#mytheme <- mytheme + theme( panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black", fill=NA, size=.5))
mytheme <- theme_few()
#q()
#n
#setwd("~/Onedrive/ebswp/data/sampler/cases/ebswp")
setwd("~/Onedrive/sampler/cases/ebswpSAM")
# source sampler.R
source("../../sampleR/R/sampler.R")

#####Catch compilation#############################################################
# Old format for catch
#cd <- (read.csv(paste("imported/akfincat.csv",sep=""),as.is=T,header=F))
#hdr_cat <- read.csv("imported/hdr_cat_short.csv",as.is=T,header=F)
# New format for catch from saved akfin run
cd        <- (read.csv(paste("imported/akfin_cat.csv",sep=""),as.is=T,header=T))
head(cd)
hdr_cat   <- read.csv("imported/hdr_cat.csv",as.is=T,header=F)
names(cd) <- hdr_cat[c(1,20,21,22,15,16,17,23,19,27,4 )]
#hdr_cat   <- read.csv("imported/hdr_cat_short.csv",as.is=T,header=F)
#names(cd) <- hdr_cat
hdr_cat
cd <-tibble(cd)
# Write all the catch files from R instead of awk...
#   subset data.table to be columns used by sampler
#dd <- data.table(cd[,c(1,20,21,22,15,16,17,23,19,27,4 )])
str(cd)

dd <- data.table(cd)
names(cd)
yr<-2020
for (yr in 2017:2017){
for (yr in 1991:2020){
  write.csv( dd[Year==yr,,], file=paste0("imported/catch",yr,".csv"),row.names=FALSE)
}
#----------------------------------------------------
#Datatable summary of catch by year
dd[FMP_Subarea=="BS",sum(Catch),.(Year)]

names(cd)
#cdf <- cd %>% filter(Year==2019,FMP_Area=="BSAI",FMP_Subarea=="BS",Species_Group=="Pollock") %>% transmute(
cdf <- cd %>% filter(Year==2020,FMP_Area=="BSAI",FMP_Subarea=="BS",Species_Group=="Pollock") %>% transmute(
    area=NMFS_Area,month=as.integer(WED/100), strata=ifelse(month<6,1,ifelse(area>519,2,3)), catch=Catch )  %>% 
    select(strata, catch) %>% group_by(strata) %>% summarise(catch=sum(catch))
tibble(cdf)
cdf$catch/sum(cdf$catch )
#####END of Catch compilation#############################################################

#--------------------------
# Fishery data pull and write files for sampler
#--------------------------
SetBS(n=1000)
SetBS(n=1)
i=2020
Sampler(yr=i)
i=2019
i=2018
for (i in 2019:2018) Sampler(yr=i)
i=2017
i=2015
Sampler(yr=i)
i=2016
Sampler(yr=i,io=T)
for (i in 2016:1991) Sampler(yr=i)
for (i in 2016:1991) Sampler(yr=i)
#--------------------------

#--------------------------
# Read in wt-age bootstraps
#--------------------------
  # Stratified
library(ggthemes)
wsdt <- (read.table("results/wtagestr.dat",header=F))
names(wsdt) <- c("bs","hdr","yr","strata",1:15)
head(wsdt)
wsdt <- wsdt %>% mutate(strata=ifelse(strata==1,"A-season",ifelse(strata==2,"B-season-NW",ifelse(strata==3,"B-season-SE","Annual"))))
wsdtl <- wsdt %>% pivot_longer(cols=5:19,names_to="age",values_to="wt") %>% mutate(age=as.numeric(age))
glimpse(wsdtl)
summary(wsdtl)
wsdtl %>% group_by(strata,age) %>% summarise(mwt=mean(wt)) %>% ggplot(aes(x=age,y=mwt,col=strata)) + geom_line() + theme_few() + geom_point()
wsdtl %>% group_by(strata,age) %>% ggplot(aes(x=as.factor(age),y=wt,fill=strata)) + geom_boxplot() + theme_few() 
wsdtl %>% filter(yr==2019,age>2,age<10) %>% group_by(strata,Age=as.factor(age)) %>% ggplot(aes(x=Age,y=wt,fill=strata)) + geom_boxplot() + theme_few() 

  # Aggregated
i=1991
wdt <- (read.table(paste0("results/wtage",i,".rep"),header=F))
names(wdt) <- c("bs","str","yr",1:15)
wdt <- wdt[-1001,]
for (i in 2020:1992){
  wdd <- (read.table(paste0("results/wtage",i,".rep"),header=F))
  names(wdd) <- c("bs","str","yr",1:15)
  wdd <- wdd[-1001,]
  wdt <- rbind(wdt,wdd)
} 
wdt %>% filter(yr==2017)
wd <- data.table(melt(wdt, id.vars = c("bs","yr"), measure.vars = 4:18, variable.name = "age", value.name = "wt"))
str(wd)
wdt
write.csv(wdt,file="wdt.csv")
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
i=2020
edt <- (read.table(paste0("results/Est_",i,".dat"),header=T))
for (i in 2019:1991){
  tdt <- (read.table(paste0("results/Est_",i,".dat"),header=T))
  edt <- rbind(edt,tdt)
}
edt %>% filter(type=="N",sex<3,stratum<4) %>% mutate(season=ifelse(stratum==1,"A","B"),sex=ifelse(sex==1,"Male",ifelse(sex==2,"Female","Total"))) %>% group_by(year,sex,season) %>% summarize(Catch=sum(value)) %>% 
        ggplot(aes(x=year,y=Catch,color=season,shape=sex,linetype=season)) + mytheme + geom_point(size=3) + geom_line(size=1.2) + expand_limits(y=0) + 
        scale_x_continuous(breaks=seq(1990,2020,by=2)) + xlab("Year")
#--------------------------
# Read in catch-age bootstraps
#--------------------------
i=2020
cdt <- (read.table(paste0("results/catage",i,".rep"),header=F))
names(cdt) <- c("bs","str","yr",1:15)
cdt <- cdt[-1001,]
cdt
for (i in 2019:1991){
  cdd <- (read.table(paste0("results/catage",i,".rep"),header=F))
  names(cdd) <- c("bs","str","yr",1:15)
  cdd <- cdd[-1001,]
  cdt <- rbind(cdt,cdd)
} 
summary(cdt)
write.csv(cdt,file="cdt.csv")
cd <- data.table(melt(cdt, id.vars = c("bs","yr"), measure.vars = 4:18, variable.name = "age", value.name = "Catch"))
cd
#cd[FMP_Subarea=="BS",sum(Catch),.(Year)]
cds <- cd[as.numeric(age)>1 & yr>2010 & as.numeric(age)<11,.(avg=mean(Catch),cv=sd(Catch)/mean(Catch)),.(yr,age)] 
cds$Year <- as.factor(cds$yr)
cds
ggplot(cds,aes(x=age,y=avg,shape=Year)) + mytheme + geom_point()
names(cd )
cds <- cd[,.(avg=mean(Catch),cv=sd(Catch)/mean(Catch)),.(yr,age)] 
cds$Year <- as.factor(cds$yr)
cds.m    <- dcast(cds,Year~age,value.var="avg")
cds.m
write.csv(cds.m,"catage.csv")
cds.m    <- dcast(cds,Year~age,value.var="cv")
cds %>% filter(Year==2020) %>% dcast(,Year~age,value.var="cv")
?dcast
dcast(cd,yr~age,value.var="Catch")
write.csv(cds.m,"catage_cv.csv")

tt <- cd %>% pivot_wider(names_from = age, values_from = Catch)
head(tt)
cd
write.csv(tt,"catage.csv")
write.csv(cd,"catage.csv")
tmp <- cd %>% filter(yr==2018) %>% group_by(bs)  %>% mutate(p=Catch/sum(Catch))
tmp %>% group_by(age) %>% summarize(mean(p))

 #fish_encounters
     #fish_encounters %>%
       #pivot_wider(names_from = station, values_from = seen)
     # Fill in missing values
     #fish_encounters %>% #pivot_wider( #names_from = station, #values_from = seen,

cd %>% filter(yr==2018,age>2) %>% group_by(age) %>% mutate(m)

dvar_vector rtmp = elem_div((pobs-phat),sqrt(elem_prod(phat,(1-phat))));
  double vtmp;
  vtmp = value(norm2(rtmp)/size_count(rtmp));
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
str(cds.tab)
print(cds.tab, caption.placement = "top", include.rownames = FALSE)



adt <- data.table(Get_Age(1991)); adt$yr <- 1991
for (i in 2017:2018) {
for (i in 1992:2019) {
  tmp <- data.table(Get_Age(i)); tmp$yr <- i
  adt <- rbind(adt,tmp)
}
adt
adt <- data.table(Get_Age(2019)); #adt$yr <- 1991
adt %>% filter(age>0) %>% group_by(strata,sex) %>% summarise(n()) 
ldt <- data.table(Get_LF(2019)); ldt$yr <- 2019
ldt %>% group_by(strata,sex) %>% summarise(sum(freq)) 
names(ldt)
for (i in 2018:2020) {
for (i in 1992:2020) {
  tmp <- data.table(Get_LF(i)); tmp$yr <- i
  ldt <- rbind(ldt,tmp)
}
setkey(adt,yr)
setkey(ldt,yr)
names(adt)
names(ldt)

# table of length data by yr/strata/sex
ldt %>% group_by(yr,strata,sex) %>% summarize(n=sum(freq)) %>% pivot_wider(names_from=2:3,values_from=n) %>% xtable()
# table of LW data by yr/strata/sex
adt %>% filter(len>0,wt>0) %>% group_by(yr,strata,sex) %>% tally() %>% pivot_wider(names_from=2:3,values_from=n) %>% xtable()
# table of age data by yr/strata/sex
adt %>% filter(age>0,yr>2010) %>% group_by(yr) %>% tally() 
#%>% pivot_wider(names_from=2:3,values_from=n) %>% xtable()
adt %>% filter(age>0,yr>1999) %>% group_by(yr,strata,sex) %>% tally() %>% pivot_wider(names_from=2:3,values_from=4) %>% xtable()

adt[age>0,.(nhauls=max(haul),.N),yr]
adt[,.(max(haul),.N),yr]
ldt[order(yr),.(hauls=max(haul),freq=sum(freq)),yr]
ldt[yr<=2017,.(max(haul),sum(freq)),yr]
ldt[yr<=2018,.(max(haul),sum(freq,na.rm=TRUE)),yr]
aldt <- cbind( adt[age>0,.(N_haul_ages=max(haul,na.rm=TRUE ),N_ages=.N),yr], ldt[yr<2018,.(N_haul_lengths=max(haul),N_lengths=sum(freq)/20),yr])
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
SetBS(n=1)    #No bootstraps
SetBS(n=1000) #1000 bootstraps
read.table("bs_setup.dat")
i=2017
i=2019
for (i in 1991:2017) 
  Sampler(yr=i,do_all=FALSE)
  args(Sampler)
#--------------------------
# For survey look see file ebs_survey.R

#--------------------------
# Look at tally of lengths etc
i=2015
dt <- data.table(read.table(paste0("results/LF_",i,".rep"),header=TRUE) )
i=2017
  dt <- rbind(dt,    data.table(read.table(paste0("results/LF_",i,".rep"),header=TRUE) ) )
for (i in 2017:2017) 
{
  dt <- rbind(dt,    data.table(read.table(paste0("results/LF_",i,".rep"),header=TRUE) ) )
}
#Sample sizes by year
dt
dt[,.(nlen=sum(nlen_samples), nwt=sum(nwt_samples),nage=sum(naged)),.(year,stratum,sex)]
dt[,.(nlen=sum(nlen_samples), nwt=sum(nwt_samples),nage=sum(naged)),.(year)]/1000
tally1 <- dt[,.(nlen=sum(nlen_samples), nwt=sum(nwt_samples),nage=sum(naged)),.(year,stratum)] %>% group_by(year)%>%summarise(sum(nlen),sum(nage),sum(nwt))
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
for (i in 1992:2018) {
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
i = 2018
for (i in c(1998,2003,2008,2013 ,2018)) {
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
swadt <- data.table(read.table(paste0("results/str_wtage",i,".rep"),header=TRUE) )
names(swadt) <- c("bs","sam","yr","str",1:15)
for (i in 1992:2018) 
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
swadt.m
mytheme <- mytheme + theme(panel.grid.major.y = element_line(colour="grey",linetype="dashed"))
#dt1 <- swadt.m[str=="Combined",.(bs,str,yr,wt,mnwt=mean(wt)),.(age)] 
#dt2 <- swadt.m[str!="Combined",.(mnwt=mean(wt)),.(age)]
dt1 <- swadt.m[str=="Combined",.(mnwt=mean(wt)),.(age)] 
#dt1 <- swadt.m[str==999,.(mnwt=mean(wt)),.(age)] 
dt1
dt2 <- swadt.m[,.(age,bs,str,yr,wt)] 
setkey(dt2,age)
setkey(dt1,age)
dtmp <- merge(dt1,dt2)
dt2 <- swadt.m[.(bs,str,yr,wt,mnwt=mean(wt)),.(age)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp <- dtmp[,.(yr,str,bs,age,wt,mnwt,swt=wt/mnwt)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp
# Plot over all years just by strata
  p <- dtmp[age>2&age<11&str!="Combined"] %>% ggplot(aes(x=as.factor(age),y=swt,fill=str)) 
  p <- p + geom_violin() + facet_grid(~str) + xlab("Age") + mytheme + 
       geom_hline(yintercept=1,size=1.0,color="brown") + ylab("Body weight relative to mean") +
  scale_fill_discrete(guide=FALSE) + scale_y_continuous(limits=c(0.4,1.6),breaks=c(0.5,.75,1.0,1.25,1.5))
       p
  #ggsave("~/OneDrive/ebswp/EBSpollock/doc/figs/fsh_wtage.pdf",plot=p,width=8,height=6)
  ggsave("~/_mymods/ebswp/doc/figs/fsh_wtage.pdf",plot=p,width=8,height=6)

i=2018
summary(dtmp)
for (i in c(1998,2003,2008,2013 ,2018)) {
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
i=2018
  p <- dtmp[yr %between% c(i-5,i)&age>2&age<11&str=="Combined"] %>% ggplot(aes(x=as.factor(age),y=swt,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_wrap(~yr,nrow=6 ) + xlab("Age") + theme_bw(base_size=13) +
       ylab("Body weight relative to mean") + geom_hline(yintercept=1)
  p <- p + scale_fill_discrete(guide=FALSE) + scale_y_continuous(limits=c(0.6,1.4),breaks=c(0.75,1.0,1.25)) #+coord_flip()   
p
#--------------------------
# collapse over all years and show season/area effects
library(ggthemes)
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
for (i in 1992:2015) 
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
for (i in c(1995,2000,2005,2010 ,2015))
{
  p <- swadt.m[yr %between% c(i-4,i)&age>2&age<11&str!=999] %>% ggplot(aes(x=as.factor(age),y=wt,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_grid(yr~str) + xlab("Age") + mytheme + ylab("Average weight (kg)")
  p <- p + scale_fill_discrete(guide=FALSE) + scale_fill_identity() + scale_y_continuous(limits=c(0,1.6),breaks=c(0.5,1.0,1.5))
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