R
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank())# element_line(colour="grey60", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=18)) + theme(axis.title.x=element_text(size=22) ,axis.title.y=element_text(size=22))
mytheme <- mytheme + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )
mytheme <- mytheme + theme( panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black", fill=NA, size=1))
setwd("~/Onedrive/ebswp/data/sampler/cases/atka")
# source sampler.R
source("../../sampleR/R/sampler.R")
#--------------------------
# Fishery data pull and write files for sampler
#--------------------------
SetBS(n=1000) #1000 bootstraps
read.table("bs_setup.dat")
for (i in 1991:2015) Sampler(yr=i,est=FALSE)
adt <- data.table(Get_Age(1991)); adt$yr <- 1991
for (i in 1992:2016) {
  tmp <- data.table(Get_Age(i)); tmp$yr <- i
  adt <- rbind(adt,tmp)
}
adt
ldt <- data.table(Get_LF(1991)); ldt$yr <- 1991
for (i in 1992:2015) {
  tmp <- data.table(Get_LF(i)); tmp$yr <- i
  ldt <- rbind(ldt,tmp)
}
setkey(adt,yr)
setkey(ldt,yr)
adt[age>0,.(nhauls=max(haul),.N),yr]
adt[,.(max(haul),.N),yr]
ldt[order(yr),.(max(haul),sum(freq)),yr]
ldt[yr<2016,.(max(haul),sum(freq)),yr]
ldt[yr<2016,.(max(haul),sum(freq,na.rm=TRUE)),yr]
aldt <- cbind( adt[age>0,.(N_haul_ages=max(haul,na.rm=TRUE ),N_ages=.N),yr], ldt[yr<2016,.(N_haul_lengths=max(haul),N_lengths=sum(freq/10)),yr])
aldt
al <- data.table(melt(aldt, measure.vars = c(2,3,5,6), variable.name = "measure", value.name = "N"))
is.factor(al$measure)
ggplot(al,aes(x=yr,y=N,colour=measure,shape=measure)) +  geom_point() + geom_line() + mytheme + xlab("Year") + ggtitle("Fishery samples")+
scale_colour_discrete(labels=c("Number of age-hauls ", "Number of ages", "Number of length-hauls","Number of lengths / 10")) +
scale_shape_discrete(labels=c("Number of age-hauls ", "Number of ages", "Number of length-hauls","Number of lengths / 10")) + guides()guide_legend(title=NULL)
#--------------------------
# For survey look see file ebs_survey.R

#--------------------------
# Look at tally of lengths etc
i=1991
dt <- data.table(read.table(paste0("results/LF_",i,".rep"),header=TRUE) )
for (i in 1992:2015) 
{
  dt <- rbind(dt,    data.table(read.table(paste0("results/LF_",i,".rep"),header=TRUE) ) )
}
#Sample sizes by year
dt[,.(nlen=sum(nlen_samples), nwt=sum(nwt_samples),nage=sum(naged)),.(year,stratum)]
q#--------------------------
# Compare Weights
#--------------------------
# read in last year's values
lywt <- as.data.frame(read.table("wtage2015.dat",header=FALSE))
mnwt <- as.data.frame(read.table("wtage2016.dat",header=FALSE))

write.table(as.data.frame(mnwt)[,2:14],file="ttt.dat")
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

yr = 1991:2014
#--------------------------
# Fishery wts
#--------------------------
i=1991
wadt <- data.table(read.table(paste0("mainresults/wtage",i,".rep"),header=TRUE) )
names(wadt) <- c("bs","sam","yr",1:15)
for (i in 1992:2015) 
{
  wadt <- rbind(wadt,data.table(read.table(paste0("mainresults/wtage",i,".rep"),header=TRUE)) ,use.names=FALSE)
}
# wide to long
wadt.m <- melt(wadt, measure.vars = 4:18, variable.name = "age", value.name = "wt")
wadt.m$age <-as.numeric(wadt.m$age )
dtmp <- wadt.m[,.(bs,yr,wt,mnwt=mean(wt)),.(age)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp <- dtmp[,.(yr,bs,age,wt,mnwt,swt=wt/mnwt)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp
for (i in c(1995,2000,2005,2010 ,2015))
{
  p <- dtmp[yr %between% c(i-4,i)&age>2&age<11] %>% ggplot(aes(x=as.factor(age),y=swt,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_grid(yr~.) + xlab("Age") + ylab("Body weight relative to mean") + 
       mytheme + theme(panel.grid.major.y = element_line(colour="grey",linetype="dashed",size=0.5) )
  p <- p + scale_fill_discrete(guide=FALSE) + scale_fill_identity() + scale_y_continuous(limits=c(0.7,1.3),breaks=c(0.75,1.0,1.25))
 #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
  print(p)
}
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
swadt <- data.table(read.table(paste0("mainresults/str_wtage",i,".rep"),header=TRUE) )
names(swadt) <- c("bs","sam","yr","str",1:15)
for (i in 1992:2015) 
{
  swadt <- rbind(swadt,data.table(read.table(paste0("mainresults/str_wtage",i,".rep"),header=TRUE)) ,use.names=FALSE)
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
#dt1 <- swadt.m[str=="Combined",.(bs,str,yr,wt,mnwt=mean(wt)),.(age)] 
#dt2 <- swadt.m[str!="Combined",.(mnwt=mean(wt)),.(age)]
dt1 <- swadt.m[str=="Combined",.(mnwt=mean(wt)),.(age)] 
dt2 <- swadt.m[,.(age,bs,str,yr,wt)] 
setkey(dt2,age)
setkey(dt1,age)
dtmp <- merge(dt1,dt2)
dt2 <- swadt.m[.(bs,str,yr,wt,mnwt=mean(wt)),.(age)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp <- dtmp[,.(yr,str,bs,age,wt,mnwt,swt=wt/mnwt)] #%>% mutate(bs,wt,mnwt,swt=wt/mnwt)
dtmp
i=2010
for (i in c(1995,2000,2005,2010 ,2015))
{
  p <- dtmp[yr %between% c(i-4,i)&age>2&age<11&str!=999] %>% ggplot(aes(x=as.factor(age),y=swt,fill=as.factor(yr-age))) 
  #p <- swadt.m[yr %between% c(i-4,i)&age>2&age<11&str!=999] %>% ggplot(aes(x=as.factor(age),y=wt,fill=as.factor(yr-age))) 
  p <- p + geom_violin() + facet_grid(yr~str) + xlab("Age") + mytheme + 
       ylab("Body weight relative to mean")
  p <- p + scale_fill_discrete(guide=FALSE) + scale_fill_identity() + 
  scale_y_continuous(limits=c(0.6,1.6),breaks=c(0.7,1.0,1.3))
  print(p)
  # ylab("Average weight (kg)")
  #scale_y_continuous(limits=c(0,1.6),breaks=c(0.5,1.0,1.5))
  #scale_fill_brewer(palette="Pastel2") # + scale_colour_manual(palette-"Pastel2") #scale_fill_discrete()
}


#--------------------------
# Now process results in a nice way...

i=2015
adt <- data.table(read.table(paste0("data/age",i,".dat"),header=FALSE) )
names(adt) <- c("str","haul","sex","age","wt","len")
setkey(adt,age)
adt[(age>0&wt>0),.(N=.N,mean=mean(wt),sd=sd(wt)),age] %>% filter(N>6) %>% transmute(age,N,mean,cv=sd/mean)

i=1991
cadt <- data.table(read.table(paste0("mainresults/catage",i,".rep"),header=TRUE) )
names(cadt) <- c("bs","sam","yr",1:15)
for (i in 1992:2015) 
{
  cadt <- rbind(cadt,data.table(read.table(paste0("mainresults/catage",i,".rep"),header=TRUE)) ,use.names=FALSE)
}
cadt
# wide to long
cadt.m <- melt(cadt, measure.vars = 4:18, variable.name = "age", value.name = "catch")
cadt.m$age <-as.numeric(cadt.m$age )
cadt.m
t1 <- cadt.m[,.(catch,age,sc=sum(catch)),.(yr)][,.(yr,p=catch/sc,age),yr][,.(v=sum(age*age*p)-(sum(age*p)^2)),yr]
t2 <- cbind(t1,cadt.m[,.(sa=sum(age*catch),sc=sum(catch)),.(bs,yr)][,.(yr,avg_age=mean(sa/sc),v_age=var(sa/sc)),yr])
t2[,.(yr,avg_age,cv_avg_age=v_age^.5/avg_age,EffN=v/v_age)]
bfor (i in c(1995,2000,2005,2010 ,2015))
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
tbl_df(cd)

cd <- (read.csv(paste("imported/akfincat.csv",sep=""),as.is=T,header=F))
hdr_cat <- read.csv("imported/hdr_cat.csv",as.is=T,header=F)
names(cd) <- hdr_cat
cd <- data.table(cd)
cd[FMP.Subarea=="BS",sum(wt_recorded),.(Year)]

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