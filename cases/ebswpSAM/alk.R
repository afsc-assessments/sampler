library(ggplot2)
library(dplyr)
library(tidyr)
df <- read.table("all_alk.rep",header=T)
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank() )
mytheme <- mytheme + theme(text=element_text(size=14)) + theme(axis.title.x=element_text(size=24) ,axis.title.y=element_text(size=24))
mytheme <- mytheme + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_line(colour="grey60", linetype="dashed"), panel.grid.major.y = element_blank() )
mytheme <- mytheme + theme( panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black", fill=NA, size=1))
names(df)
tit <- "Male"
tdf <- gather(df,age,proportion,5:19) %>% filter(Year>=1992,Strata>1,Strata<99,Sex==1) %>%
tit <- "Female"
tdf <- gather(df,age,proportion,5:19) %>% filter(Year>=1992,Strata>1,Strata<99,Sex==2) %>%
mutate(age=as.numeric(age)) %>% group_by(Year,Strata,Length) %>% summarise(ma = sum(age*proportion))
tbl_df(tdf)
ggplot(tdf,aes(x=Length,y=ma,colour=as.factor(Strata)))  + xlim(20,65) + geom_line() + facet_wrap( ~ Year) +
  labs(y="Mean age (years)",col="strata") + mytheme + ggtitle(tit) +
  scale_colour_discrete(labels=c("W of 170W", "E of 170W" ))

tdf <- gather(df,age,proportion,5:19) %>% filter(Year>=1992,Strata>1,Strata<99,Sex==2) %>%
tit <- "Male"
tdf <- gather(df,age,proportion,5:19) %>% filter(Year>=1992,Strata>1,Strata<99,Sex==1) %>%
mutate(age=as.numeric(age)) %>% group_by(Strata,Length) %>% summarise(ma = sum(age*proportion))
ggplot(tdf,aes(x=Length,y=ma,colour=as.factor(Strata)))  + xlim(20,65) + geom_line() + 
  labs(y="Mean age (years)",col="strata") + mytheme + ggtitle(tit) +
  scale_colour_discrete(labels=c("W of 170W", "E of 170W" ))
  
tbl_df(tdf)
as.numeric(tdf$age)

bs <- read.table("catagebs.dat",header=T)
names(bs)
tbs <- gather(bs,age,catch,3:17)
glimpse(tbs)
tbl_df(tbs)
neff <- 3000
tt <- filter(tbs,yr>=1999) %>% mutate(age=as.numeric(age)) %>%
  group_by(yr,sim) %>% mutate(ff=sum(catch),proportion=catch/ff) %>% group_by(yr,age) %>%
  summarise(p=mean(proportion), cv=sqrt(var(proportion))/p, cv1=sqrt(p*(1-p)/neff)/p)

# First compute the variance of mean age-------------
tt <- filter(tbs) %>% mutate(age=as.numeric(age)) %>% group_by(yr,sim) %>% 
  mutate(ff=sum(catch),proportion=catch/ff) %>% group_by(yr,age) %>%
  summarise(p=mean(proportion), cv=sqrt(var(proportion))/p)      #%>%
  group_by(yr) %>% summarise(mobs=sum(p*age),var=sum(age*age*p)-mobs*mobs,sd=sqrt(var))
t2 <- group_by(tt,yr) %>% summarise(mobs=sum(p*age),var=sum(age*age*p)-mobs*mobs,sd=sqrt(var))
  dim(t2)
  dim(tt)
  mobs <- pobs %*% av
  stmp <- sqrt(elem_prod(av,av)*pobs - mobs*mobs));

# Check if proportions look right...
ggplot(tt, aes(x=age,y=p)) +  geom_line() + facet_wrap( ~ yr) + mytheme #+ ggtitle(paste("Sample size ="f))
# Look at mean age over years
group_by(tt,yr) %>% mutate(mean_age=sum(age*p)) %>% ggplot(aes(x=yr,y=mean_age)) + geom_line() + mytheme

#Variance in age over bootstrap samples 
tt <- filter(tbs) %>% mutate(age=as.numeric(age)) %>% group_by(yr,sim) %>% 
   mutate(ff=sum(catch), p=catch/ff) %>% summarise(ma=sum(age*p)) %>% 
   group_by(yr) %>% summarise(varA=var(ma))
t2 <- group_by(tt,yr) %>% summarise(mobs=sum(p*age),var=sum(age*age*p)-mobs*mobs,sd=sqrt(var))
# Effective sample size from Francis
t2$var/tt$varA
cbind(tt,t2)




df <- read.table("LF_2014.rep",header=T)
df <- as.data.frame(df)
tbl_df(df)
t2 <- df %>% mutate(length=as.numeric(length)) %>% group_by(year,Sim) %>% 
  mutate(ff=sum(freq),proportion=freq/ff) %>% group_by(year,length) %>%
  summarise(p=mean(proportion), cv=sqrt(var(proportion))/p)      %>%
  group_by(year) %>% summarise(mobs=sum(p*length),var=sum(length*length*p)-mobs*mobs,sd=sqrt(var))

#Variance in length over bootstrap samples 
tt <- df %>% mutate(length=as.numeric(length)) %>% group_by(year,Sim) %>% 
   mutate(ff=sum(freq), p=freq/ff) %>% summarise(ma=sum(length*p)) %>% 
   group_by(year) %>% summarise(varA=var(ma))
df %>% select(year,Sim,length,freq) %>% mutate(length=as.numeric(length)) %>% group_by(year,Sim) %>% 
  mutate(ff=sum(freq),p=freq/ff) %>% summarise(sum(p))  #%>% #group_by(year,length) %>%
  #summarise(p=mean(proportion), cv=sqrt(var(proportion))/p)      %>%
ggplot( aes(x=length,y=p),col=as.factor(Sim)) +  geom_line() + mytheme

# Effective sample size from Francis
t2$var/tt$varA
cbind(tt,t2)














  summarise(mobs=sum(p*age),var=sum(age*age*p)-mobs*mobs,sd=sqrt(var))
tt

tt <- filter(tbs) %>% mutate(age=as.numeric(age)) %>% group_by(yr,sim) %>% 
        mutate(ff=sum(catch),proportion=catch/ff)%>% summarise(ma=sum(age*proportion)) %>% 
        group_by(yr) %>% summarise(varA=var(ma))

group_by(tt,yr) %>% mutate(mean_age=sum(age*p)) %>% ggplot(aes(x=yr,y=mean_age)) + geom_line() + mytheme


  variance of proportion at age (sd_age) / var(average_age)

  av   <- 1:15
  av*av
ggplot(tt, aes(x=log(p),y=log(cv))) +  geom_smooth(aes(x=log(p),y=log(cv1))) + geom_point() + facet_wrap( ~ yr) +
  mytheme + ggtitle(paste("Sample size =",neff))

tbl_df(tt)
glimpse(tt)

  summarise(freq=sum(freq),proportion=sum(prop))
aglf <- glf %>% select(adfg,len,freq,nobs) %>% group_by(adfg) %>% mutate(ff=sum(freq), cs = cumsum(freq)/ff ) %>% arrange(adfg,len)
  group_by(adfg,len) %>% mutate(proportion = sum(freq)/ff) %>% arrange(adfg,len) %>% mutate(cp = cumsum(proportion)) 