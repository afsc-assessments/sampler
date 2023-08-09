# This is to cross check stratum-specific data 
dtmp <- ad %>% 
  transmute( area = NMFS_AREA,
    len = LENGTH, wt = WEIGHT, age = AGE, sex = ifelse(SEX=="F",1,2)
  )
tbl_df(dtmp)
read.csv("results/wtall.csv")
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=18)) + theme(axis.title.x=element_text(size=22) ,axis.title.y=element_text(size=22))
mytheme <- mytheme + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
#mytheme <- mytheme + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_line(colour="grey60",linetype="dashed"), 
                           panel.grid.major.y = element_line(colour="grey60", linetype="dashed") )
mytheme <- mytheme + theme(panel.background = element_rect(fill="white"),panel.border = element_rect(size=2))
mytheme <- mytheme + theme( panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black", fill=NA, size=1))

library(tidyr)
wd <- read.table("results/wtage2013.rep",as.is=T,header=F)
wd <- read.table("results/wtall.csv",as.is=T,header=F)
tbl_df(wd)
names(wd) <- c("sim","x","yr","str",1:15)
names(wd)
tbl_df(wd)
wdf <- wd %>% dplyr::select(yr,str,5:19) %>%  gather(age,wt, -yr, -str) %>% filter(as.numeric(age)>2, as.numeric(age)<10)
tbl_df(wdf)
p <- ggplot(wdf,aes(x=as.factor(age),y=wt)) + 
  geom_boxplot(aes(fill=as.factor(str)),outlier.size=.2,notch=T,notch.width=.5 ) + 
  mytheme + scale_y_continuous(limits=c(0,1.5)) + xlab("Age") + ylab("Body mass of pollock (kg)") + facet_wrap(~yr) + 
  theme(legend.position=c(.75,.04)) + labs(fill="Area-season") + scale_fill_discrete(labels=c("All areas A-season", "NW B-season", "SE B-season","Combined"))
print(p)

p <- ggplot(wdf,aes(x=as.factor(age),y=wt)) + 
  #geom_boxplot(aes(fill=as.factor(str)),outlier.size=.2,notch=T,notch.width=.5 ) + mytheme +
  geom_violin(aes(fill=as.factor(str)),outlier.size=.2 ) + mytheme +
scale_y_continuous(limits=c(0,1.5)) + xlab("Age") + ylab("Body mass of pollock (kg)") + 
  theme(legend.position=c(.85,.14)) + labs(fill="Area-season") + scale_fill_discrete(labels=c("All areas A-season", "NW B-season", "SE B-season","Combined"))

print(p)
