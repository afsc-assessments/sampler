
#--------------------------
# Survey pull and convert
#--------------------------
# Filled in individual body weights using wt-lengths
# MALEB 3.038 FEMALEB 2.986 TOTALB  2.9954  
lwb <- c(3.038, 2.986,  2.9954)
lwa <- c(0.000004919, 0.000006681,  6.3611E-06)

srvlen <- data.table(read.csv("~/OneDrive/ebswp/data/Survey/bts/Length_Survey_pollock.csv"))

srvlen$yr <- substr(srvlen$CRUISE,1,4)
srvage$a <- lwa[srvage$SEX]
srvage$b <- lwb[srvage$SEX]
names(srvlen)
names(srvage)
i=1993
for (i in 1982:2015) 
{
  aout <- paste0("srv_age_",i,".rep")  
  lout <- paste0("srv_len_",i,".rep")  
  afile <- paste0("data/srv_age_",i,".dat")  
  lfile <- paste0("data/srv_len_",i,".dat")  
  cfile <- paste0("data/srv_sam_",i,".dat")  
  #adt   <- srvage[yr==i,.(str=1,haul=as.numeric(as.factor(HAULJOIN)),sex=SEX,age=AGE,wt:=ifelse(WEIGHT>0,WEIGHT/1000,(lwa[SEX]*LENGTH^lwb[SEX])/1000),len=LENGTH/10)] 
  # adt   <- srvage[yr==i,.(str=1,haul=as.numeric(as.factor(HAULJOIN)),sex=SEX,age=AGE,wt=wt,len=LENGTH/10)] 
  adt   <- srvage[yr==i,.(str=1,haul=as.numeric(as.factor(HAULJOIN)),sex=SEX,age=AGE,wt = ifelse(is.na(WEIGHT),lwa[SEX]*LENGTH^lwb[SEX]/1000,WEIGHT/1000), len=LENGTH/10  )]
  ldt   <- srvlen[yr==i,.(1,as.numeric(as.factor(HAULJOIN)),SEX,LENGTH/10,FREQUENCY)] 
  write.table(file=afile,adt ,row.names=FALSE,col.names=FALSE)
  write.table(file=lfile,ldt ,row.names=FALSE,col.names=FALSE)
  lad <- dim(adt)[1]
  lld <- dim(ldt)[1]
  cat(sep= " \n",
        paste0("srv_",i) , afile, lfile, lad , lld ,
        1 , 15 , 20 , 80 , 1,
        1000000,
        paste("results/Est_srv_",i,".dat",sep=""),
        file = cfile
    )
}
wadt[,lapply(.SD,mean),yr]
for (i in 1982:2015) 
{
  cfile <- paste0("data/srv_sam_",i,".dat")  
  system(paste0("./sam -ind  ",cfile))
}

#--------------------------
# Now process survey results in a nice way...
#--------------------------
# Read in RACE estimates
rwadt <- data.table(read.table(paste0("results/race_wts.rep"),header=TRUE) )
names(rwadt) <- c("yr",1:15)
rwadt.m <- melt(rwadt, measure.vars = 2:16, variable.name = "age", value.name = "wt")
setkey(wadt.m,(yr,age))
setkey(rwadt.m,(yr,age))
rwadt.m
str(rwadt.m)
  p <- rwadt.m[age>2&age<11] %>% ggplot(aes(x=as.factor(age),y=wt,fill=as.factor(yr-age))) 
  p <- p + geom_line() + xlab("Age") +  mytheme 
  p <- p + scale_fill_discrete(guide=FALSE) + scale_y_continuous(limits=c(0,1.6),breaks=c(0.5,1.0,1.5))
  p
# write files
# Long to wide
res <- dcast(wadt.m[,.(mean(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="mnsrvwt.dat")
res <- dcast(wadt.m[,.(median(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="mdnsrvwt.dat")
res <- dcast(wadt.m[,.(sd(wt)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="sdsrvwt.dat")
res
rwadt.m

# Survey N at age 
i=1982
scadt <- data.table(read.table(paste0("results/catagesrv_",i,".rep")) )
names(scadt) <- c("bs","sam","yr",1:15)
for (i in 1983:2015) 
{
  scadt <- rbind(scadt,data.table(read.table(paste0("results/catagesrv_",i,".rep"))) ,use.names=FALSE)
}
# wide to long
scadt.m <- melt(scadt, measure.vars = 4:18, variable.name = "age", value.name = "N")
substr(scadt.m$yr,5,8) 
scadt.m$age <-as.numeric(scadt.m$age )
scadt.m$yr <-as.numeric(substr(scadt.m$yr,5,8) )
scadt.m <- scadt.m[,.(age,N,p=N/sum(N)),.(bs,yr)]
scadt.m[age %between% c(5,6)&yr==2015] %>% ggplot(aes(x=N,y=N,colour=as.factor(age))) + geom_point()
dtmp <- as.data.frame(dcast(scadt.m[yr==2015&age %between% c(3,9),.(age,p=N/sum(N)),bs],bs~age,value.var="p"))

 ggplot(dtmp,aes(x="5",y="6")) +geom_point()
 pairs(dtmp[,2:8])
 ?pairs

 pairs(dtmp[2:8],pch=19,cex=.2, lower.panel = panel.smooth, upper.panel = panel.cor)
 max(dtmp[2:8])
 pairs(dtmp[2:8], panel=panel.smooth, cex=0.5, pch=19, bg = "light blue", diag.panel = panel.hist, cex.labels = 2, font.labels = 2,xlim=c(0,.5), ylim=c(0,.5))

names(dtmp)
[,.("3","4","5","6","7","8","9")]# %>% ggpairs()
bb(scadt.m)

res <- dcast(scadt.m[,.(mean(N)),.(yr,age),],yr~age,value.var="V1")
write.table(res, file="mncatage.dat")
