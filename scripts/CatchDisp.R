t <- data.frame( x1 = runif(1000,1,2), x2 = runif(1000,3,4), x3 = runif(1000,5,6))
lm1  <-1. 
lcv1 <-.01 
dm1  <-1.
dcv1 <-.2 
t$l1 <- rnorm(1000,lm1,lcv1*lm1)
t$d1 <- rnorm(1000,dm1,dcv1*dm1)

lm2  <-1. 
lcv2 <-.01 
dm2  <-.2
dcv2 <-.1  
t$l2 <- rnorm(1000,lm2,lcv2*lm2)
t$d2 <- rnorm(1000,dm2,dcv2*dm2)

lm3  <-1. 
lcv3 <-.01 
dm3  <-1.6
dcv3 <-.05 
t$l3 <- rnorm(1000,lm3,lcv3*lm3)
t$d3 <- rnorm(1000,dm3,dcv3*dm3)
  
plot(t$x1,t$l1,ylim=c(0,3),pch=19,xlim=c(0,6),xlab="Fishery",ylab="Catch")
points(t$x1,t$l1+t$d1,pch=19,col="red")
points(t$x1,t$l1+2,pch=19,col="yellow")

points(t$x2,t$l2,pch=19)
points(t$x2,t$l2+t$d2,pch=19,col="red")
points(t$x2,t$l2+2,pch=19,col="yellow")

points(t$x3,t$l3,pch=19)
points(t$x3,t$l3+t$d3,pch=19,col="red")
points(t$x3,t$l3+2,pch=19,col="yellow")

library(ggplot2)
library(dplyr)
library(tidyr)
separate(t,)
ggplotky




library(DiagrammeR)
mermaid("
        gantt
        dateFormat  YYYY-MM-DD
        title An alternative Gantt diagram
        
        section Basic Tasks
        This is completed             :done,          first_1,    2014-01-06, 2014-01-08
        This is active                :active,        first_2,    2014-01-09, 3d
        Do this later                 :               first_3,    after first_2, 5d
        Do this after that            :               first_4,    after first_3, 5d
        
        section Important Things
        Completed, critical task      :crit, done,    import_1,   2014-01-06,24h
        Also done, also critical      :crit, done,    import_2,   after import_1, 2d
        Doing this important task now :crit, active,  import_3,   after import_2, 3d
        Next critical task            :crit,          import_4,   after import_3, 5d
        
        section The Extras
        First extras                  :active,        extras_1,   after import_4,  3d
        Second helping                :               extras_2,   after extras_1, 20h
        More of the extras            :               extras_3,   after extras_1, 48h
        ")


plot(density(rbeta(1000,1 ,8)))
8/1
,ylim=c(0,1))
points(rbeta(1000,8 ,2),pch=19)
summary(rbeta(1000,8 ,2))
sqrt(var(rbeta(1000,2 ,8)))
var(1:10)
,pch=19)
points(rbeta(1000,10 ,8))
