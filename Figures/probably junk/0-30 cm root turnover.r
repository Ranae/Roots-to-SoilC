library(tidyr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/rdietzel/Dropbox/Root manuscript/Figures and analysis")

roots<-read.table("rootplotmeans.txt", header = TRUE)

top<-roots%>%
  filter(depth < 60)%>%
  group_by(year, trt, depth, point)%>%
  summarize(mean = mean(mass))%>%
  group_by(year, trt)%>%
  summarize(total = sum(mean))%>%
  filter(year %in% c(2010, 2011))%>%
  print()

###k=input/pool
#input from 2015 Crop Science publication, pool from 0-30 cm mass calculated above
#assumes steady state, inputs = losses 
2010P <- 367.0/748.4
2010PF <- 146.3/230.6
2011P <- 386.6/757.6
2011PF <- 168.3/342.1 

###
P10 <- 367.0
PF10 <- 146.3
P11 <- 386.6
PF11 <- 168.3


turn<-c(367.0, 146.3, 386.6, 168.3, 55.8, 47.9)
turn<-as.data.frame(turn)
colnames(turn)<-"input"
turn$trt<-c("P10" ,"PF10", "P11", "PF11", "C10", "C11")
turn$gain<-c(104.04, 62.0, 77.75, 55.12, 17.89, 16.43)
turn$loss<-turn$input-turn$gain
turn$pool<-c(748.4, 230.6, 757.6, 342.1, 44.1, 47.2)
turn$k<-turn$loss/turn$pool
turn$mrt<-1/turn$k

  #input  trt   gain   loss  pool         k      mrt
#1 367.0  P10 104.04 262.96 748.4 0.3513629 2.846060
#2 146.3 PF10  62.00  84.30 230.6 0.3655681 2.735469
#3 386.6  P11  77.75 308.85 757.6 0.4076690 2.452971
#4 168.3 PF11  55.12 113.18 342.1 0.3308389 3.022619
#5  55.8  C10  17.89  37.91  44.1 0.8596372 1.163281
#6  47.9  C11  16.43  31.47  47.2 0.6667373 1.499841