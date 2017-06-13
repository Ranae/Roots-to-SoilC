library(tidyr)
library(dplyr)
library(ggplot2)


roots<-read.table("../Data/rootplotmeans.txt", header = TRUE)
roots$plot<-as.factor(plot)
roots<-roots[roots$trt %in% c("P", "PF", "CC"),]  

ggplot(roots, a)

ggplot(roots, aes(x=year, y=mass, color=trt, group=trt))+
  geom_point()+
  facet_grid(depth ~ plot)

top<-roots%>%
  filter(depth < 60)%>%
  group_by(year, trt, depth, point)%>%
  summarize(mean = mean(mass))%>%
  group_by(year, trt)%>%
  summarize(total = sum(mean))%>%
  filter(year %in% c(2009, 2010, 2011))%>%
  print()

dif<-roots%>%
  filter(depth < 60)%>%
  filter(plot != 41)%>%
  select(year, trt, plot, mass)%>%
  group_by(year, trt, plot)%>%
  summarize(total = sum(mass))%>%
  spread(key = year, value = total)%>%
  mutate(dif2010 = `2010`-`2009`, dif2011 = `2011`-`2010`)%>%
  group_by(trt)%>%
  summarize_each(funs(mean(.)))
  
  

###k=input/pool
#input from 2015 Crop Science publication, pool from 0-30 cm mass calculated above
#assumes steady state, inputs = losses 
#2010P <- 367.0/748.4
#2010PF <- 146.3/230.6
#2011P <- 386.6/757.6
#2011PF <- 168.3/342.1 

###
P10 <- 367.0
PF10 <- 146.3
P11 <- 386.6
PF11 <- 168.3


turn<-c(367.0, 146.3, 386.6, 168.3, 55.8, 47.9)  #Inputs from Crop Science (w.max)
turn<-as.data.frame(turn)
colnames(turn)<-"input"
turn$k<-turn$input/turn$pool
turn$mrt<-1/turn$k

turn2<-turn%>%
  mutate(trt = ifelse((trt == "P10"), "Prairie2010",
                         ifelse((trt == "P11"), "Prairie2011",
                                ifelse((trt == "PF10"), "FertilizedPrairie2010",
                                       ifelse((trt == "PF11"), "FertilizedPrairie2011",
                                              ifelse((trt == "C10"), "Maize2010", "Maize2011"))))))%>%
  select(trt, input, gain, loss, pool, k, mrt)
  

knitr::kable(turn2, digits = 2, caption = "Root pool decomposition, masses in g/m^2")


  #input  trt   gain   loss  pool         k      mrt
#1 367.0  P10 104.04 262.96 748.4 0.3513629 2.846060
#2 146.3 PF10  62.00  84.30 230.6 0.3655681 2.735469
#3 386.6  P11  77.75 308.85 757.6 0.4076690 2.452971
#4 168.3 PF11  55.12 113.18 342.1 0.3308389 3.022619
#5  55.8  C10  17.89  37.91  44.1 0.8596372 1.163281
#6  47.9  C11  16.43  31.47  47.2 0.6667373 1.499841

#Calculation of differences in measured values
try<-spread(top, key=year, value = total)
colnames(try)<-c("trt", "a", "b", "c")
try$firstyear<-try$b-try$a
try$secondyear<-try$c-try$b
  