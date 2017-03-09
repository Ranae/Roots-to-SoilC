library(tidyverse)

chchanges<-read.csv("Data/follett_C_changes.csv", header=TRUE)
colnames(chchanges)<-c("location","soil", "management", "0-100", "0-10", "10-20", "20-30", "30-60", "60-100")

time<-chchanges%>%
  select(-management, -`0-100`)%>%
  gather(`0-10`:`60-100`, key=depth, value=kgCha)%>%
  mutate(depth = factor(depth),
         depth = factor(depth, levels = rev(levels(depth))))%>%
  ggplot(aes(x=depth, y=kgCha))+
  geom_point()+
  coord_flip()+
  #scale_y_reverse()+
  geom_abline(intercept=0)