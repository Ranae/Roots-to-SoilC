library(tidyverse)
tex<- read_csv("Data/Characterization Summary for COBS 2008 Fall Soil Samples.csv"
               ,skip = 2)

mean_tex<-tex%>%
  filter(`Trt #` != "NA")%>%
  filter(!(`Trt #` %in% c("S2", "C2", "CCW")))%>%
  select(Depth, `BULK DENSITY`, `pH`, `Bray P`, `Avail K`, SAND, 
         COSILT, FISILT, CLAY)%>%
  rename(depth = Depth, bd = `BULK DENSITY`, P =`Bray P`, K = `Avail K`, 
         sand = SAND, cosilt = COSILT, fisilt = FISILT, clay = CLAY)%>%
  mutate(bd = as.numeric(bd), pH = as.numeric(pH), P = as.numeric(P), 
         K = as.numeric(K))%>%
  group_by(depth)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))

cn<-read.table("Data/2008CNbd.txt", header = TRUE)

mean_cn<-cn%>%
  filter(!(trt %in% c("S2", "C2", "CCW")))%>%
  select(depth, BD, TN, TC)%>%
  group_by(depth)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))

  