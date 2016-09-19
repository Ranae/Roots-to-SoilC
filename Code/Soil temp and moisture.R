cc<-read.csv("Data/cc soil climate.csv", header=TRUE)
cc$trt<-"cc"
pf<-read.csv("Data/pf soil climate.csv", header=TRUE)
pf$trt<-"pf"
p<-read.csv("Data/p soil climate.csv", header=TRUE)
p$trt<-"p"

all<-rbind(cc, pf, p)


allmeans<-all%>%
  select(-(ec1:ec5))%>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))%>%
  group_by(date, trt)%>%
  summarise_each(funs(mean, "mean", mean(., na.rm = TRUE)))%>%
  gather(key = variable, value = mean, sw1_mean:st5_mean)%>%
  select(-block_mean)%>%
  mutate(type = c("water", "temp") [grepl("sw", variable)+2*grepl("st", variable)])%>%
  mutate(depth = c("5", "10", "17.5", "35", "50") 
         [grepl("1", variable)+2*grepl(2, variable)+3*grepl(3, variable)+4*grepl(4, variable)+5*grepl(5, variable)])%>%
  mutate(month=month(date))

allmeans<-allmeans[!(allmeans$type == "water" & allmeans$month %in% c("1", "2", "3", "12")),]

  allmeans<-group_by(allmeans, trt, type, depth)%>%
  summarise_each(funs(mean, "mean", mean(., na.rm = TRUE)))


allmeans$depth<-as.numeric(allmeans$depth)

ggplot() +
  geom_line(aes(x=depth, y=mean_mean, group=trt, colour=trt), size=2, 
             data=filter(allmeans, type=="temp"))+
  coord_flip()+
  scale_x_reverse()+
  ggtitle("Average yearly soil temperature over the soil profile")

ggplot() +
  geom_line(aes(x=depth, y=mean_mean, group=trt, colour=trt), size=2, 
            data=filter(allmeans, type=="water"))+
  coord_flip()+
  scale_x_reverse()+
  ggtitle("Average soil moisture of the soil profile")
  
##Check out the raw sensor data
compblocks<-all%>%
  select(-(ec1:ec5))%>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))%>%
  group_by(date, trt, block)%>%
  gather(key = variable, value = value, sw1:st5)%>%
  mutate(type = c("water", "temp") [grepl("sw", variable)+2*grepl("st", variable)])%>%
  mutate(depth = c("5", "10", "17.5", "35", "50") 
         [grepl("1", variable)+2*grepl(2, variable)+3*grepl(3, variable)+4*grepl(4, variable)+5*grepl(5, variable)])
  

ggplot()+
  geom_point(aes(x=date, y=value, color=trt), alpha = .35,
             data=filter(compblocks, type=="water"))+
  facet_grid(block~depth)
  

