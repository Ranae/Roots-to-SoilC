#This code is a little broken, but works up to allC
library(dplyr)
library(tidyr)
library(reshape2)

cn<-read.table("Data/Root CN content.txt", header = TRUE)
toc<-read.table("Data/toc.txt", header = TRUE)
roots<-read.table("Data/rootplotmeans.txt", header = TRUE)
bd12<-read.table("Data/2012bdplotmeans.txt", header=TRUE)
bd08<-read.table("Data/2008bdplotmeans.txt", header=TRUE)
pox<-read.table("Data/pox plot means.txt", header=TRUE)
 
roots<-roots[roots$year == "2013",]
roots<-roots[,c(-2, -6)]


bd08<-bd08[,c(1:5)]
colnames(bd08)<-c("year", "plot", "trt", "depth", "bd")

bd<-bd12[,-1]



cn$trt<-as.character(cn$trt)

cn$trt[cn$plot == "11"]<-"SC"
cn$trt[cn$plot == "33"]<-"SC"
cn$trt[cn$plot == '26']<-"SC"
cn$trt[cn$plot == '44']<-"SC"
cn$trt[cn$plot == '16']<-"CS"
cn$trt[cn$plot == '34']<-"CS"
cn$trt[cn$plot == '22']<-"CS"
cn$trt[cn$plot == '45']<-"CS"

cn<-cn[cn$year == "2013",]
#cn<-cn%>%
  #group_by(year, trt, depth)%>%
  #summarise_each(funs(mean))%>%
  #select(-plot)


toc$trt<-as.character(toc$trt)

toc$trt[toc$plot == "11"]<-"SC"
toc$trt[toc$plot == "33"]<-"SC"
toc$trt[toc$plot == '26']<-"SC"
toc$trt[toc$plot == '44']<-"SC"
toc$trt[toc$plot == '16']<-"CS"
toc$trt[toc$plot == '34']<-"CS"
toc$trt[toc$plot == '22']<-"CS"
toc$trt[toc$plot == '45']<-"CS"

roots$trt<-as.character(roots$trt)

roots$trt[roots$plot == "11"]<-"SC"
roots$trt[roots$plot == "33"]<-"SC"
roots$trt[roots$plot == '26']<-"SC"
roots$trt[roots$plot == '44']<-"SC"
roots$trt[roots$plot == '16']<-"CS"
roots$trt[roots$plot == '34']<-"CS"
roots$trt[roots$plot == '22']<-"CS"
roots$trt[roots$plot == '45']<-"CS"

bd$trt<-as.character(bd$trt)

bd$trt[bd$plot == "11"]<-"SC"
bd$trt[bd$plot == "33"]<-"SC"
bd$trt[bd$plot == '26']<-"SC"
bd$trt[bd$plot == '44']<-"SC"
bd$trt[bd$plot == '16']<-"CS"
bd$trt[bd$plot == '34']<-"CS"
bd$trt[bd$plot == '22']<-"CS"
bd$trt[bd$plot == '45']<-"CS"

wtoc<-merge(toc, bd, by=c("trt", "plot", "depth"))

wtoc$layer[wtoc$depth == 5] <- 5
wtoc$layer[wtoc$depth == 15] <- 10
wtoc$layer[wtoc$depth == 30] <- 15
wtoc$layer[wtoc$depth == 60] <- 30
wtoc$layer[wtoc$depth == 100] <- 40

wtoc$gCcm2<-(wtoc$bd * (wtoc$oc *.01))*wtoc$layer
wtoc$MgCha<-wtoc$gCcm2 * 100

pox$trt<-as.character(pox$trt)

pox$trt[pox$plot == "11"]<-"SC"
pox$trt[pox$plot == "33"]<-"SC"
pox$trt[pox$plot == '26']<-"SC"
pox$trt[pox$plot == '44']<-"SC"
pox$trt[pox$plot == '16']<-"CS"
pox$trt[pox$plot == '34']<-"CS"
pox$trt[pox$plot == '22']<-"CS"
pox$trt[pox$plot == '45']<-"CS"

pox$depth[pox$point %in% 2.5]<-5
pox$depth[pox$point %in% 10]<-15
pox$depth[pox$point %in% 22.5]<-30
pox$depth[pox$point %in% 45]<-60
pox$depth[pox$point %in% 80]<-100

pox<-pox[,c(-1, -4)]


croots<-merge(roots, cn, by=c("year", "trt", "plot", "depth"))

croots$rootC<-croots$mass * (croots$perC * .01)

allC<-merge(wtoc, croots, by=c("year", "trt", "plot", "depth"))

allC$rootsandsoil<-allC$MgCha + allC$rootC

allC<-allC[,c(-1)]

allC<-merge(allC, pox, by=c("trt", "plot", "depth"))

allC$poxMgha<-(allC$bd*allC$layer*.001)*allC$poxconc*.1

lessC<-allC[,c(1,2,3,8, 9, 13, 17)]

split<-lessC %>% 
  filter(trt %in% c("CC", "P", "PF"))%>%
  #group_by(trt, plot, depth) %>% 
  #bind_rows(., .) %>%
  mutate(root = ifelse((depth == 5), mass,
         ifelse((depth == 15), mass/2,
         ifelse((depth == 30), mass/3,
         ifelse((depth == 60), mass/6,
         ifelse((depth == 100), mass/8, mass))))))%>%
  mutate(carbon = ifelse((depth == 5), MgCha,
                      ifelse((depth == 15), MgCha/2,
                             ifelse((depth == 30), MgCha/3,
                                    ifelse((depth == 60), MgCha/6,
                                           ifelse((depth == 100), MgCha/8, MgCha))))))%>%
  mutate(depth = ifelse((depth == 5), 2.5,
                         ifelse((depth == 15), 10,
                                ifelse((depth == 30), 22.5,
                                       ifelse((depth == 60), 45,
                                              ifelse((depth == 100), 80, NA))))))%>%
  select(trt, plot, depth, root, carbon)%>%
arrange(trt, plot, depth)


ggplot(split, aes(y=carbon, x=-depth))+
  geom_bar(stat="identity")+
  #geom_line()+
  coord_flip()+
  facet_wrap(~trt)
  

ggplot(df, aes(x=year, y=coverage, group=functional_group, color=functional_group))+
  geom_line()+
  facet_wrap(~year)

sumC<-aggregate(cbind(MgCha, mass, rootC, poxMgha) ~ trt + plot, data=allC, sum)
colnames(sumC)<-c("trt", "plot", "sumMgCha", "summass", "sumrootC", "sumpoxMgha")

propC<-merge(allC, sumC, by = c("trt", "plot"), all=TRUE)

propC$soilCprop<-propC$MgCha/propC$sumMgCha
propC$rootmassprop<-propC$mass/propC$summass
propC$rootCprop<-propC$rootC/propC$sumrootC
propC$poxCprop<-propC$poxMgha/propC$sumpoxMgha
propC<-merge(propC, croots, by = c("trt", "plot", "depth"), all=TRUE)

propC<-propC[,c(1:25)]
colnames(propC)<-c("trt","plot","depth","oc","bd","layer","gCcm2","MgCha","mass","perC","perN","CN", "rootC","rootsandsoil",
                   "poxc","poxconc","poxMgha","sumMgCha", "summass","sumrootC", "sumpoxMgha","soilCprop", "rootmassprop",
   "rootCprop", "poxCprop") 
   
   
propfig<-aggregate(cbind(soilCprop, rootCprop)~trt+depth, data=propC, mean)
propfig<-propfig[propfig$trt %in% c("P", "PF", "CC"),]
propfig <- within(propfig, depth <- ordered(depth, levels = rev(sort(unique(depth)))))

write.csv(propfig, file="Data/Roots and Soil C.csv", row.names=FALSE)
#propfig<-read.csv("Roots and Soil C.csv", header=TRUE)

meltprop<-melt(propfig, id = c("trt", "depth"), measured = c("soilCprop", "rootCprop"))
meltprop$weighted<-meltprop$value
###meltprop$weighted[meltprop$depth == "5"]<-(meltprop$value/2)

 
dodge <- position_dodge(width=.9)
g<-ggplot(meltprop, aes(x=factor(depth), y = value, group=variable)) + 
    geom_line(aes(colour=variable), size=1.5)+#(stat = "identity", position = "dodge") +
	#geom_errorbar(aes(ymax = poxconc + poxconcse, ymin=poxconc - poxconcse), position = dodge, width=0.25) +
	coord_flip()+
	facet_wrap(~trt)+
	scale_fill_discrete(breaks=c("CC", "CCW", "CS", "P", "PF", "SC"), 
	  labels = c("Continuous Corn", "Continuous Corn with Rye", "Corn-soybean", "Prairie", "Fertilized Prairie", "Soybean-corn"))+
	guides(col = guide_legend(reverse = TRUE))+
		labs(y = "Proportion",x = "Average depth (cm)")+
	    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(),
          legend.position=c(.85,.25), legend.title=element_blank(),
		  legend.text = element_text(size=12),
		  axis.title.x = element_text(size=14,vjust=-0.5),
          axis.title.y = element_text(size=14,angle=90),
          axis.text.x = element_text(colour="black", size=16),
          axis.text.y = element_text(colour="black", size=16)) 
		  
png(file="Roots and SOC.png", height=600, width=600)
ggplot(meltprop, aes(x=depth, y = value, group=variable)) + 
    geom_line(aes(colour=variable), size=1.5)+#(stat = "identity", position = "dodge") +
	#geom_errorbar(aes(ymax = poxconc + poxconcse, ymin=poxconc - poxconcse), position = dodge, width=0.25) +
	coord_flip()+
	facet_wrap(~trt)+
	scale_fill_discrete(breaks=c("CC", "CCW", "CS", "P", "PF", "SC"), 
	  labels = c("Continuous Corn", "Continuous Corn with Rye", "Corn-soybean", "Prairie", "Fertilized Prairie", "Soybean-corn"))+
	guides(col = guide_legend(reverse = TRUE))+
		labs(y = "Proportion",x = "Average depth (cm)")+
	    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(),
          legend.position=c(.50,.25), legend.title=element_blank(),
		  legend.text = element_text(size=12),
		  axis.title.x = element_text(size=14,vjust=-0.5),
          axis.title.y = element_text(size=14,angle=90),
          axis.text.x = element_text(colour="black", size=16),
          axis.text.y = element_text(colour="black", size=16)) 
dev.off()
 
