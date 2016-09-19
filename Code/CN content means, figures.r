#setwd("C:/Users/rdietzel/Dropbox/C Manuscript/Organized Data")
cn<-read.table("Data/Root CN content.txt", header = TRUE)

cn$point[cn$depth %in% 5]<-2.5
cn$point[cn$depth %in% 15]<-10
cn$point[cn$depth %in% 30]<-22.5
cn$point[cn$depth %in% 60]<-45
cn$point[cn$depth %in% 100]<-80

cn$trt<-as.character(cn$trt)
cn$trt[cn$plot %in% c("11", "33", "26", "44")]<-"SC"
cn$trt[cn$plot %in% c('16', "34", "22", "45")]<-"CS"
cn$trt[cn$plot %in% c("12", "35", "21", "43")]<-"CC"
cn$trt[cn$plot %in% c('14', "36", "25", "42")]<-"CCW"
cn$trt[cn$plot %in% c("13", "31", "24", "46")]<-"P"
cn$trt[cn$plot %in% c('15', "32", "23", "41")]<-"PF"

library(reshape2)
library(ggplot2)
library(plotrix)

trtmelt<-melt(cn, id=c("year", "plot", "trt", "depth", "point"), measured=c("perC", "perN", "CN"))
trtmeans<-dcast(trtmelt, year + trt + depth + point ~variable, mean, margins=TRUE, na.rm=TRUE)
trtmeans<-trtmeans[trtmeans$year != "(all)",]
trtmeans<-trtmeans[trtmeans$trt != "(all)",]
trtmeans<-trtmeans[trtmeans$depth != "(all)",]
trtmeans<-trtmeans[trtmeans$point != "(all)",]
trtmeans<-trtmeans[,-8]

trterr<-dcast(trtmelt, year + trt + depth + point ~variable, std.error, margins=TRUE, na.rm=TRUE)
trterr<-trterr[trterr$year != "(all)",]
trterr<-trterr[trterr$trt != "(all)",]
trterr<-trterr[trterr$depth != "(all)",]
trterr<-trterr[trterr$point != "(all)",]
trterr<-trterr[,-8]

cnsum<-merge(trtmeans, trterr, by = c("year", "trt", "depth", "point"))
colnames(cnsum)<-c("year", "trt", "depth", "point", "perC", "perN", "CN", "perCse", "perNse", "CNse")

trtmelt<-melt(trtmeans, id=c("year", "trt", "depth", "point"), measured=c("perC", "perN", "CN"))
trtmeans<-dcast(trtmelt, year + trt  ~variable, mean, margins=TRUE, na.rm=TRUE)
trtmeans<-trtmeans[trtmeans$year != "(all)",]
trtmeans<-trtmeans[trtmeans$trt != "(all)",]
trtmeans<-trtmeans[trtmeans$depth != "(all)",]
trtmeans<-trtmeans[trtmeans$point != "(all)",]
trtmeans<-trtmeans[,-8]

cnsum <- within(cnsum, point <- ordered(point, levels = rev(sort(unique(point)))))
cnsum2 <- cnsum[cnsum$trt %in% c("P", "PF", "CC"),]

dodge <- position_dodge(width=.9)

##CN ratios over years, each depth, barplot
ggplot(cnsum, aes(x=factor(year), y = CN, fill=trt)) + 
    geom_bar(stat = "identity", position = "dodge") +
	geom_errorbar(aes(ymax = CN + CNse, ymin=CN - CNse), position = dodge, width=0.25) +
	facet_wrap(~ depth)

theme_set(theme_bw())

##CN ratios over depths (profile), each year
e<-ggplot(data = cnsum2,aes(x = point,y = CN)) + 
    geom_ribbon(aes(group = trt,ymin = CN - CNse,ymax = CN + CNse),alpha = 0.25) + 
    geom_line(aes(group = trt,linetype = trt,colour = trt),size = 1) + 
	coord_flip() +
	facet_wrap(~ year, ncol = 1)+
	labs(x = "Depth (cm)",y = "C:N Ratio") + 
    theme(#panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          axis.line = element_line(),
          legend.position=c(.80,.93), legend.title=element_blank(),
		  legend.text = element_text(size=12),
		  axis.title.x = element_text(size=14,vjust=-0.5),
          axis.title.y = element_text(size=14,angle=90),
          axis.text.x = element_text(colour="black", size=16),
          axis.text.y = element_text(colour="black", size=16)) 

##CN ratios over years, each depth, line	
d<-ggplot(data = cnsum2,aes(x = year,y = CN)) + 
    geom_ribbon(aes(group = trt,ymin = CN - CNse,ymax = CN + CNse),alpha = 0.25) + 
    geom_line(aes(group = trt,colour = trt),size = 1) + 
	scale_colour_discrete(breaks=c("CC", "CCW", "CS", "P", "PF", "SC"), 
	  labels = c("Continuous Corn", "Continuous Corn with Rye", "Corn-soybean", "Prairie", "Fertilized Prairie", "Soybean-corn"))+
	facet_wrap(~ depth)+
	labs(x = "Year",y = "C:N Ratio") + 
    theme(#panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          axis.line = element_line(),
          legend.position=c(.85,.25), legend.title=element_blank(),
		  legend.text = element_text(size=12),
		  axis.title.x = element_text(size=14,vjust=-0.5),
          axis.title.y = element_text(size=14,angle=90),
          axis.text.x = element_text(colour="black", size=10),
          axis.text.y = element_text(colour="black", size=16)) 


##%N over years, line	
ggplot(data = cnsum,aes(x = year,y = perN)) + 
    geom_ribbon(aes(group = trt,ymin = perN - perNse,ymax = perN + perNse),alpha = 0.25) + 
    geom_line(aes(group = trt,linetype = trt,colour = trt),size = 1) + 
	facet_wrap(~ depth)

##%C over year, line	
ggplot(data = cnsum,aes(x = year,y = perC)) + 
    geom_ribbon(aes(group = trt,ymin = perC - perCse,ymax = perC + perCse),alpha = 0.25) + 
    geom_line(aes(group = trt,linetype = trt,colour = trt),size = 1) + 
	facet_wrap(~ depth)
	
pdf("Figures/CN ratios over timev2.pdf",width = 8, height = 8)
print(d)
dev.off()

pdf("Figures/CN ratios over timev2, stacked depth.pdf",width = 8, height = 16)
print(e)
dev.off()
