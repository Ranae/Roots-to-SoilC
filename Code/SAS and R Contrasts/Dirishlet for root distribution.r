library(reshape2)
library(ggplot2)
library(plotrix)

roots<-read.table("COBS Roots 2008-2013noCNreal08v2.txt", header = TRUE)

roots$trt<-as.character(roots$trt)

roots$trt[roots$plot == "11"]<-"SC"
roots$trt[roots$plot == "33"]<-"SC"
roots$trt[roots$plot == '26']<-"SC"
roots$trt[roots$plot == '44']<-"SC"
roots$trt[roots$plot == '16']<-"CS"
roots$trt[roots$plot == '34']<-"CS"
roots$trt[roots$plot == '22']<-"CS"
roots$trt[roots$plot == '45']<-"CS"



roots<-roots[(roots$year != 2008),]

plotmelt<-melt(roots, id=c("year", "block", "plot", "trt", "core", "depth"), measured="mass")
plotmeans<-dcast(plotmelt, year + block + plot + trt + core ~variable, sum, margins=TRUE, na.rm=TRUE)
plotmeans<-plotmeans[plotmeans$year != "(all)",]
plotmeans<-plotmeans[plotmeans$block != "(all)",]
plotmeans<-plotmeans[plotmeans$plot != "(all)",]
plotmeans<-plotmeans[plotmeans$core != "(all)",]
plotmeans<-plotmeans[plotmeans$trt != "(all)",]
plotmeans<-plotmeans[,-7]

make<-merge(plotmeans, roots, by=c("year", "block", "plot", "trt", "core"))
colnames(make)<-c("year", "block", "plot", "trt", "core", "sum", "depth", "mass")

make$prop<-make$mass/make$sum

make$point[make$depth %in% 5]<-2.5
make$point[make$depth %in% 15]<-10
make$point[make$depth %in% 30]<-22.5
make$point[make$depth %in% 60]<-45
make$point[make$depth %in% 100]<-80

plotmelt<-melt(make, id=c("year", "block", "plot", "trt", "core", "depth", "point"), measured=c("mass", "sum", "prop"))
plotmeans<-dcast(plotmelt, year + block + plot + trt + point + depth ~variable, mean, margins=TRUE, na.rm=TRUE)
plotmeans<-plotmeans[plotmeans$year != "(all)",]
plotmeans<-plotmeans[plotmeans$block != "(all)",]
plotmeans<-plotmeans[plotmeans$plot != "(all)",]
plotmeans<-plotmeans[plotmeans$trt != "(all)",]
plotmeans<-plotmeans[plotmeans$depth != "(all)",]
plotmeans<-plotmeans[plotmeans$point != "(all)",]
plotmeans<-plotmeans[,-10]

roots2<-plotmeans

rootscc<-roots2[(roots2$trt == "CC"),]
rootscc<-rootscc[,c(1,3,5,9)]
cct<-reshape(rootscc, timevar="point", idvar=c("year", "plot"), direction="wide")

al<- DR_data(cct[, 3:7])
cmod<- DirichReg(al ~ year, cct)

rootsccw<-roots2[(roots2$trt == "CCW"),]
rootsccw<-rootsccw[,c(1,3,5,9)]
ccwt<-reshape(rootsccw, timevar="point", idvar=c("year", "plot"), direction="wide")
al<- DR_data(ccwt[, 3:7])
ccwmod<- DirichReg(al ~ year, ccwt)

rootscs<-roots2[(roots2$trt == "CS"),]
rootscs<-rootscs[,c(1,3,5,9)]
cst<-reshape(rootscs, timevar="point", idvar=c("year", "plot"), direction="wide")
al<- DR_data(cst[, 3:7])
csmod<- DirichReg(al ~ year, cst)

rootssc<-roots2[(roots2$trt == "SC"),]
rootssc<-rootssc[,c(1,3,5,9)]
sct<-reshape(rootssc, timevar="point", idvar=c("year", "plot"), direction="wide")
al<- DR_data(sct[, 3:7])
scmod<- DirichReg(al ~ year, sct)

rootsp<-roots2[(roots2$trt == "P"),]
rootsp<-rootsp[,c(1,3,5,9)]
pt<-reshape(rootsp, timevar="point", idvar=c("year", "plot"), direction="wide")
alp<- DR_data(pt[, 3:7])
pmod<- DirichReg(alp ~ year, pt)

rootspf<-roots2[(roots2$trt == "PF"),]
rootspf<-rootspf[,c(1,3,5,9)]
pft<-reshape(rootspf, timevar="point", idvar=c("year", "plot"), direction="wide")
alpf<- DR_data(pft[, 3:7])
pfmod<- DirichReg(alp ~ year, pft)


trtmelt<-melt(roots2, id=c("block", "plot", "trt", "year", "depth", "point"), measured=c("sum", "mass", "prop"))
trtmeans<-dcast(trtmelt, year + trt + depth + point ~variable, mean, margins=TRUE, na.rm=TRUE)
trtmeans<-trtmeans[trtmeans$year != "(all)",]
trtmeans<-trtmeans[trtmeans$trt != "(all)",]
trtmeans<-trtmeans[trtmeans$depth != "(all)",]
trtmeans<-trtmeans[trtmeans$point != "(all)",]
trtmeans<-trtmeans[,-8]

trtmeans$year = as.numeric(as.character(trtmeans$year))

this<-ggplot(trtmeans, aes(x = year, y = prop, fill = factor(point))) + 
        geom_area(position = 'stack')+
		scale_y_reverse()+
		facet_wrap(~ trt)+
		labs(x = "Year", 
             y = "Proportion of Total Root Mass") +
        theme(axis.text = element_text(size = 12, color = 'black'),
              axis.title = element_text(size = 12, face = 'bold'),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.title = element_text(size = 14, face = 'bold'),
			  axis.text.x = element_text(angle = 90, hjust=1))
			  
pdf("Stacked root distribution.pdf",width = 8, height = 8)
print(this)
dev.off()