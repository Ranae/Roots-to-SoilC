library(ggplot2)
this_theme<-theme_bw()+
  theme(#panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    legend.position='none', legend.title=element_blank(),
    legend.text = element_text(size=12),
    axis.title.x = element_text(size=22,vjust=-0.5),
    axis.title.y = element_text(size=22,angle=90, vjust=1.2),
    axis.text.x = element_text(colour="black", size=18),
    axis.text.y = element_text(colour="black", size=18), 
    strip.text = element_text(colour="black", size=18))

theme_set(this_theme)

vgPalette <- c("#EBD46C", "#CA7636", "#8F3928", "#63602D", "#3A2814")
otPalette <- c("#fff275", "#ff8c42", "#ff3c38", "#a23e48", "#3b5166")

coeff<-read.csv("Data/Logistic parameters of root accumulationv2.csv", header=TRUE)

log.rate<-function(t, Asym, xmid, scal){
  
  a<-Asym/scal
  b<-exp((xmid - t)/scal)
  c<- (1+exp((xmid-t)/scal))^2
  
  ans = a * (b/c)
  
  ans
}

t<- 0:1978
rates <- NULL
for(i in 1:dim(coeff)[1]){
  rates <- cbind(rates, log.rate(t, coeff[i,1], coeff[i,2], coeff[i,3]))
}
colnames(rates) <- (as.factor(coeff$plot):(as.factor(coeff$depth)))


rootrates <- data.frame(tu=rep(t,120), eu=rep(colnames(rates),each=length(t)), Mg.ha.day=c(rates))

rootrates$plot<-sub("^([^:]*):.*$", "\\1", rootrates$eu)
rootrates$depth<-sub("^[^:]*:(.*$)", "\\1", rootrates$eu)

rootrates$trt[rootrates$plot %in% c("11", "33", "26", "44")]<-"SC"
rootrates$trt[rootrates$plot %in% c('16', "34", "22", "45")]<-"CS"
rootrates$trt[rootrates$plot %in% c("12", "35", "21", "43")]<-"CC"
rootrates$trt[rootrates$plot %in% c('14', "36", "25", "42")]<-"CCW"
rootrates$trt[rootrates$plot %in% c("13", "31", "24", "46")]<-"P"
rootrates$trt[rootrates$plot %in% c('15', "32", "23", "41")]<-"PF"

library(reshape2)
library(plotrix)
plotmelt<-melt(rootrates, id=c("eu","tu","plot", "trt", "depth"), measured="Mg.ha.day")
plotmeans<-dcast(plotmelt, tu + trt + depth ~variable, mean, margins=TRUE, na.rm=TRUE)
plotmeans<-plotmeans[plotmeans$depth != "(all)",]
plotmeans<-plotmeans[plotmeans$trt != "(all)",]
plotmeans<-plotmeans[plotmeans$tu != "(all)",]
plotmeans<-plotmeans[,-5]

plotstderr<-dcast(plotmelt, tu + trt + depth ~variable, std.error, margins=TRUE, na.rm=TRUE)
plotstderr<-plotstderr[plotstderr$depth != "(all)",]
plotstderr<-plotstderr[plotstderr$trt != "(all)",]
plotstderr<-plotstderr[plotstderr$tu != "(all)",]
plotstderr<-plotstderr[,-5]

ratemeans<-merge(plotmeans, plotstderr, by = c("tu", "trt", "depth"))
colnames(ratemeans)<-c("day", "trt", "depth", "Mg.ha.day", "Mg.ha.day.se")

ratemeans <- within(ratemeans, day <- ordered(day, levels = sort(unique(day))))

ratemeans<-ratemeans[order(as.numeric(ratemeans$day)),,drop=FALSE]
ratemeans$depth <- factor(ratemeans$depth, levels = c("5", "15", "30", "60", "100"))

ratemeansno5<-ratemeans[ratemeans$depth != 5,]
ratemeans5<-ratemeans[ratemeans$depth == 5,]


ratemeans2<-ratemeans[ratemeans$trt %in% c("P", "PF", "CC"),]
ratemeans2$g.m2.day<-ratemeans2$Mg.ha.day*100
ratemeans2$g.m2.day.se<-ratemeans2$Mg.ha.day.se*100

ratescc<-ggplot(data = filter(ratemeans2, trt=="CC"),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = depth,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = depth,colour = depth, linetype = depth),size = 1.3) + 
  #labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  scale_x_discrete(breaks=c(153,520,882,1248,1619,1978), labels = c("2008", "2009", "2010", "2011", "2012", "2013"))+
  #scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  scale_colour_manual(values = otPalette)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour="black", size=22))

ratesp<-ggplot(data = filter(ratemeans2, trt=="P"),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = depth,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = depth,colour = depth, linetype = depth),size = 1.3) + 
  labs(x = "Year",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  scale_x_discrete(breaks=c(153,520,882,1248,1619,1978), labels = c("2008", "2009", "2010", "2011", "2012", "2013"))+
  #scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  scale_colour_manual(values = otPalette)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=22, angle = 45, hjust=1, vjust=1),
        axis.text.y = element_text(colour="black", size=22))

ratespf<-ggplot(data = filter(ratemeans2, trt=="PF"),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = depth,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = depth,colour = depth, linetype = depth),size = 1.3) + 
  labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  scale_x_discrete(breaks=c(153,520,882,1248,1619,1978), labels = c("2008", "2009", "2010", "2011", "2012", "2013"))+
  #scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  scale_colour_manual(values = otPalette)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position=c(.75, .55), legend.title=element_blank(),
        legend.text = element_text(size=22), legend.key.size=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour="black", size=22))

pdf("Figures/Root Accumulation and Ratesv5, three panels.pdf", width = 10, height = 15)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,2, heights = unit(c(5,5,5,.39), "null"), widths = unit(c(.38,5), "null"))))
#grid.text((expression(paste("Biomass (Mg ha" ^ "-1",")"))), rot = 90, vp = viewport(layout.pos.row = 1:5, layout.pos.col = 1), gp=gpar(fontsize=18))
grid.text((expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")"))), rot = 90, vp = viewport(layout.pos.row = 1:3, layout.pos.col = 1), gp=gpar(fontsize=24))
grid.text("Year", vp = viewport(layout.pos.row = 4, layout.pos.col = 2), gp=gpar(fontsize=24))
print(ratescc, vp = vplayout(1,2))
print(ratespf, vp = vplayout (2,2))
print(ratesp, vp = vplayout(3,2))
dev.off()