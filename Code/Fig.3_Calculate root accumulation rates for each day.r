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
#ratemeans2$g.m2.day<-ratemeans2$Mg.ha.day*100
#ratemeans2$g.m2.day.se<-ratemeans2$Mg.ha.day.se*100

ratemeans2$Mg.ha.year<-ratemeans2$Mg.ha.day*365
ratemeans2$Mg.ha.year.se<-ratemeans2$Mg.ha.day.se*365

			 
b5<-ggplot(data = filter(ratemeans2, depth == 5),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = trt,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = trt,colour = trt, linetype = trt),size = 1.1) + 
  labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  #scale_x_discrete(breaks=c(153,520,882,1248,1619,1978))+
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  #ylim(0,.0025) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))

b15<-ggplot(data = filter(ratemeans2, depth == 15),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = trt,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = trt,colour = trt, linetype = trt),size = 1.1) + 
  labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  #scale_x_discrete(breaks=c(153,520,882,1248,1619,1978))+
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  ylim(0,.25) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))

b30<-ggplot(data = filter(ratemeans2, depth == 30),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = trt,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = trt,colour = trt, linetype = trt),size = 1.1) + 
  labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  #scale_x_discrete(breaks=c(153,520,882,1248,1619,1978))+
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  ylim(0,.25) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))

b60<-ggplot(data = filter(ratemeans2, depth == 60),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = trt,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = trt,colour = trt, linetype = trt),size = 1.1) + 
  labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  #scale_x_discrete(breaks=c(153,520,882,1248,1619,1978))+
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  ylim(0,.25) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))

b100<-ggplot(data = filter(ratemeans2, depth == 100),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = trt,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = trt,colour = trt, linetype = trt),size = 1.1) + 
  labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  #scale_x_discrete(breaks=c(153,520,882,1248,1619,1978))+
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  ylim(0,.25) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))

alldepths<-ggplot(data = filter(ratemeans2),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = depth,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = depth,colour = depth, linetype = depth),size = 1.1) + 
  facet_wrap(~trt, nrow=3)+
  labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  scale_x_discrete(breaks=c(153,520,882,1248,1619,1978))+
  #scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  #ylim(0,.25) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))

####Straight up root biomass
log<-function(t, Asym, xmid, scal){
  
  ans = Asym/(1+exp((xmid-t)/scal))
  
  ans
}

t<- 0:1978
rates <- NULL
for(i in 1:dim(coeff)[1]){
  rates <- cbind(rates, log(t, coeff[i,1], coeff[i,2], coeff[i,3]))
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

write.csv(rootrates, file="Root accum rates for each plot each depth each dayv2.csv", row.names=FALSE)

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

massmeans<-merge(plotmeans, plotstderr, by = c("tu", "trt", "depth"))
colnames(massmeans)<-c("day", "trt", "depth", "Mg.ha.day", "Mg.ha.day.se")

massmeans <- within(massmeans, day <- ordered(day, levels = sort(unique(day))))

massmeans<-massmeans[order(as.numeric(massmeans$day)),,drop=FALSE]
massmeans$depth <- factor(massmeans$depth, levels = c("5", "15", "30", "60", "100"))

massmeansno5<-massmeans[massmeans$depth != 5,]
massmeans5<-massmeans[massmeans$depth == 5,]

massmeans2<-massmeans[massmeans$trt %in% c("P", "PF", "CC"),]

massmeans3<-massmeans2%>%
  mutate(trt = ifelse((trt == "CC"), "Maize",
                      ifelse((trt == "P"), "Prairie",
                             ifelse((trt == "PF"), "Fertilized Prairie", NA))))

massmeans3$trt<-factor(massmeans3$trt, levels = c("Maize", "Fertilized Prairie", "Prairie"))



			 
a5<-ggplot(data = filter(massmeans2, depth == 5),aes(x = day,y = Mg.ha.day)) + 
  geom_ribbon(aes(group = trt,ymin = Mg.ha.day - Mg.ha.day.se,ymax = Mg.ha.day + Mg.ha.day.se),alpha = 0.25) + 
  geom_line(aes(group = trt,colour = trt, linetype=trt),size = 1) + 
  labs(x = "Days after establishment",y = (expression(paste("Root pool mass (Mg ha" ^ "-1",")")))) + 
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  ylim(0,4.2) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))

a15<-ggplot(data = filter(massmeans2, depth == 15),aes(x = day,y = Mg.ha.day)) + 
  geom_ribbon(aes(group = trt,ymin = Mg.ha.day - Mg.ha.day.se,ymax = Mg.ha.day + Mg.ha.day.se),alpha = 0.25) + 
  geom_line(aes(group = trt,colour = trt, linetype=trt),size = 1) + 
  labs(x = "Days after establishment",y = (expression(paste("Root pool mass (Mg ha" ^ "-1",")")))) + 
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  ylim(0,4.2) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))

a30<-ggplot(data = filter(massmeans2, depth == 30),aes(x = day,y = Mg.ha.day)) + 
  geom_ribbon(aes(group = trt,ymin = Mg.ha.day - Mg.ha.day.se,ymax = Mg.ha.day + Mg.ha.day.se),alpha = 0.25) + 
  geom_line(aes(group = trt,colour = trt, linetype=trt),size = 1) + 
  labs(x = "Days after establishment",y = (expression(paste("Root pool mass (Mg ha" ^ "-1",")")))) + 
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  ylim(0,4.2) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))

a60<-ggplot(data = filter(massmeans2, depth == 60),aes(x = day,y = Mg.ha.day)) + 
  geom_ribbon(aes(group = trt,ymin = Mg.ha.day - Mg.ha.day.se,ymax = Mg.ha.day + Mg.ha.day.se),alpha = 0.25) + 
  geom_line(aes(group = trt,colour = trt, linetype=trt),size = 1) + 
  labs(x = "Days after establishment",y = (expression(paste("Root pool mass (Mg ha" ^ "-1",")")))) + 
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  ylim(0,4.2) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))

a100<-ggplot(data = filter(massmeans2, depth == 100),aes(x = day,y = Mg.ha.day)) + 
  geom_ribbon(aes(group = trt,ymin = Mg.ha.day - Mg.ha.day.se,ymax = Mg.ha.day + Mg.ha.day.se),alpha = 0.25) + 
  geom_line(aes(group = trt,colour = trt, linetype=trt),size = 1) + 
  labs(x = "Days after establishment",y = (expression(paste("Root pool mass (Mg ha" ^ "-1",")")))) + 
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  ylim(0,4.2) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black", size=12))



###This one is in the publication, Figure 3
pdf(file = "Figures/Root accumulation by mass.pdf", height = 8, width = 10, family = "Times")
ggplot(data = filter(massmeans3),aes(x = day,y = Mg.ha.day)) + 
  geom_ribbon(aes(group = depth,ymin = Mg.ha.day - Mg.ha.day.se,ymax = Mg.ha.day + Mg.ha.day.se),alpha = 0.25) + 
  geom_line(aes(group = depth,colour = depth, linetype=depth),size = 1.3) + 
  facet_wrap(~trt)+
  labs(x = "Year",y = (expression(paste("Root pool mass (Mg ha" ^ "-1",")")))) + 
  scale_x_discrete(breaks=c(153,520,882,1248,1619,1978), labels = c("2008", "2009", "2010", "2011", "2012", "2013")) +
  ylim(0,4.2) + 
  scale_colour_manual(values = otPalette)+
  theme(#panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position=c(.17, .72), legend.title=element_blank(),
        legend.text = element_text(size=22),  
        #axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=16, angle = 45, hjust=1, vjust=1),
        axis.text.y = element_text(colour="black", size=18))
dev.off()


vplayout<- function(x,y)
  viewport(layout.pos.row=x, layout.pos.col=y)

pdf("Figures/Root Accumulation and Ratesv4.pdf", width = 10, height = 15)
grid.newpage()
pushViewport(viewport(layout = grid.layout(6,4, heights = unit(c(5,5,5,5,5,.39), "null"), widths = unit(c(.38,5,.38,5), "null"))))
grid.text((expression(paste("Biomass (Mg ha" ^ "-1",")"))), rot = 90, vp = viewport(layout.pos.row = 1:5, layout.pos.col = 1), gp=gpar(fontsize=18))
grid.text((expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")"))), rot = 90, vp = viewport(layout.pos.row = 1:5, layout.pos.col = 3), gp=gpar(fontsize=18))
grid.text("Days after establishment", vp = viewport(layout.pos.row = 6, layout.pos.col = 2:4), gp=gpar(fontsize=18))
print(a5, vp = vplayout(1,2))
print(a15, vp = vplayout (2,2))
print(a30, vp = vplayout(3,2))
print(a60, vp = vplayout (4,2))
print(a100, vp = vplayout(5,2))
print(b5, vp = vplayout (1,4))
print(b15, vp = vplayout(2,4))
print(b30, vp = vplayout (3,4))
print(b60, vp = vplayout(4,4))
print(b100, vp = vplayout (5,4))
dev.off()

write.csv(ratemeans, file="Treatment means and std err of root accumulation ratesv2.csv", row.names=FALSE)

ratescc<-ggplot(data = filter(ratemeans2, trt=="CC"),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = depth,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = depth,colour = depth, linetype = depth),size = 1.3) + 
  #labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  #scale_x_discrete(breaks=c(153,520,882,1248,1619,1978))+
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
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

ratesp<-ggplot(data = filter(ratemeans2, trt=="P"),aes(x = day,y = Mg.ha.year)) + 
  #geom_ribbon(aes(group = depth,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  #geom_line(aes(group = depth,colour = depth, linetype = depth),size = 1.3) + 
  geom_ribbon(aes(group = depth,ymin = Mg.ha.year - Mg.ha.year.se,ymax = Mg.ha.year + Mg.ha.year.se),alpha = 0.25) + 
  geom_line(aes(group = depth,colour = depth, linetype = depth),size = 1.3) + 
  labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  #scale_x_discrete(breaks=c(153,520,882,1248,1619,1978))+
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  scale_colour_manual(values = otPalette)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="none", legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour="black", size=22),
        axis.text.y = element_text(colour="black", size=22))

ratespf<-ggplot(data = filter(ratemeans2, trt=="PF"),aes(x = day,y = g.m2.day)) + 
  geom_ribbon(aes(group = depth,ymin = g.m2.day - g.m2.day.se,ymax = g.m2.day + g.m2.day.se),alpha = 0.25) + 
  geom_line(aes(group = depth,colour = depth, linetype = depth),size = 1.3) + 
  labs(x = "Days after establishment",y = (expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")")))) + 
  #scale_x_discrete(breaks=c(153,520,882,1248,1619,1978))+
  scale_x_discrete(breaks=c(100,400,700,1000,1300,1600,1900))+
  scale_colour_manual(values = otPalette)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position=c(.75, .50), legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour="black", size=22))

###Remember! You moved this to its own file!!!
pdf("Figures/Root Accumulation and Ratesv5, three panels.pdf", width = 10, height = 15)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,2, heights = unit(c(5,5,5,.39), "null"), widths = unit(c(.38,5), "null"))))
#grid.text((expression(paste("Biomass (Mg ha" ^ "-1",")"))), rot = 90, vp = viewport(layout.pos.row = 1:5, layout.pos.col = 1), gp=gpar(fontsize=18))
grid.text((expression(paste("Accumulation Rate (g m" ^ "-2","day" ^ "-1",")"))), rot = 90, vp = viewport(layout.pos.row = 1:3, layout.pos.col = 1), gp=gpar(fontsize=22))
grid.text("Days after establishment", vp = viewport(layout.pos.row = 4, layout.pos.col = 2), gp=gpar(fontsize=22))
print(ratescc, vp = vplayout(1,2))
print(ratespf, vp = vplayout (2,2))
print(ratesp, vp = vplayout(3,2))
dev.off()