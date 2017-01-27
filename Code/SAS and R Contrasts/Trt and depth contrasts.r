#read data
roots<-read.table("COBS Roots 2008-2013noCNreal08v2.txt", header = TRUE)

library(reshape2)
library(plotrix)
plotmelt<-melt(roots, id=c("year", "block", "trt","plot", "core", "depth"), measured="mass")
plotmeans<-dcast(plotmelt, year + block + trt + plot + depth ~variable, mean, margins=TRUE, na.rm=TRUE)
plotmeans<-plotmeans[plotmeans$year != "(all)",]
plotmeans<-plotmeans[plotmeans$plot != "(all)",]
plotmeans<-plotmeans[plotmeans$block != "(all)",]
plotmeans<-plotmeans[plotmeans$trt != "(all)",]
plotmeans<-plotmeans[plotmeans$depth != "(all)",]
plotmeans<-plotmeans[,-7]

roots$point[roots$depth %in% 5]<-2.5
roots$point[roots$depth %in% 15]<-10
roots$point[roots$depth %in% 30]<-22.5
roots$point[roots$depth %in% 60]<-45
roots$point[roots$depth %in% 100]<-80
roots$point[roots$year %in% 2008 & roots$depth %in% 30]<-15

roots$trt<-as.character(roots$trt)
roots$trt[roots$plot %in% c("11", "33", "26", "44")]<-"SC"
roots$trt[roots$plot %in% c('16', "34", "22", "45")]<-"CS"

#convert to factors
roots$block.f <- as.factor(roots$block)
roots$plot.f <- as.factor(roots$plot)
roots$core.f <- as.factor(roots$core)
roots$depth.f <- as.factor(roots$depth)

###2009
roots<-roots[roots$year == "2009",]

library(lme4)
#random effects as random
#this model gives REML estimates of sigma_error, sigma_block*trt, and sigma_sub(block*trt)
lm <- lmer(mass ~ block.f + (1|block.f:trt) + trt + (1|core.f:trt:block.f) + depth.f +
             depth.f:trt, data = roots)

#random effects as fixed
#get E(MS) for random effects, solve for sigma_error, sigma_block*trt, and sigma_sub(block*trt)
#get results that agree with SAS - negative estimate for sigma squared block*trt
lm2 <- lm(mass ~ block.f + block.f:trt + trt + core.f:trt:block.f + depth.f +
             depth.f:trt, data = roots)



##############################################
#MAIN PLOT TREATMENT EFFECTS DEPTH SPECIFIC  #
##############################################
###################################
# BY DEPTH                        #
###################################
k <- c(2,3,4)
#formula for standard error with k subsamples
#(plugged in values for sigmasq eu, sub, e)
tsed <- sqrt(.91/2 + .053/(2*k) + .48/(2*k)) 
#widths of 95% confidence intervals for k = 4,3,2
widths <- 2*qt(.975, df=15)*se
#this gives estimates for each of the 6 treatments, for each level of depth
tmeansd <- t( matrix( c( 
  #for depth = 5   
  c(1, 1/4,1/4,1/4, 1,0,0,0,0, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  c(1, 1/4,1/4,1/4, 0,1,0,0,0, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  c(1, 1/4,1/4,1/4, 0,0,1,0,0, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  c(1, 1/4,1/4,1/4, 0,0,0,1,0, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  c(1, 1/4,1/4,1/4, 0,0,0,0,1, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  c(1, 1/4,1/4,1/4, 0,0,0,0,0, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  
  
  #for depth =15
  c(1, 1/4,1/4,1/4, 1,0,0,0,0, 1,0,0,0, 1,0,0,0,0, rep(0,15)),
  c(1, 1/4,1/4,1/4, 0,1,0,0,0, 1,0,0,0, 0,1,0,0,0, rep(0,15)),
  c(1, 1/4,1/4,1/4, 0,0,1,0,0, 1,0,0,0, 0,0,1,0,0, rep(0,15)),
  c(1, 1/4,1/4,1/4, 0,0,0,1,0, 1,0,0,0, 0,0,0,1,0, rep(0,15)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,1, 1,0,0,0, 0,0,0,0,1, rep(0,15)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,0, 1,0,0,0,  rep(c(0,0,0,0,0),4)), 

#for depth =30
  c(1, 1/4,1/4,1/4, 1,0,0,0,0, 0,1,0,0, rep(0,5) ,1,0,0,0,0, rep(0,10)),
  c(1, 1/4,1/4,1/4, 0,1,0,0,0, 0,1,0,0, rep(0,5) ,0,1,0,0,0, rep(0,10)),
  c(1, 1/4,1/4,1/4, 0,0,1,0,0, 0,1,0,0, rep(0,5) ,0,0,1,0,0, rep(0,10)),
  c(1, 1/4,1/4,1/4, 0,0,0,1,0, 0,1,0,0, rep(0,5) ,0,0,0,1,0, rep(0,10)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,1, 0,1,0,0, rep(0,5) ,0,0,0,0,1, rep(0,10)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,0, 0,1,0,0, rep(0,20)),
#for depth = 60  
  c(1, 1/4,1/4,1/4, 1,0,0,0,0, 0,0,1,0, rep(0,10) ,1,0,0,0,0, rep(0,5)),
  c(1, 1/4,1/4,1/4, 0,1,0,0,0, 0,0,1,0, rep(0,10) ,0,1,0,0,0, rep(0,5)),
  c(1, 1/4,1/4,1/4, 0,0,1,0,0, 0,0,1,0, rep(0,10) ,0,0,1,0,0, rep(0,5)),
  c(1, 1/4,1/4,1/4, 0,0,0,1,0, 0,0,1,0, rep(0,10) ,0,0,0,1,0, rep(0,5)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,1, 0,0,1,0, rep(0,10) ,0,0,0,0,1, rep(0,5)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,0, 0,0,1,0, rep(0,20)),
 
 #for depth = 100 
  c(1, 1/4,1/4,1/4, 1,0,0,0,0, 0,0,0,1, rep(0,15) ,1,0,0,0,0),
  c(1, 1/4,1/4,1/4, 0,1,0,0,0, 0,0,0,1, rep(0,15) ,0,1,0,0,0),
  c(1, 1/4,1/4,1/4, 0,0,1,0,0, 0,0,0,1, rep(0,15) ,0,0,1,0,0),
  c(1, 1/4,1/4,1/4, 0,0,0,1,0, 0,0,0,1, rep(0,15) ,0,0,0,1,0),
  c(1, 1/4,1/4,1/4, 0,0,0,0,1, 0,0,0,1, rep(0,15) ,0,0,0,0,1),
  c(1, 1/4,1/4,1/4, 0,0,0,0,0, 0,0,0,1, rep(0,20)) ),ncol=30 ))%*%fixef(lm)


tmeansd <- data.frame(trt = rep(c("CC", "CCW", "P","PF", "SC", "CS"),5), 
                      depth = rep(c(5,15,30,60,100),c(6,6,6,6,6)), 
                      mean = tmeansd)

tmeansd$trt <- as.character(tmeansd$trt)



tcombsd <- data.frame(t(combn(as.vector(unique(tmeansd$trt)),2)))
colnames(tcombsd) <- c("trt1", "trt2")

tcombsd$trt1 <- as.character(tcombsd$trt1)
tcombsd$trt2 <- as.character(tcombsd$trt2)

#get differences between expected values
tcombsd$diff <- NA
tcombsdata <- NA
for (j in unique(tmeansd$depth)) {
  ddata <- subset(tmeansd, depth == j)
for (i in 1:length(tcombsd$trt1)){
  tcombsd$diff[i] <- ddata$mean[ddata$trt == tcombsd$trt1[i]]-ddata$mean[ddata$trt==tcombsd$trt2[i]]
  tcombsd$depth[i] <- j
}   
tcombsdata <- rbind(tcombsd, tcombsdata)
}
tcombsd <- tcombsdata[-76,]


#what would the t-values be for the changing standard errors based on k = 4,3,2
tcombsd$t4 <- tcombsd$diff/tsed[3]
tcombsd$t3 <- tcombsd$diff/tsed[2]
tcombsd$t2 <- tcombsd$diff/tsed[1]

#What are the p-values for each of the numbers of p-values
tcombsd$p4 <- 2*(1-pt(abs(tcombsd$t4), df = 15))
tcombsd$p3 <- 2*(1-pt(abs(tcombsd$t3), df = 15))
tcombsd$p2 <- 2*(1-pt(abs(tcombsd$t2), df = 15))


#is the difference significant for 2,3,4 subsamples?
tcombsd$sig4 <- tcombsd$t4 > qt(.975, df=15)
tcombsd$sig3 <- tcombsd$t3 > qt(.975, df=15)
tcombsd$sig2 <- tcombsd$t2 > qt(.975, df=15)

tcombsd  ###Not sure these are the right results

###2010
roots<-roots[roots$year == "2010",]

library(lme4)
#random effects as random
#this model gives REML estimates of sigma_error, sigma_block*trt, and sigma_sub(block*trt)
lm <- lmer(mass ~ block.f + (1|block.f:trt) + trt + (1|core.f:trt:block.f) + depth.f +
             depth.f:trt, data = roots)

#random effects as fixed
#get E(MS) for random effects, solve for sigma_error, sigma_block*trt, and sigma_sub(block*trt)
#get results that agree with SAS - negative estimate for sigma squared block*trt
lm2 <- lm(mass ~ block.f + block.f:trt + trt + core.f:trt:block.f + depth.f +
             depth.f:trt, data = roots)



##############################################
#MAIN PLOT TREATMENT EFFECTS DEPTH SPECIFIC  #
##############################################
###################################
# BY DEPTH                        #
###################################
k <- c(2,3,4)
#formula for standard error with k subsamples
#(plugged in values for sigmasq eu, sub, e)
tsed <- sqrt(1.72/2 + .0138/(2*k) + .341/(2*k)) 
#widths of 95% confidence intervals for k = 4,3,2
widths <- 2*qt(.975, df=15)*se
#this gives estimates for each of the 6 treatments, for each level of depth
tmeansd <- t( matrix( c( 
  #for depth = 5   
  c(1, 1/4,1/4,1/4, 1,0,0,0,0, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  c(1, 1/4,1/4,1/4, 0,1,0,0,0, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  c(1, 1/4,1/4,1/4, 0,0,1,0,0, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  c(1, 1/4,1/4,1/4, 0,0,0,1,0, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  c(1, 1/4,1/4,1/4, 0,0,0,0,1, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  c(1, 1/4,1/4,1/4, 0,0,0,0,0, 0,0,0,0, rep(c(0,0,0,0,0),4)) ,
  
  
  #for depth =15
  c(1, 1/4,1/4,1/4, 1,0,0,0,0, 1,0,0,0, 1,0,0,0,0, rep(0,15)),
  c(1, 1/4,1/4,1/4, 0,1,0,0,0, 1,0,0,0, 0,1,0,0,0, rep(0,15)),
  c(1, 1/4,1/4,1/4, 0,0,1,0,0, 1,0,0,0, 0,0,1,0,0, rep(0,15)),
  c(1, 1/4,1/4,1/4, 0,0,0,1,0, 1,0,0,0, 0,0,0,1,0, rep(0,15)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,1, 1,0,0,0, 0,0,0,0,1, rep(0,15)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,0, 1,0,0,0,  rep(c(0,0,0,0,0),4)), 

#for depth =30
  c(1, 1/4,1/4,1/4, 1,0,0,0,0, 0,1,0,0, rep(0,5) ,1,0,0,0,0, rep(0,10)),
  c(1, 1/4,1/4,1/4, 0,1,0,0,0, 0,1,0,0, rep(0,5) ,0,1,0,0,0, rep(0,10)),
  c(1, 1/4,1/4,1/4, 0,0,1,0,0, 0,1,0,0, rep(0,5) ,0,0,1,0,0, rep(0,10)),
  c(1, 1/4,1/4,1/4, 0,0,0,1,0, 0,1,0,0, rep(0,5) ,0,0,0,1,0, rep(0,10)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,1, 0,1,0,0, rep(0,5) ,0,0,0,0,1, rep(0,10)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,0, 0,1,0,0, rep(0,20)),
#for depth = 60  
  c(1, 1/4,1/4,1/4, 1,0,0,0,0, 0,0,1,0, rep(0,10) ,1,0,0,0,0, rep(0,5)),
  c(1, 1/4,1/4,1/4, 0,1,0,0,0, 0,0,1,0, rep(0,10) ,0,1,0,0,0, rep(0,5)),
  c(1, 1/4,1/4,1/4, 0,0,1,0,0, 0,0,1,0, rep(0,10) ,0,0,1,0,0, rep(0,5)),
  c(1, 1/4,1/4,1/4, 0,0,0,1,0, 0,0,1,0, rep(0,10) ,0,0,0,1,0, rep(0,5)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,1, 0,0,1,0, rep(0,10) ,0,0,0,0,1, rep(0,5)),
  c(1, 1/4,1/4,1/4, 0,0,0,0,0, 0,0,1,0, rep(0,20)),
 
 #for depth = 100 
  c(1, 1/4,1/4,1/4, 1,0,0,0,0, 0,0,0,1, rep(0,15) ,1,0,0,0,0),
  c(1, 1/4,1/4,1/4, 0,1,0,0,0, 0,0,0,1, rep(0,15) ,0,1,0,0,0),
  c(1, 1/4,1/4,1/4, 0,0,1,0,0, 0,0,0,1, rep(0,15) ,0,0,1,0,0),
  c(1, 1/4,1/4,1/4, 0,0,0,1,0, 0,0,0,1, rep(0,15) ,0,0,0,1,0),
  c(1, 1/4,1/4,1/4, 0,0,0,0,1, 0,0,0,1, rep(0,15) ,0,0,0,0,1),
  c(1, 1/4,1/4,1/4, 0,0,0,0,0, 0,0,0,1, rep(0,20)) ),ncol=30 ))%*%fixef(lm)


tmeansd <- data.frame(trt = rep(c("CC", "CCW", "P","PF", "SC", "CS"),5), 
                      depth = rep(c(5,15,30,60,100),c(6,6,6,6,6)), 
                      mean = tmeansd)

tmeansd$trt <- as.character(tmeansd$trt)



tcombsd <- data.frame(t(combn(as.vector(unique(tmeansd$trt)),2)))
colnames(tcombsd) <- c("trt1", "trt2")

tcombsd$trt1 <- as.character(tcombsd$trt1)
tcombsd$trt2 <- as.character(tcombsd$trt2)

#get differences between expected values
tcombsd$diff <- NA
tcombsdata <- NA
for (j in unique(tmeansd$depth)) {
  ddata <- subset(tmeansd, depth == j)
for (i in 1:length(tcombsd$trt1)){
  tcombsd$diff[i] <- ddata$mean[ddata$trt == tcombsd$trt1[i]]-ddata$mean[ddata$trt==tcombsd$trt2[i]]
  tcombsd$depth[i] <- j
}   
tcombsdata <- rbind(tcombsd, tcombsdata)
}
tcombsd <- tcombsdata[-76,]


#what would the t-values be for the changing standard errors based on k = 4,3,2
tcombsd$t4 <- tcombsd$diff/tsed[3]
tcombsd$t3 <- tcombsd$diff/tsed[2]
tcombsd$t2 <- tcombsd$diff/tsed[1]

#What are the p-values for each of the numbers of p-values
tcombsd$p4 <- 2*(1-pt(abs(tcombsd$t4), df = 15))
tcombsd$p3 <- 2*(1-pt(abs(tcombsd$t3), df = 15))
tcombsd$p2 <- 2*(1-pt(abs(tcombsd$t2), df = 15))


#is the difference significant for 2,3,4 subsamples?
tcombsd$sig4 <- tcombsd$t4 > qt(.975, df=15)
tcombsd$sig3 <- tcombsd$t3 > qt(.975, df=15)
tcombsd$sig2 <- tcombsd$t2 > qt(.975, df=15)

tcombsd  ###Not sure these are the right results


