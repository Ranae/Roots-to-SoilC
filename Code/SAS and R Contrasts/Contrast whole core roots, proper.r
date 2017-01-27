#read data

roots<-read.table("COBS Roots 2008-2013noCNreal08.txt", header = TRUE)

roots$trt<-as.character(roots$trt)

roots$trt[roots$plot == "11"]<-"SC"
roots$trt[roots$plot == "33"]<-"SC"
roots$trt[roots$plot == '26']<-"SC"
roots$trt[roots$plot == '44']<-"SC"
roots$trt[roots$plot == '16']<-"CS"
roots$trt[roots$plot == '34']<-"CS"
roots$trt[roots$plot == '22']<-"CS"
roots$trt[roots$plot == '45']<-"CS"

#####2008#####
roots<-roots[(roots$year == 2008),]

roots$trt<-as.character(roots$trt)

library(plyr)
roots$block.f <- as.factor(roots$block)
roots2 <- ddply(roots, .(year, block.f, trt, core), summarise,
   mass_sum = sum(mass))
   
write.csv(roots2, file="Root biomass, whole core.csv", row.names=FALSE)

lm1 <- lmer(mass_sum ~ block.f+ trt + (1|block.f:trt), data = roots2)
coefs <- fixef(lm1)
varb <- vcov(lm1)

anova(fit.nlme6, L=c(0, 1, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, -1))

##CC vs CCW
anova(lm1, L=c(0,0,0,0,1,0,0,0,0))

c1 <- as.matrix(c(0,0,0,0,1,0,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvccw <- (1-pt(abs(t1), df = 15))*2

##CC vs CS
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvcs <- (1-pt(abs(t1), df = 15))*2

##CC vs P
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvp <- (1-pt(abs(t1), df = 15))*2

##CC vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvpf <- (1-pt(abs(t1), df = 15))*2

##CC vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,0,1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvsc <- (1-pt(abs(t1), df = 15))*2

##CCW vs CS
c1 <- as.matrix(c(0,0,0,0,1,-1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvcs <- (1-pt(abs(t1), df = 15))*2

##CCW vs P
c1 <- as.matrix(c(0,0,0,0,1,0,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvp <- (1-pt(abs(t1), df = 15))*2

##CCW vs PF
c1 <- as.matrix(c(0,0,0,0,1,0,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvpf <- (1-pt(abs(t1), df = 15))*2

##CCW vs SC
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvsc <- (1-pt(abs(t1), df = 15))*2

##CS vs P
c1 <- as.matrix(c(0,0,0,0,0,1,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvp <- (1-pt(abs(t1), df = 15))*2

##CS vs PF
c1 <- as.matrix(c(0,0,0,0,0,1,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvpf <- (1-pt(abs(t1), df = 15))*2

##CS vs SC
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvsc <- (1-pt(abs(t1), df = 15))*2

##P vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,1,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvpf <- (1-pt(abs(t1), df = 15))*2

##P vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvsc <- (1-pt(abs(t1), df = 15))*2

##PF vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pfvsc <- (1-pt(abs(t1), df = 15))*2

ccvccw
ccvcs
ccvp
ccvpf
ccvsc
ccwvcs
ccwvp
ccwvpf
ccwvsc
csvp
csvpf
csvsc
pvpf
pvsc
pfvsc

#####2009#####

roots<-read.table("COBS Roots 2008-2013noCNfake08.txt", header = TRUE)

roots$trt<-as.character(roots$trt)

roots$trt[roots$plot == "11"]<-"SC"
roots$trt[roots$plot == "33"]<-"SC"
roots$trt[roots$plot == '26']<-"SC"
roots$trt[roots$plot == '44']<-"SC"
roots$trt[roots$plot == '16']<-"CS"
roots$trt[roots$plot == '34']<-"CS"
roots$trt[roots$plot == '22']<-"CS"
roots$trt[roots$plot == '45']<-"CS"

roots<-roots[(roots$year == 2009),]

roots$trt<-as.character(roots$trt)

library(plyr)
roots$block.f <- as.factor(roots$block)
roots2 <- ddply(roots, .(block.f, trt, core), summarise,
   mass_sum = sum(mass))

lm1 <- lmer(mass_sum ~ block.f+ trt + (1|block.f:trt), data = roots2)
coefs <- fixef(lm1)
varb <- vcov(lm1)

##CC vs CCW
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvccw <- (1-pt(abs(t1), df = 15))*2

##CC vs CS
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvcs <- (1-pt(abs(t1), df = 15))*2

##CC vs P
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvp <- (1-pt(abs(t1), df = 15))*2

##CC vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvpf <- (1-pt(abs(t1), df = 15))*2

##CC vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,0,1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvsc <- (1-pt(abs(t1), df = 15))*2

##CCW vs CS
c1 <- as.matrix(c(0,0,0,0,1,-1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvcs <- (1-pt(abs(t1), df = 15))*2

##CCW vs P
c1 <- as.matrix(c(0,0,0,0,1,0,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvp <- (1-pt(abs(t1), df = 15))*2

##CCW vs PF
c1 <- as.matrix(c(0,0,0,0,1,0,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvpf <- (1-pt(abs(t1), df = 15))*2

##CCW vs SC
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvsc <- (1-pt(abs(t1), df = 15))*2

##CS vs P
c1 <- as.matrix(c(0,0,0,0,0,1,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvp <- (1-pt(abs(t1), df = 15))*2

##CS vs PF
c1 <- as.matrix(c(0,0,0,0,0,1,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvpf <- (1-pt(abs(t1), df = 15))*2

##CS vs SC
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvsc <- (1-pt(abs(t1), df = 15))*2

##P vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,1,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvpf <- (1-pt(abs(t1), df = 15))*2

##P vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvsc <- (1-pt(abs(t1), df = 15))*2

##PF vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pfvsc <- (1-pt(abs(t1), df = 15))*2

ccvccw
ccvcs
ccvp
ccvpf
ccvsc
ccwvcs
ccwvp
ccwvpf
ccwvsc
csvp
csvpf
csvsc
pvpf
pvsc
pfvsc


#####2010#####

roots<-read.table("COBS Roots 2008-2013noCNfake08.txt", header = TRUE)

roots$trt<-as.character(roots$trt)

roots$trt[roots$plot == "11"]<-"SC"
roots$trt[roots$plot == "33"]<-"SC"
roots$trt[roots$plot == '26']<-"SC"
roots$trt[roots$plot == '44']<-"SC"
roots$trt[roots$plot == '16']<-"CS"
roots$trt[roots$plot == '34']<-"CS"
roots$trt[roots$plot == '22']<-"CS"
roots$trt[roots$plot == '45']<-"CS"

roots<-roots[(roots$year == 2010),]

roots$trt<-as.character(roots$trt)

library(plyr)
roots$block.f <- as.factor(roots$block)
roots2 <- ddply(roots, .(block.f, trt, core), summarise,
   mass_sum = sum(mass))

lm1 <- lmer(mass_sum ~ block.f+ trt + (1|block.f:trt), data = roots2)
coefs <- fixef(lm1)
varb <- vcov(lm1)

##CC vs CCW
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvccw <- (1-pt(abs(t1), df = 15))*2

##CC vs CS
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvcs <- (1-pt(abs(t1), df = 15))*2

##CC vs P
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvp <- (1-pt(abs(t1), df = 15))*2

##CC vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvpf <- (1-pt(abs(t1), df = 15))*2

##CC vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,0,1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvsc <- (1-pt(abs(t1), df = 15))*2

##CCW vs CS
c1 <- as.matrix(c(0,0,0,0,1,-1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvcs <- (1-pt(abs(t1), df = 15))*2

##CCW vs P
c1 <- as.matrix(c(0,0,0,0,1,0,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvp <- (1-pt(abs(t1), df = 15))*2

##CCW vs PF
c1 <- as.matrix(c(0,0,0,0,1,0,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvpf <- (1-pt(abs(t1), df = 15))*2

##CCW vs SC
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvsc <- (1-pt(abs(t1), df = 15))*2

##CS vs P
c1 <- as.matrix(c(0,0,0,0,0,1,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvp <- (1-pt(abs(t1), df = 15))*2

##CS vs PF
c1 <- as.matrix(c(0,0,0,0,0,1,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvpf <- (1-pt(abs(t1), df = 15))*2

##CS vs SC
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvsc <- (1-pt(abs(t1), df = 15))*2

##P vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,1,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvpf <- (1-pt(abs(t1), df = 15))*2

##P vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvsc <- (1-pt(abs(t1), df = 15))*2

##PF vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pfvsc <- (1-pt(abs(t1), df = 15))*2

ccvccw
ccvcs
ccvp
ccvpf
ccvsc
ccwvcs
ccwvp
ccwvpf
ccwvsc
csvp
csvpf
csvsc
pvpf
pvsc
pfvsc


#####2011#####

roots<-read.table("COBS Roots 2008-2013noCNfake08.txt", header = TRUE)

roots$trt<-as.character(roots$trt)

roots$trt[roots$plot == "11"]<-"SC"
roots$trt[roots$plot == "33"]<-"SC"
roots$trt[roots$plot == '26']<-"SC"
roots$trt[roots$plot == '44']<-"SC"
roots$trt[roots$plot == '16']<-"CS"
roots$trt[roots$plot == '34']<-"CS"
roots$trt[roots$plot == '22']<-"CS"
roots$trt[roots$plot == '45']<-"CS"

roots<-roots[(roots$year == 2011),]

roots$trt<-as.character(roots$trt)

library(plyr)
roots$block.f <- as.factor(roots$block)
roots2 <- ddply(roots, .(block.f, trt, core), summarise,
   mass_sum = sum(mass))

lm1 <- lmer(mass_sum ~ block.f+ trt + (1|block.f:trt), data = roots2)
coefs <- fixef(lm1)
varb <- vcov(lm1)

##CC vs CCW
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvccw <- (1-pt(abs(t1), df = 15))*2

##CC vs CS
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvcs <- (1-pt(abs(t1), df = 15))*2

##CC vs P
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvp <- (1-pt(abs(t1), df = 15))*2

##CC vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvpf <- (1-pt(abs(t1), df = 15))*2

##CC vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,0,1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvsc <- (1-pt(abs(t1), df = 15))*2

##CCW vs CS
c1 <- as.matrix(c(0,0,0,0,1,-1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvcs <- (1-pt(abs(t1), df = 15))*2

##CCW vs P
c1 <- as.matrix(c(0,0,0,0,1,0,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvp <- (1-pt(abs(t1), df = 15))*2

##CCW vs PF
c1 <- as.matrix(c(0,0,0,0,1,0,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvpf <- (1-pt(abs(t1), df = 15))*2

##CCW vs SC
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvsc <- (1-pt(abs(t1), df = 15))*2

##CS vs P
c1 <- as.matrix(c(0,0,0,0,0,1,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvp <- (1-pt(abs(t1), df = 15))*2

##CS vs PF
c1 <- as.matrix(c(0,0,0,0,0,1,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvpf <- (1-pt(abs(t1), df = 15))*2

##CS vs SC
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvsc <- (1-pt(abs(t1), df = 15))*2

##P vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,1,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvpf <- (1-pt(abs(t1), df = 15))*2

##P vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvsc <- (1-pt(abs(t1), df = 15))*2

##PF vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pfvsc <- (1-pt(abs(t1), df = 15))*2

ccvccw
ccvcs
ccvp
ccvpf
ccvsc
ccwvcs
ccwvp
ccwvpf
ccwvsc
csvp
csvpf
csvsc
pvpf
pvsc
pfvsc

#####2012#####

roots<-read.table("COBS Roots 2008-2013noCNfake08.txt", header = TRUE)

roots$trt<-as.character(roots$trt)

roots$trt[roots$plot == "11"]<-"SC"
roots$trt[roots$plot == "33"]<-"SC"
roots$trt[roots$plot == '26']<-"SC"
roots$trt[roots$plot == '44']<-"SC"
roots$trt[roots$plot == '16']<-"CS"
roots$trt[roots$plot == '34']<-"CS"
roots$trt[roots$plot == '22']<-"CS"
roots$trt[roots$plot == '45']<-"CS"

roots<-roots[(roots$year == 2012),]

roots$trt<-as.character(roots$trt)

library(plyr)
roots$block.f <- as.factor(roots$block)
roots2 <- ddply(roots, .(block.f, trt, core), summarise,
   mass_sum = sum(mass))

lm1 <- lmer(mass_sum ~ block.f+ trt + (1|block.f:trt), data = roots2)
coefs <- fixef(lm1)
varb <- vcov(lm1)

##CC vs CCW
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvccw <- (1-pt(abs(t1), df = 15))*2

##CC vs CS
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvcs <- (1-pt(abs(t1), df = 15))*2

##CC vs P
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvp <- (1-pt(abs(t1), df = 15))*2

##CC vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvpf <- (1-pt(abs(t1), df = 15))*2

##CC vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,0,1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvsc <- (1-pt(abs(t1), df = 15))*2

##CCW vs CS
c1 <- as.matrix(c(0,0,0,0,1,-1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvcs <- (1-pt(abs(t1), df = 15))*2

##CCW vs P
c1 <- as.matrix(c(0,0,0,0,1,0,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvp <- (1-pt(abs(t1), df = 15))*2

##CCW vs PF
c1 <- as.matrix(c(0,0,0,0,1,0,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvpf <- (1-pt(abs(t1), df = 15))*2

##CCW vs SC
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvsc <- (1-pt(abs(t1), df = 15))*2

##CS vs P
c1 <- as.matrix(c(0,0,0,0,0,1,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvp <- (1-pt(abs(t1), df = 15))*2

##CS vs PF
c1 <- as.matrix(c(0,0,0,0,0,1,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvpf <- (1-pt(abs(t1), df = 15))*2

##CS vs SC
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvsc <- (1-pt(abs(t1), df = 15))*2

##P vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,1,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvpf <- (1-pt(abs(t1), df = 15))*2

##P vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvsc <- (1-pt(abs(t1), df = 15))*2

##PF vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pfvsc <- (1-pt(abs(t1), df = 15))*2

ccvccw
ccvcs
ccvp
ccvpf
ccvsc
ccwvcs
ccwvp
ccwvpf
ccwvsc
csvp
csvpf
csvsc
pvpf
pvsc
pfvsc

#####2013#####

roots<-read.table("COBS Roots 2008-2013noCNfake08.txt", header = TRUE)

roots$trt<-as.character(roots$trt)

roots$trt[roots$plot == "11"]<-"SC"
roots$trt[roots$plot == "33"]<-"SC"
roots$trt[roots$plot == '26']<-"SC"
roots$trt[roots$plot == '44']<-"SC"
roots$trt[roots$plot == '16']<-"CS"
roots$trt[roots$plot == '34']<-"CS"
roots$trt[roots$plot == '22']<-"CS"
roots$trt[roots$plot == '45']<-"CS"

roots<-roots[(roots$year == 2013),]

roots$trt<-as.character(roots$trt)

library(plyr)
roots$block.f <- as.factor(roots$block)
roots2 <- ddply(roots, .(block.f, trt, core), summarise,
   mass_sum = sum(mass))

lm1 <- lmer(mass_sum ~ block.f+ trt + (1|block.f:trt), data = roots2)
coefs <- fixef(lm1)
varb <- vcov(lm1)

##CC vs CCW
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvccw <- (1-pt(abs(t1), df = 15))*2

##CC vs CS
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvcs <- (1-pt(abs(t1), df = 15))*2

##CC vs P
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvp <- (1-pt(abs(t1), df = 15))*2

##CC vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvpf <- (1-pt(abs(t1), df = 15))*2

##CC vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,0,1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccvsc <- (1-pt(abs(t1), df = 15))*2

##CCW vs CS
c1 <- as.matrix(c(0,0,0,0,1,-1,0,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvcs <- (1-pt(abs(t1), df = 15))*2

##CCW vs P
c1 <- as.matrix(c(0,0,0,0,1,0,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvp <- (1-pt(abs(t1), df = 15))*2

##CCW vs PF
c1 <- as.matrix(c(0,0,0,0,1,0,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvpf <- (1-pt(abs(t1), df = 15))*2

##CCW vs SC
c1 <- as.matrix(c(0,0,0,0,1,0,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
ccwvsc <- (1-pt(abs(t1), df = 15))*2

##CS vs P
c1 <- as.matrix(c(0,0,0,0,0,1,-1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvp <- (1-pt(abs(t1), df = 15))*2

##CS vs PF
c1 <- as.matrix(c(0,0,0,0,0,1,0,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvpf <- (1-pt(abs(t1), df = 15))*2

##CS vs SC
c1 <- as.matrix(c(0,0,0,0,0,1,0,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
csvsc <- (1-pt(abs(t1), df = 15))*2

##P vs PF
c1 <- as.matrix(c(0,0,0,0,0,0,1,-1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvpf <- (1-pt(abs(t1), df = 15))*2

##P vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,1,0,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pvsc <- (1-pt(abs(t1), df = 15))*2

##PF vs SC
c1 <- as.matrix(c(0,0,0,0,0,0,0,1,-1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
pfvsc <- (1-pt(abs(t1), df = 15))*2

ccvccw
ccvcs
ccvp
ccvpf
ccvsc
ccwvcs
ccwvp
ccwvpf
ccwvsc
csvp
csvpf
csvsc
pvpf
pvsc
pfvsc