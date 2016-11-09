library(ggplot2)
library(dplyr)
library(tidyr)

depth<-c(10, 20, 40, 60, 80, 100, 10, 20, 40, 60, 80, 100)
soc<-c(26.63, 21.74, 19.23, 14.05, 10.41, 8.52, 17.75, 14.84, 13.18, 11.40, 9.52, 7.12)
rootc<-c(1.393, .433, .223, .157, .115, .247, .200, .059, .034, .031, .030, .009)
trt<-c("G", "G", "G", "G", "G", "G", "C", "C", "C", "C", "C", "C")
df<-as.data.frame(cbind(depth, soc, rootc, trt), stringsAsFactors=FALSE)
df$soc<-as.numeric(df$soc)
df$rootc<-as.numeric(df$rootc)
df$depth<-as.numeric(df$depth)

ggplot(df, aes(x=-depth, y=soc))+
  geom_line()+
  coord_flip()+
  facet_wrap(~trt)

ggplot(df_intpolated, aes(x=-depth, y=root_int, color=trt)) +
  geom_line() +
  geom_point(shape=1) +
  geom_point(aes(y=rootc), size=5, alpha=.3, na.rm=T) +
  theme_bw()+
  coord_flip()+
  facet_wrap(~trt)

df_depths_possible <- expand.grid(
  depth            = seq(from=min(df$depth), max(100), by=10), #Decide resolution here.
  trt              = c("C", "G"),
  stringsAsFactors = FALSE
)

df_intpolated <- df %>% 
  right_join(df_depths_possible, by=c("trt", "depth")) %>% #Incorporate locations to interpolate
  group_by(trt)%>%
  mutate(
    carbon_int     = spline(x=depth, y=soc  , xout=depth,  method="natural")$y,
    root_int       = spline(x=depth, y=rootc  , xout=depth,  method="natural")$y
    ) %>% 
  ungroup()


sumpropsK<-df_intpolated%>%
  group_by(trt)%>%
  mutate(totalrootC = sum(root_int), 
         totalcarbon = sum(carbon_int))%>%
  mutate(proprootC = root_int/totalrootC,
         propcarbon = carbon_int/totalcarbon)

theme_set(yf_theme)
ggplot(sumpropsK, aes(x=-depth, y=proprootC)) +
  geom_line(color="green", size=1.2) +
  geom_line(aes(y=propcarbon), color="brown", size=1.2) +
  coord_flip()+
  facet_wrap(~trt)