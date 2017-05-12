library(tidyverse)

rut <- read.csv('../Data/rootmeansv2.csv', header=TRUE)

mod<-lm(beans$bu_acre ~ beans$year)

days<-rut%>%
  mutate(day = ifelse((year == 2008 & block == 1), 304,
                      ifelse((year == 2008 & block == 2), 323,
                             ifelse((year == 2008 & block == 3), 308,
                                    ifelse((year == 2008 & block == 4), 329,
                      ifelse((year == 2009 & block == 1), 313,
                               ifelse((year == 2009 & block == 2), 314,
                                      ifelse((year == 2009 & block %in% c(3,4)), 315,
                      ifelse((year == 2010 & block == 1), 298,
                             ifelse((year == 2010 & block %in% c(2,3)), 300,
                                    ifelse((year == 2010 & block == 4), 301,
                      ifelse((year == 2011 & block %in% c(1,2)), 289,
                             ifelse((year == 2011 & block %in% c(3,4)), 304,
                      ifelse((year == 2012 & block %in% c(1,2)), 289,
                             ifelse((year == 2012 & block %in% c(3,4)), 290,
                      ifelse((year == 2013 & block == 1), 311,
                             ifelse((year == 2013 & block %in% c(2,3)), 312,
                                    ifelse((year == 2013 & block == 4), 315, 400)
                                    )))))))))))))))))%>%
  filter(trt %in% c("CC"))

fit<-group_by(days, depth)
mod<-lm(fit$mass ~ fit$day)

  ggplot(days, aes(x=day, y=mass, color = as.factor(year)))+
  geom_point()+
  #geom_smooth(method='lm')+
  #annotate("text", x=310, y=.6, label = summary(mod)$r.squared, size=5, parse=TRUE)+
  facet_wrap(~depth)