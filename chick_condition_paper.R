# 03/08/17 Musanze, Rwanda
# Comparison of chick condition for LHI 2004, 2015 and 2016

rm(list=ls())
library(ggplot2)
library(lubridate)

# set wd for where chick data is stored
setwd("~/grive/phd/analyses/foraging_strategy")

all_ck<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/ck_wgt_tar_LHI_Heron_ALL.csv", h=T, strip.white=T)

all_ck$ColYr<-paste(all_ck$Col, all_ck$Year, sep="_")

qplot(data=all_ck, x=ck_tar, y=ck_weight, colour=ColYr)+geom_smooth(method="lm")

all_ck<-all_ck[all_ck$ck_weight<600,]

# remove chicks only measured in mornings (LHI 2014)
all_ck<-all_ck[-which(all_ck$NestID %in% c("c1", "c2", "c3", "c4", "c5")),]

# remove rows without a weight (will remove some tarsus measures)
all_ck<-all_ck[-which(is.na(all_ck$ck_weight)),]

# ok just some other exploratory graphs, while we have the data
all_ck$Date<-ymd(substr(all_ck$DateTime,1,10))
# add date

png("~/grive/phd/analyses/foraging_strategy/trial_plots/weight_date_all_dat.jpg", width = 12, height =8 , units ="in", res =600)

qplot(data=all_ck, x=Date, y=ck_weight, group=Date, geom="boxplot")+
  facet_wrap(~ColYr, scales="free_x")

dev.off()

# now remove all weights without a concurrent tarsus measure
all_ck<-all_ck[-which(is.na(all_ck$ck_tar)),]

qplot(data=all_ck, x=ck_tar, y=ck_weight, colour=ColYr)+geom_smooth(method="lm")

# some errors remove outliers.
all_ck<-all_ck[all_ck$ck_tar<52,]
all_ck<-all_ck[all_ck$ck_tar>18,]

qplot(data=all_ck, x=ck_tar, y=ck_weight, colour=ColYr)+geom_smooth(method="lm")

png("~/grive/phd/analyses/foraging_strategy/trial_plots/weight_tar_all_dat.jpg", width = 12, height =8 , units ="in", res =600)

qplot(data=all_ck, x=ck_tar, y=ck_weight)+geom_smooth(method="lm")+
      facet_wrap(~ColYr)

dev.off()

all_ck$week<-week(all_ck$Date)

qplot(data=all_ck, x=ck_tar, y=ck_weight, colour=ColYr)+geom_smooth(method="lm")

png("~/grive/phd/analyses/foraging_strategy/trial_plots/tar_date_all_dat.jpg", width = 12, height =8 , units ="in", res =600)

qplot(data=all_ck, x=week, y=ck_tar, colour=ColYr, geom="boxplot")+
  facet_wrap(~week, scales="free_x")

dev.off()




