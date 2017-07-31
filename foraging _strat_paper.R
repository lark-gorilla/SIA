# 31/07/17 Musanze, Rwanda
# Production foraging strategy graphs for LHI 2004, 2015 and 2016

rm(list=ls())
library(readxl)
library(ggplot2)
library(lubridate)

# set wd for where attendance data is stored
setwd("~/grive/phd/analyses/foraging_strategy")

# source foragingStrat script
source("~/grive/phd/scripts/WTSH_foraging_strategy/foragingStrat.R")

nest_comp_lhi16<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/LHI_2016_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)
nest_comp_lhi16$Date<-ymd(nest_comp_lhi16$Date)


nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=10,]
nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=19,]
nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=23,]
nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=36,]
nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=40,]
# first stab at removing bad nests, could remove more
# applying the <=25% unknown rule
nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=16,]
D1="2016-02-05"
D2="2016-03-12"

lhi_16<-foragingStrat(nest_comp=nest_comp_lhi16, D1="2016-02-05"
                      ,D2="2016-03-12" , longallow=TRUE, Nruns=1000, remUnknowns=FALSE)

lhi_16RU<-foragingStrat(nest_comp=nest_comp_lhi16, D1="2016-02-05"
                        ,D2="2016-03-12" , longallow=TRUE, Nruns=1, remUnknowns=TRUE)


bs_smry<-rbind(data.frame(tLength=seq(1:25),
                          mean_prop=apply(lhi_16$bs1000_tl[,2:1001],1,mean, na.rm=T),
                          sd_prop=apply(lhi_16$bs1000_tl[,2:1001],1,sd, na.rm=T),
                          mean_raw=apply(lhi_16$bs1000_Rtl[,2:1001],1,mean, na.rm=T),
                          sd_raw=apply(lhi_16$bs1000_Rtl[,2:1001],1,sd, na.rm=T),
                          col_yr="lhi_16"), 
               
               data.frame(tLength=seq(1:25),
                          mean_prop=lhi_16RU$bs1000_tl[,2],
                          sd_prop=0,
                          mean_raw=lhi_16RU$bs1000_Rtl[,2],
                          sd_raw=0,
                          col_yr="lhi_16RU"))

for(i in 2:5){
  bs_smry[which(bs_smry[,i]==0),i]<-NA} #change 0 to NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))

plot(pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
       geom_errorbar(limits, width=0.5)+
       scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
       theme_bw()+ylab("Mean time spent foraging (prop)")+
       xlab("Duration of foraging trip (days)")+ggtitle(paste("Lord Howe", D1, "-", D2))+
       facet_grid(col_yr~.))

limits <- aes(ymax = mean_raw + sd_raw, ymin=mean_raw - sd_raw)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_raw))

plot(pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
       geom_errorbar(limits, width=0.5)+
       scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
       theme_bw()+ylab("Number of trips")+
       xlab("Duration of foraging trip (days)")+ggtitle(paste("Lord Howe", D1, "-", D2))+
       facet_grid(col_yr~.))


nest_comp_lhi15<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/LHI_2015_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)

nest_comp_lhi15$Date<-ymd(nest_comp_lhi15$Date)

nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=48,]
nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=51,]
nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=38,]
nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=14,]
#nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=2,] #extra nests that appear one adult appears to abandon
#nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=6,]
#nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=33,]

D1="2015-02-18"
D2="2015-03-12"

lhi_15<-foragingStrat(nest_comp=nest_comp_lhi15, D1="2015-02-18"
                      ,D2="2015-04-11" , longallow=TRUE, Nruns=1000, remUnknowns=FALSE)

lhi_15RU<-foragingStrat(nest_comp=nest_comp_lhi15, D1="2015-02-18"
                        ,D2="2015-04-11" , longallow=TRUE, Nruns=1, remUnknowns=TRUE)


bs_smry<-rbind(data.frame(tLength=seq(1:25),
                          mean_prop=apply(lhi_15$bs1000_tl[,2:1001],1,mean, na.rm=T),
                          sd_prop=apply(lhi_15$bs1000_tl[,2:1001],1,sd, na.rm=T),
                          mean_raw=apply(lhi_15$bs1000_Rtl[,2:1001],1,mean, na.rm=T),
                          sd_raw=apply(lhi_15$bs1000_Rtl[,2:1001],1,sd, na.rm=T),
                          col_yr="lhi_15"), 
               
               data.frame(tLength=seq(1:25),
                          mean_prop=lhi_15RU$bs1000_tl[,2],
                          sd_prop=0,
                          mean_raw=lhi_15RU$bs1000_Rtl[,2],
                          sd_raw=0,
                          col_yr="lhi_15RU"))

for(i in 2:5){
  bs_smry[which(bs_smry[,i]==0),i]<-NA} #change 0 to NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))

plot(pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
       geom_errorbar(limits, width=0.5)+
       scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
       theme_bw()+ylab("Mean time spent foraging (prop)")+
       xlab("Duration of foraging trip (days)")+ggtitle(paste("Lord Howe", D1, "-", D2))+
       facet_grid(col_yr~.))

limits <- aes(ymax = mean_raw + sd_raw, ymin=mean_raw - sd_raw)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_raw))

plot(pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
       geom_errorbar(limits, width=0.5)+
       scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
       theme_bw()+ylab("Number of trips")+
       xlab("Duration of foraging trip (days)")+ggtitle(paste("Lord Howe",D1, "-", D2))+
       facet_grid(col_yr~.))


nest_comp_lhi04<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/LHI_2004_nest_attendance_cleaned.csv", h=T)
nest_comp_lhi04$Date<-nest_comp_lhi04$Date_hack
nest_comp_lhi04$Date<-ymd(nest_comp_lhi04$Date)

nest_comp_lhi04<-nest_comp_lhi04[nest_comp_lhi04$NestID!="5a",] # remove 25% unknown nest


D1="2004-02-05"
D2="2004-02-28"

lhi_04<-foragingStrat(nest_comp=nest_comp_lhi04, D1="2004-01-31"
                      ,D2="2004-02-28" , longallow=TRUE, Nruns=1000, remUnknowns=FALSE)

lhi_04RU<-foragingStrat(nest_comp=nest_comp_lhi04, D1="2004-01-31"
                        ,D2="2004-02-28" , longallow=TRUE, Nruns=1, remUnknowns=FALSE)


bs_smry<-rbind(data.frame(tLength=seq(1:25),
                          mean_prop=apply(lhi_04$bs1000_tl[,2:1001],1,mean, na.rm=T),
                          sd_prop=apply(lhi_04$bs1000_tl[,2:1001],1,sd, na.rm=T),
                          mean_raw=apply(lhi_04$bs1000_Rtl[,2:1001],1,mean, na.rm=T),
                          sd_raw=apply(lhi_04$bs1000_Rtl[,2:1001],1,sd, na.rm=T),
                          col_yr="lhi_04"), 
               
               data.frame(tLength=seq(1:25),
                          mean_prop=lhi_04RU$bs1000_tl[,2],
                          sd_prop=0,
                          mean_raw=lhi_04RU$bs1000_Rtl[,2],
                          sd_raw=0,
                          col_yr="lhi_04RU"))

for(i in 2:5){
  bs_smry[which(bs_smry[,i]==0),i]<-NA} #change 0 to NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))

plot(pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
       geom_errorbar(limits, width=0.5)+
       scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
       theme_bw()+ylab("Mean time spent foraging (prop)")+
       xlab("Duration of foraging trip (days)")+ggtitle(paste("Lord Howe", D1, "-", D2))+
       facet_grid(col_yr~.))

limits <- aes(ymax = mean_raw + sd_raw, ymin=mean_raw - sd_raw)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_raw))

plot(pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
       geom_errorbar(limits, width=0.5)+
       scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
       theme_bw()+ylab("Number of trips")+
       xlab("Duration of foraging trip (days)")+ggtitle(paste("Lord Howe",D1, "-", D2))+
       facet_grid(col_yr~.))


bs_smry<-rbind(data.frame(tLength=seq(1:25),
                          mean_prop=apply(lhi_04$bs1000_tl[,2:1001],1,mean, na.rm=T),
                          sd_prop=apply(lhi_04$bs1000_tl[,2:1001],1,sd, na.rm=T),
                          mean_raw=apply(lhi_04$bs1000_Rtl[,2:1001],1,mean, na.rm=T),
                          sd_raw=apply(lhi_04$bs1000_Rtl[,2:1001],1,sd, na.rm=T),
                          col_yr="LHI04"), 
               
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(lhi_15$bs1000_tl[,2:1001],1,mean, na.rm=T),
                          sd_prop=apply(lhi_15$bs1000_tl[,2:1001],1,sd, na.rm=T),
                          mean_raw=apply(lhi_15$bs1000_Rtl[,2:1001],1,mean, na.rm=T),
                          sd_raw=apply(lhi_15$bs1000_Rtl[,2:1001],1,sd, na.rm=T),
                          col_yr="LHI15"),
               
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(lhi_16$bs1000_tl[,2:1001],1,mean, na.rm=T),
                          sd_prop=apply(lhi_16$bs1000_tl[,2:1001],1,sd, na.rm=T),
                          mean_raw=apply(lhi_16$bs1000_Rtl[,2:1001],1,mean, na.rm=T),
                          sd_raw=apply(lhi_16$bs1000_Rtl[,2:1001],1,sd, na.rm=T),
                          col_yr="LHI16"))

for(i in 2:5){
  bs_smry[which(bs_smry[,i]==0),i]<-NA} #change 0 to NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Mean time spent foraging (prop)")+
  xlab("Duration of foraging trip (days)")+
  facet_grid(col_yr~.)

#ggsave(paste("LHI16_foraging",D1, D2, "longallow", longallow, "feed_fun_SINGLE.png", sep="_")) #change feed_fun manually!

limits <- aes(ymax = mean_raw + sd_raw, ymin=mean_raw - sd_raw)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_raw))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Number of trips")+
  xlab("Duration of foraging trip (days)")+
  facet_grid(col_yr~.)
