# 30/08/17 Musanze, Rwanda
# GPS data analyses for SIA paper. Assessing feasability of using GPS for foraging 
# strategy inference and then using GPS position data to show movement differences
# between years short trip area and long trip area. 

library(ggplot2)

setwd("~/grive/phd/analyses/SIA")

dat<-read.csv("spreads/GPS_141516_hmm_oceano_attribs.csv", h=T, strip.white=T)

dat$PA<-0
dat[dat$HMMstates!=3,]$PA<-1

dat$ColYr<-paste(dat$Colony, dat$Year, sep="")

qplot(data=dat[dat$trip_type=="S",], y=PA, x=sstOi)+
  geom_smooth(method="gam", formula=y~s(x, k=5))+facet_wrap(~ColYr, scales="free")

g1<-gam(PA~s(sstOi, k=10), data=dat[dat$trip_type=="S" & dat$ColYr=="LHI2014",])



gpssum<-read.csv("~/grive/phd/analyses/SIA/spreads/GPS_141516_trip_summary_attrib_sstchl.csv", h=T)

p<-ggplot(data=gpssum[gpssum$Colony=="LHI",], aes(x=factor(days),
                                                  y=DcolForMN/1000, fill=factor(Year)))+
  geom_boxplot(position=position_dodge(1))    

p+ geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,
                position=position_dodge(1))

# sort non returners

gpssum$days2<-gpssum$days
gpssum[gpssum$days>4,]$days2<-"4+"
gpssum$Returns2<-gpssum$Returns
gpssum[gpssum$Returns=="N" & gpssum$days2=="4+",]$Returns2<-"Y"
# fix to exclude 1-4 day non returners (LTs seen as STs)

# more

gpssum$ColYr<-paste(gpssum$Colony, gpssum$Year, sep="")


p<-ggplot(data=gpssum[gpssum$Returns2=="Y",], aes(x=factor(days2),
          y=DcolForMN/1000, fill=factor(ColYr)))+
  geom_boxplot(position=position_dodge(1))    

p+ geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,
                position=position_dodge(1)) 

# Use classes to check SST

p<-ggplot(data=gpssum[gpssum$Returns2=="Y",], aes(x=factor(days2),
          y=sstMN, fill=factor(ColYr)))+
  geom_boxplot(position=position_dodge(1))    

p+ geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,
                position=position_dodge(1)) 



gpssum$Actual_time2<-gpssum$time
gpssum[gpssum$Returns=="N",]$Actual_time2<-gpssum[gpssum$Returns=="N",]$Actual_time
# NOTE: some NAs, but cant be bothered to check now

p<-ggplot(data=gpssum[gpssum$Colony=="LHI",], aes(x=Actual_time2,
                                                  y=max_dist, colour=factor(Year)))+
  geom_point()+geom_smooth(method="glm", formula=y~poly(x,2))

# This could be a bit of an underestimate of max dist as long trips 
# that didnt reach max dist are given their actual return time (when we got logger back)
# ideally would force through origin

p<-ggplot(data=gpssum, aes(x=Actual_time2,
                           y=max_dist, colour=factor(Year)))+
  geom_point(aes(shape=Colony))+geom_smooth(method="glm", formula=y~poly(x,2), aes(linetype=Colony))



