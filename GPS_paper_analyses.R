## 11/09/2017, Musanze Rwanda 
## GPS analyses and summaries of data collected in 2015 and 2016

# Summary of foraging strategy from GPS data

gpsdat<-read.csv("~/grive/phd/analyses/SIA/spreads/GPS_141516_trip_summary_attrib_sstchl.csv", h=T)

library(lubridate)
gpsdat$week<-week(gpsdat$stDateTime)

# subset to data only in early chick sampling period

gpsdat<-gpsdat[gpsdat$week<11,]

gpsdat$all_corr_time<-gpsdat$time
gpsdat[gpsdat$Returns=="N",]$all_corr_time<-gpsdat[gpsdat$Returns=="N",]$Actual_time
gpsdat$all_corr_days<-ceiling(gpsdat$all_corr_time/24)

lhi15<-gpsdat[gpsdat$Colony=="LHI" & gpsdat$Year=="2015",]
her15<-gpsdat[gpsdat$Colony=="Heron" & gpsdat$Year=="2015",]
lhi16<-gpsdat[gpsdat$Colony=="LHI" & gpsdat$Year=="2016",]

lhi16<-lhi16[lhi16$all_corr_days<10,] # kill overly long trips, birds did not return

mean(lhi15$all_corr_days);sd(lhi15$all_corr_days)
mean(her15$all_corr_days);sd(her15$all_corr_days)
mean(lhi16$all_corr_days);sd(lhi16$all_corr_days)

sam_dat<-rbind(lhi15, her15, lhi16)
hist(sam_dat$all_corr_days)
sam_dat$ColYr<-paste(sam_dat$Colony, sam_dat$Year)

m1<-glm(all_corr_days~ColYr, data=sam_dat, family=poisson)

sum(resid(m1)^2)/df.residual(m1)

library(lme4)
library(MASS)

sam_dat$birdID<-substr(sam_dat$trip, 1,13)

m2<-glmer(all_corr_days~ColYr+(1|birdID), data=sam_dat, family=poisson)

sum(resid(m2)^2)/df.residual(m2)

m3<-glmer.nb(all_corr_days~ColYr+(1|birdID), data=sam_dat)

sum(resid(m3)^2)/df.residual(m3) # nice

summary(m3)

Anova(m3)

library(lsmeans)
lsmeans(m3, specs="ColYr", type="response")

kmeans(lhi16$all_corr_days, centers=2)
kmeans(her15$all_corr_days, centers=2)
kmeans(lhi15$all_corr_days, centers=2)


# Trip distance summaries

aggregate(DcolForMN~ColYr, sam_dat, mean)

aggregate(DcolForMN~ColYr, sam_dat[sam_dat$all_corr_days<5,], mean)

hist(sam_dat$DcolForMN)

hist(sqrt(sam_dat$DcolForMN))

mf1<-lmer(sqrt(DcolForMN)~ColYr+(1|birdID), data=sam_dat)

plot(mf1) # looks ok

summary(mf1) # zero variance, use LM

mf2<-lm(sqrt(DcolForMN)~ColYr, data=sam_dat)
plot(mf2,1) # hmm


sam_dat$DcolForMN<-as.integer(sam_dat$DcolForMN)

mf3<-glm(DcolForMN~ColYr, data=sam_dat, family=poisson)
plot(mf3,1) # h
plot(mf3,2)

mf4<-glm.nb(DcolForMN~ColYr, data=sam_dat)
plot(mf4,1) # h
plot(mf4,2)
sum(resid(mf4)^2)/df.residual(mf4) # ok

summary(mf4)
anova(mf4)

lsmeans(mf4, specs="ColYr", type="response")

# Split into long and short trips

sam_dat$DcolForMN<-sam_dat$DcolForMN/1000

sam_dat$trip_type<-"S"
sam_dat[sam_dat$all_corr_days>4,]$trip_type<-"L"

#between years again

hist(sam_dat[sam_dat$trip_type=="S" & sam_dat$DcolForMN<500,]$DcolForMN)

# get rid of + 500 km trip

mf1<-lmer(DcolForMN~ColYr+(1|birdID),
          data=sam_dat[sam_dat$trip_type=="S" & sam_dat$DcolForMN<500,])

plot(mf1)
summary(mf1)

lsmeansLT(mf1, specs="ColYr")

anova(mf1)

# Long trips

hist(sam_dat[sam_dat$trip_type=="L",]$DcolForMN)

# no random effect birds only did 1 LT
mf2<-lm(DcolForMN~ColYr,
          data=sam_dat[sam_dat$trip_type=="L",])

plot(mf2, 1)

summary(mf2)

lsmeans(mf2, specs="ColYr")

anova(mf2)

# Within each ColYr testing difference in distance between ST and LT

# Heron

hist(sam_dat[sam_dat$ColYr=="Heron 2015",]$DcolForMN)

hist(sqrt(sam_dat[sam_dat$ColYr=="Heron 2015",]$DcolForMN))

mf1<-lmer(sqrt(DcolForMN)~trip_type+(1|birdID), data=sam_dat[sam_dat$ColYr=="Heron 2015",])
plot(mf1) #ok
summary(mf1)
#preds
l=23.335^2
s=(23.335-13.436)^2
l
s
# slight bodge to get standard errors
mean(c((23.335+1.482)^2-l, l-(23.335-1.482)^2))
mean(c(((23.335-13.436)+1.678)^2-s, s-((23.335-13.436)-1.678)^2))

anova(mf1)

# LHI 15

hist(sam_dat[sam_dat$ColYr=="LHI 2015",]$DcolForMN)

hist(sqrt(sam_dat[sam_dat$ColYr=="LHI 2015",]$DcolForMN))

mf1<-lmer(sqrt(DcolForMN)~trip_type+(1|birdID),
          data=sam_dat[sam_dat$ColYr=="LHI 2015",])
plot(mf1) #ok
summary(mf1)
#preds
l=23.109^2
s=(23.109-10.094)^2
l
s
# slight bodge to get standard errors
mean(c((23.109+2.163)^2-l, l-(23.109-2.163)^2))
mean(c(((23.109-10.094)+1.793)^2-s, s-((23.109-10.094)-1.793)^2))

anova(mf1)

# LHI 16

hist(sam_dat[sam_dat$ColYr=="LHI 2016",]$DcolForMN)

hist(sqrt(sam_dat[sam_dat$ColYr=="LHI 2016",]$DcolForMN))

mf1<-lmer(DcolForMN~trip_type+(1|birdID),
          data=sam_dat[sam_dat$ColYr=="LHI 2016",])
plot(mf1) #good
summary(mf1)
#preds no transformation needed
222.67-86.45 
anova(mf1)

## Now create kernels of ST and LT from each ColYr

#trip_ids we can use
intripsST<-factor(sam_dat[sam_dat$trip_type=="S",]$trip)
intripsLT<-factor(sam_dat[sam_dat$trip_type=="L",]$trip)

setwd("~/grive/phd/analyses/SIA")

dat<-read.csv("spreads/GPS_141516_hmm_oceano_attribs.csv", h=T, strip.white=T)

dat$ColYr<-paste(dat$Colony, dat$Year)
dat$ID<-dat$trip_id

datST<-dat[dat$trip_id %in% intripsST,]
datLT<-dat[dat$trip_id %in% intripsLT,]

datST$ID<-factor(datST$ID)
datLT$ID<-factor(datLT$ID)

# write out data
#write.csv(datST, "spreads/GPS_141516_ST_SIAtimeperiod.csv", quote=F, row.names=F)
#write.csv(datLT, "spreads/GPS_141516_LT_SIAtimeperiod.csv", quote=F, row.names=F)

  
# find h value 
source("~/grive/phd/scripts/MIBA_scripts_revised/scaleARS_revised.r")

Scales=(seq(0, 50, 2))
fpt.scales=scaleARS(DataGroup=datLT ,Scales=Scales, Peak="User")
#ST = 15 km half it!!! for h value
#LT = 31, half it

qplot(data=datST, x=Longitude, y=Latitude, colour=ColYr)

her15ST<-datST[datST$ColYr=="Heron 2015",]
lhi15ST<-datST[datST$ColYr=="LHI 2015",]
lhi16ST<-datST[datST$ColYr=="LHI 2016",]

her15LT<-datLT[datLT$ColYr=="Heron 2015",]
lhi15LT<-datLT[datLT$ColYr=="LHI 2015",]
lhi16LT<-datLT[datLT$ColYr=="LHI 2016",]

daty<-lhi16LT
  
source("~/grive/phd/scripts/github_MIBA/batchUD.R")

for(j in c(99,75,50,25))
{
  #heron_old$ID<-j
  daty$ID<-j
  UD_out<-batchUD(daty,
                  Scale = 15.5, UDLev = j)
  
  if(j==99){all_UDs<-UD_out}else{all_UDs<-spRbind(all_UDs, UD_out)}
  
  plot(all_UDs, border=factor(all_UDs$id))
}

all_UDs <- spTransform(all_UDs, CRS=CRS("+proj=longlat +ellps=WGS84"))
#I updated Lubuntu and now proj is handed correctly so can deal with WGS!! :)

plot(all_UDs, border=factor(all_UDs$id), lwd=2)

setwd("~/grive/phd/analyses/SIA")
writeOGR(all_UDs, layer="lhi16_all_15.5", dsn="spatial", driver="ESRI Shapefile", verbose=TRUE, overwrite=T)



