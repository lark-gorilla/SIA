# 10/05/17 Musanze, Rwanda
# Exploration of Chick and Adult blood and plasma SIA results

rm(list=ls())
setwd("~/grive/phd/analyses/SIA")

dat<-read.csv("spreads/SIA_data.csv", h=T)

library(ggplot2)

p1<-ggplot(data=dat[dat$sampleType=="blood",], aes(x=d13C_VDPB, y=d15N_Air))

#png("exploration/blood_ad_ck.png", width = 6, height =6 , units ="in", res =600)
p1+geom_point(aes(shape=Age, color=ColYr), size=3)+theme_bw()
#dev.off()

#png("exploration/blood_ad_ck_yr.png", width = 9, height =6 , units ="in", res =600)
p1+geom_point(aes(color=Age), size=3)+theme_bw()+facet_wrap(~ColYr)
#dev.off()

#png("exploration/blood_ad_ck_yr_WB_correction.png", width = 9, height =6 , units ="in", res =600)
p1+geom_point(aes(color=Age), size=3)+geom_point(data=dat[dat$sampleType2=="whole_blood",])+
  theme_bw()+facet_wrap(~ColYr)
#dev.off()

p1<-ggplot(data=dat, aes(x=d13C_VDPB, y=d15N_Air))

#png("exploration/blood__ad_ck_yr.png", width = 9, height =6 , units ="in", res =600)
p1+geom_point(aes(color=Age, shape=sampleType), size=3 )+theme_bw()+facet_wrap(~ColYr)
#dev.off()

#png("exploration/blood_plasma_ad_ck_yr.png", width = 9, height =6 , units ="in", res =600)
p1+geom_point(aes(color=Age, shape=sampleType), size=3 )+theme_bw()+facet_wrap(~ColYr)
#dev.off()

#png("exploration/blood_plasma_ad_ck_yrSTLT.png", width = 9, height =6 , units ="in", res =600)
p1+geom_point(aes(color=Ptrip_type, shape=sampleType), size=3 )+theme_bw()+facet_wrap(~ColYr)
#dev.off()

#png("exploration/blood_plasma_ad_ck_yr_all.png", width = 6, height =6 , units ="in", res =600)
p1+geom_point(aes(color=ColYr, shape=sampleType), size=3 )+
  geom_point(data=dat[dat$Age=="CK",], size=1, colour=1 )+theme_bw()
#dev.off()

p1<-ggplot(data=dat[dat$sampleType=="plasma",], aes(x=d13C_VDPB, y=d15N_Air))

#png("exploration/plasma_trip_yr.png", width = 6, height =6 , units ="in", res =600)
p1+geom_point(aes(color=ColYr, shape=Ptrip_type), size=3 )+theme_bw()
#dev.off()

# test if chick age/time has an affect on SIA results

#Heron 2015
qplot(data=dat[dat$sampleType=="blood" & dat$ColYr=="HER15",],
      x=Date, y=d13C_VDPB, colour=Age, geom="point")
anova(lm(d13C_VDPB~Date+Age, data=dat[dat$sampleType=="blood" & dat$ColYr=="HER15",]))
# None
qplot(data=dat[dat$sampleType=="blood" & dat$ColYr=="HER15",],
      x=Date, y=d15N_Air, colour=Age, geom="point")
anova(lm(d15N_Air~Date+Age, data=dat[dat$sampleType=="blood" & dat$ColYr=="HER15",]))
# very slight trend in adults. NS

#LHI 2015
qplot(data=dat[dat$sampleType=="blood" & dat$ColYr=="LHI15",],
      x=Date, y=d13C_VDPB, colour=Age, geom="point")
anova(lm(d13C_VDPB~Date+Age, data=dat[dat$sampleType=="blood" & dat$ColYr=="LHI15",]))
# over very shot time, no real difference
qplot(data=dat[dat$sampleType=="blood" & dat$ColYr=="LHI15",],
      x=Date, y=d15N_Air, colour=Age, geom="point")
anova(lm(d15N_Air~Date+Age, data=dat[dat$sampleType=="blood" & dat$ColYr=="LHI15",]))
# over very shot time, no real difference

#LHI 2016
qplot(data=dat[dat$sampleType=="blood" & dat$ColYr=="LHI16",],
      x=Date, y=d13C_VDPB, colour=Age, geom="point")
anova(lm(d13C_VDPB~Date+Age, data=dat[dat$sampleType=="blood" & dat$ColYr=="LHI16",]))
# no difference
qplot(data=dat[dat$sampleType=="blood" & dat$ColYr=="LHI16",],
      x=Date, y=d15N_Air, colour=Age, geom="point")
anova(lm(d15N_Air~Date+Age, data=dat[dat$sampleType=="blood" & dat$ColYr=="LHI16",]))
# appears significant, test chicks seperately
qplot(data=dat[dat$sampleType2=="blood_cells" & dat$Age=="CK" & dat$ColYr=="LHI16",],
      x=Date, y=d15N_Air, colour=Age, geom="point")
anova(lm(d15N_Air~Date, data=dat[dat$sampleType2=="blood_cells" & dat$Age=="CK" & dat$ColYr=="LHI16",]))
# Nope
qplot(data=dat[dat$sampleType2=="whole_blood" & dat$Age=="CK" & dat$ColYr=="LHI16",],
      x=Date, y=d15N_Air, colour=Age, geom="point")
anova(lm(d15N_Air~Date, data=dat[dat$sampleType2=="whole_blood" & dat$Age=="CK" & dat$ColYr=="LHI16",]))
# Nope

# paired t-test to make sure no difference between whole and blood_cells
# using 2016 chick data - make sure to choose one for stats!
library(reshape2)
wbID<-order(dat[dat$sampleType2=="whole_blood" & dat$Age=="CK" &
                  dat$ColYr=="LHI16",]$birdID)
bcID<-order(dat[dat$sampleType2=="blood_cells" & dat$Age=="CK" &
                  dat$ColYr=="LHI16",]$birdID)

dfwb<-data.frame(birdID=dat[dat$sampleType2=="whole_blood" & dat$Age=="CK" &
      dat$ColYr=="LHI16",]$birdID[wbID], 
      d13C_VDPB=dat[dat$sampleType2=="whole_blood" & dat$Age=="CK" &
            dat$ColYr=="LHI16",]$d13C_VDPB[wbID],
      d15N_Air=dat[dat$sampleType2=="whole_blood" & dat$Age=="CK" &
            dat$ColYr=="LHI16",]$d15N_Air[wbID])

dfbc<-data.frame(bc_birdID=dat[dat$sampleType2=="blood_cells" & dat$Age=="CK" &
                              dat$ColYr=="LHI16",]$birdID[bcID], 
                 bc_d13C_VDPB=dat[dat$sampleType2=="blood_cells" & dat$Age=="CK" &
                                 dat$ColYr=="LHI16",]$d13C_VDPB[bcID],
                 bc_d15N_Air=dat[dat$sampleType2=="blood_cells" & dat$Age=="CK" &
                                dat$ColYr=="LHI16",]$d15N_Air[bcID])

bl_tt<-cbind(dfwb, dfbc[1:7,])
# have a look
me1<-melt(bl_tt)
qplot(data=me1[me1$variable=="d13C_VDPB" |me1$variable=="bc_d13C_VDPB",], x=variable, y=value, geom="boxplot")
# blood cells higher carbon
qplot(data=me1[me1$variable=="d15N_Air" |me1$variable=="bc_d15N_Air",], x=variable, y=value, geom="boxplot")
# no difference

t.test(bl_tt$d13C_VDPB, 
       bl_tt$bc_d13C_VDPB, 
       paired=TRUE, 
       conf.level=0.95)
# v sig -0.38 diff

t.test(bl_tt$d15N_Air, 
       bl_tt$bc_d15N_Air, 
       paired=TRUE, 
       conf.level=0.95)


# attribute iso sampled gps tracks with plasma results :)

gpsdat<-read.csv("~/grive/phd/analyses/tracking_data_pot/GPS_141516_clean_resamp_tripsplit_hmm_attribs.csv", h=T)

gpsdat<-gpsdat[gpsdat$TrackID %in% dat[dat$sampleType=="plasma",]$birdID,]

plot(Latitude~Longitude, gpsdat, pch=16, cex=0.5, col=TrackID)

d1<-merge(gpsdat, dat[dat$sampleType=="plasma",], by.x="TrackID", by.y="birdID")

#write for GIS

write.csv(d1, "spreads/SIA_gps_trips_plasma_attrib.csv", quote=F, row.names=F)

# work out core areas in each year. Use different approaches.

gpsdat<-read.csv("~/grive/phd/analyses/tracking_data_pot/GPS_141516_clean_resamp_tripsplit_hmm_attribs.csv", h=T)

# work out scale of interraction
source("~/grive/phd/scripts/MIBA_scripts_revised/scaleARS_revised.r")

gpsdat$ID<-gpsdat$trip_id
# work around for ID factors in fpt.scales
seldat<-gpsdat[gpsdat$Colony=="Heron" & gpsdat$Year=="2015",]
seldat$ID<-factor(seldat$ID)

Scales=(c(seq(1:20),25,30,35,40))
fpt.scales=scaleARS(DataGroup=seldat, Scales=Scales,Peak="Flexible")
#LHI 2014 = 17.4
#LHI 2015 = 13.7
#LHI 2016 = 15.2
#Heron 2014 (ST only!) = 7.6
#Heron 2015 = 15
#mean(c(17.4,13.7,15.2,15)) = 15.3, call it 15
# should divide by 2!! = 7.5
# For all points we do 15 km for forage only we do 7.5 km

# can edit batchUD to up grid cell resolution to make prettier
source("~/grive/phd/scripts/github_MIBA/batchUD.R")

for(j in c(99,75,50,25))
{
  #heron_old$ID<-j
  gpsdat$ID<-j
  UD_out<-batchUD(gpsdat[gpsdat$Colony=="LHI" & gpsdat$Year=="2014" &
                           gpsdat$HMMstates==2,],
                  Scale = 7.5, UDLev = j)
  
  #UD_out<-batchUD(datfull[datfull$Y_assign=="2016" & datfull$dive==1,],
  #Scale = 5, UDLev = j)
  
  if(j==99){all_UDs<-UD_out}else{all_UDs<-spRbind(all_UDs, UD_out)}
  
  plot(all_UDs, border=factor(all_UDs$id))
}

all_UDs <- spTransform(all_UDs, CRS=CRS("+proj=longlat +ellps=WGS84"))
#I updated Lubuntu and now proj is handed correctly so can deal with WGS!! :)

setwd("~/grive/phd/analyses/SIA")
writeOGR(all_UDs, layer="for_7.5km_LHI2014", dsn="spatial", driver="ESRI Shapefile", verbose=TRUE, overwrite=T)

## visualise SST data

library(rasterVis)
library(animation)
library(gridExtra)
library(rgdal)
library(ncdf4)
setwd("~/grive/phd/sourced_data/env_data/erdap_hires")

data.nc<- nc_open("sst_ncdcOisst2Agg.nc")
Zdim = ncvar_get(data.nc,varid="Date")
r2014<-grep("2014", Zdim)
r2015<-grep("2015", Zdim)
r2016<-grep("2016", Zdim)

lhi16<-readOGR(dsn="/home/mark/grive/phd/analyses/SIA/spatial/allpts_LHI2016.shp", layer="allpts_LHI2016")
lhi15<-readOGR(dsn="/home/mark/grive/phd/analyses/SIA/spatial/allpts_LHI2015.shp", layer="allpts_LHI2015")
lhi14<-readOGR(dsn="/home/mark/grive/phd/analyses/SIA/spatial/allpts_LHI2014.shp", layer="allpts_LHI2014")

counter<-1
while(counter<84){
 
  r1<-NULL
  r2<-NULL
  r3<-NULL
  
    counterb<-counter+6
  
    for(i in counter:counterb)
    if(which(i==counter:counterb)==1){
      r1<-raster("sst_ncdcOisst2Agg.nc", band=r2014[i])}else{
        r1<-r1+raster("sst_ncdcOisst2Agg.nc", band=r2014[i])}
    writeRaster(r1/7, paste("~/grive/phd/analyses/SIA/spatial/oceano_weekly_ave/SST2014_",counter, "_", counterb, ".tif", sep=""), overwrite=T)
    
    for(i in counter:counterb)    
    if(which(i==counter:counterb)==1){
      r2<-raster("sst_ncdcOisst2Agg.nc", band=r2015[i])}else{
        r2<-r2+raster("sst_ncdcOisst2Agg.nc", band=r2015[i])}
    writeRaster(r2/7, paste("~/grive/phd/analyses/SIA/spatial/oceano_weekly_ave/SST2015_",counter, "_", counterb, ".tif", sep=""), overwrite=T)
    
    for(i in counter:counterb)
    if(which(i==counter:counterb)==1){
      r3<-raster("sst_ncdcOisst2Agg.nc", band=r2016[i])}else{
        r3<-r3+raster("sst_ncdcOisst2Agg.nc", band=r2016[i])}
    writeRaster(r3/7, paste("~/grive/phd/analyses/SIA/spatial/oceano_weekly_ave/SST2016_",counter, "_", counterb, ".tif", sep=""), overwrite=T)
    
    counter<-counterb+1
    }
 

# write out monthly averages
# replace num and years of bands and write. Rerun as necessary
# do all seasons change write and number divided by
#1:28, 29:57, 58:86

num=1:28
r1<-NULL
for(i in num)
  if(which(i==num)==1){
    r1<-raster("sst_ncdcOisst2Agg.nc", band=r2016[i])}else{
      r1<-r1+raster("sst_ncdcOisst2Agg.nc", band=r2016[i])}
writeRaster(r1/28, paste("~/grive/phd/analyses/SIA/spatial/oceano_monthly_ave/SST2016_",num[1], "_", num[28], ".tif", sep=""), overwrite=T)

# already have chl as a monthly product downloaded

# Have a look at SST and CHL values within ranges of foragers
gpssum<-read.csv("~/grive/phd/analyses/tracking_data_pot/GPS_141516_trip_summary_attrib.csv", h=T)

max(gpssum[gpssum$Colony=="LHI" & gpssum$days==4 & gpssum$Returns=="Y",]$max_dist)
# 1 day 210 km
# 2 day 318 km
# 3 day 325 km, just say the same
# 4 day 658 km

LHI<-SpatialPoints(data.frame(long=159.0602, lat=-31.5249), proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

DgProj <- CRS(paste("+proj=laea +lon_0=", LHI@coords[1], " +lat_0=", LHI@coords[2], sep=""))
LHI_proj <- spTransform(LHI, CRS=DgProj)

library(rgeos)
library(maptools)
for ( i in c(658,325,210))
{
  
  TBuffProj37 <- gBuffer(LHI_proj, width=(i*1000), quadsegs=50)
  TBuffProj37@polygons[[1]]@ID <- as.character(i)
  
  b1Wgs <- spTransform(TBuffProj37, CRS=CRS( "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  if(i==658){out<-b1Wgs}else{out<-spRbind(out, b1Wgs)}
  plot(out)
  print(i)
}

# splitting out in half!
b_poly <- as(extent(c(130, 159.0602, -20, -40)), "SpatialPolygons")
projection(b_poly)<-CRS( "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
out<-gIntersection(out, b_poly, byid = T)

library(raster)
library(ncdf4)
setwd("~/grive/phd/sourced_data/env_data/erdap_hires")

data.nc<- nc_open("chl_erdVH3chlamday.nc")
ncvar_get(data.nc,varid="Date")

chlvh_stack<-stack("chl_erdVH3chlamday.nc")
chlmh_stack<-stack("chl_erdMH1chlamday.nc")


chlvh_ext<-extract(chlvh_stack, out)
chlmh_ext<-extract(chlmh_stack, out)

library(reshape2)

d1<-rbind(
  data.frame(melt((chlvh_ext)[[1]]), rad=658),
  data.frame(melt((chlvh_ext)[[2]]), rad=325),
  data.frame(melt((chlvh_ext)[[3]]), rad=210))

d2<-rbind(
  data.frame(melt((chlmh_ext)[[1]]), rad=658),
  data.frame(melt((chlmh_ext)[[2]]), rad=325),
  data.frame(melt((chlmh_ext)[[3]]), rad=210)) # chlmh_ext doesnt have May

library(ggplot2)

d1$Var3<-paste(substr(d1$Var2, 8,9), substr(d1$Var2, 6,7), substr(d1$Var2, 2,5),sep="_")
d1$Var3<-factor(d1$Var3)
d1$Var4<-factor(substr(d1$Var2, 2,5))

#png("~/grive/phd/analyses/SIA/exploration/chl_lhi_rad_half.png", width =12 , height =4 , units ="in", res =600)
qplot(data=d1[d1$value<0.25,], x=factor(rad), y=value, colour=Var3, geom="boxplot")
#dev.off()

#png("~/grive/phd/analyses/SIA/exploration/chl_lhi_rad_half_notched.png", width =12 , height =4 , units ="in", res =600)
ggplot(data=d1[d1$value<0.25, ], aes(x=factor(rad), y=value,
                                              colour=Var3))+geom_boxplot(notch=T)
#dev.off()

#png("~/grive/phd/analyses/SIA/exploration/sst_lhi_rad_half_seasonave.png", width =9 , height =3 , units ="in", res =600)
qplot(data=d1[d1$value<0.25, ], x=factor(rad), y=value, colour=Var4, geom="boxplot")
#dev.off()
# ok maybe warmer plume and general warmer water dictates breeding


# try sst

setwd("~/grive/phd/analyses/SIA/spatial/oceano_monthly_ave")
l1<-list.files()
l1<-l1[-grep("aux", l1)]

for(i in l1){
  if(which(i==l1)==1){r1<-raster(paste("~/grive/phd/analyses/SIA/spatial/oceano_monthly_ave/", i, sep=""))}else{
    r1<-stack(r1, raster(paste("~/grive/phd/analyses/SIA/spatial/oceano_monthly_ave/", i, sep="")))}
  print(i)}

sst_ext<-extract(r1, out)

library(reshape2)

d1<-rbind(
  data.frame(melt((sst_ext)[[1]]), rad=658),
  data.frame(melt((sst_ext)[[2]]), rad=325),
  data.frame(melt((sst_ext)[[3]]), rad=210))

d1$Var3<-paste(substr(d1$Var2, 9, nchar(as.character(d1$Var2))), substr(d1$Var2, 4,7), sep="_")
d1$Var3<-factor(d1$Var3)
d1$Var4<-factor(substr(d1$Var2, 4,7))
#png("~/grive/phd/analyses/SIA/exploration/sst_lhi_rad_half.png", width =12 , height =4 , units ="in", res =600)
qplot(data=d1[d1$Var3!="1_86_2014" & d1$Var3!="1_86_2015" & d1$Var3!="1_86_2016", ], x=factor(rad), y=value, colour=Var3, geom="boxplot")
#dev.off()

#png("~/grive/phd/analyses/SIA/exploration/sst_lhi_rad_half_notched.png", width =12 , height =4 , units ="in", res =600)
ggplot(data=d1[d1$Var3!="1_86_2014" & d1$Var3!="1_86_2015" &
                 d1$Var3!="1_86_2016", ], aes(x=factor(rad), y=value,
       colour=Var3))+geom_boxplot(notch=T)
#dev.off()


#png("~/grive/phd/analyses/SIA/exploration/sst_lhi_rad_half_zoom.png", width =12 , height =4 , units ="in", res =600)
qplot(data=d1[d1$Var3!="1_86_2014" & d1$Var3!="1_86_2015" & d1$Var3!="1_86_2016" & d1$rad!=658, ], x=factor(rad), y=value, colour=Var3, geom="boxplot")
#dev.off()

#png("~/grive/phd/analyses/SIA/exploration/sst_lhi_rad_half_seasonave.png", width =9 , height =3 , units ="in", res =600)
qplot(data=d1[d1$Var3!="1_86_2014" & d1$Var3!="1_86_2015" & d1$Var3!="1_86_2016", ], x=factor(rad), y=value, colour=Var4, geom="boxplot")
#dev.off()
# ok maybe warmer plume and general warmer water dictates breeding

## pull in GPS attributed data for comparisons

setwd("~/grive/phd/analyses/SIA")

dat<-read.csv("spreads/GPS_141516_hmm_oceano_attribs.csv", h=T, strip.white=T)

# sstOi is the better dataset (worse resolution but no NAs)
qplot(data=dat[dat$Colony=="LHI"& dat$HMMstates==2,], x=factor(days), y=sstOi, colour=factor(Year), geom="boxplot")
# ok but day classification is not accurate for non returners
qplot(data=dat[dat$Colony=="LHI"& dat$HMMstates==2 & dat$Returns!="N",], x=factor(days), y=sstOi, colour=factor(Year), geom="boxplot")
#better but a little restrictive
qplot(data=dat[dat$Colony=="LHI"& dat$HMMstates==2,], x=ColDist, y=sstOi, colour=factor(Year), geom="point")+geom_smooth()
#better can see trend, now try without just foraging, giev broad pic
qplot(data=dat[dat$Colony=="LHI",], x=ColDist, y=sstOi, colour=factor(Year), geom="point")+geom_smooth()
# ok so they travel out from the colony and encounter warmer sst at different dist

#whats the available SST travelled over by birds
ggplot(data=dat[dat$Colony=="LHI",],
       aes(y=sstOi, x=factor(Year)))+geom_boxplot()
# 2015 and 2016 the same BUT only cos 2015 birds went north
ggplot(data=dat[dat$Colony=="LHI",],
       aes(x=sstOi, fill=factor(Year)))+
  geom_histogram(position="dodge")
# see bimodal peak in 2015 dragging mean SST up. 2016 actually warmest
# but this is all points, what about foraging
dat$FT<-"trans"
dat[dat$HMMstates!=3,]$FT<-"forrest"

#png("~/grive/phd/analyses/SIA/exploration/sst_lhi_foragepts.png", width =6 , height =9 , units ="in", res =600)
ggplot(data=dat[dat$Colony=="LHI",],
       aes(x=sstOi))+
geom_density(aes(x=sstOi, (..scaled..), fill=factor(FT)),
                  position="identity",alpha=0.5)+
  scale_x_continuous(breaks=seq(22.5, 27.5, 0.5), limits=c(22.5,27.5))+
  facet_grid(Year~.)
#dev.off()
#ok cool

dat$binnedsstOi<-cut(dat$sstOi, breaks=seq(22, 30, 1),
                labels=c(as.character(seq(22, 28, 1)), '29+'))

library(gridExtra)
p1<-ggplot(data=dat[dat$Colony=="LHI" & dat$HMMstates==2,],
       aes(x=sstOi, y=Latitude))+
  scale_x_continuous(breaks=seq(22.5, 27.5, 0.5), limits=c(22.5,27.5))+
  scale_y_continuous(breaks=seq(-34, -20, 1), limits=c(-34,-20))+
  geom_point(aes(colour=factor(Year)),size=0.5)+
  geom_smooth(aes(linetype=factor(Year)), colour=1)+geom_hline(yintercept=-31.5249)

p1b<-ggplot(data=dat[dat$Colony=="LHI" & dat$HMMstates==2,],
           aes(x=binnedsstOi, y=Latitude, colour=factor(Year)))+
  scale_y_continuous(breaks=seq(-34, -20, 1), limits=c(-34,-20))+
  geom_boxplot(notch=T)+geom_hline(yintercept=-31.5249)


p2<-ggplot(data=dat[dat$Colony=="LHI" & dat$HMMstates==2,],
           aes(x=sstOi, y=ColDist/1000))+
  scale_x_continuous(breaks=seq(22.5, 27.5, 0.5), limits=c(22.5,27.5))+
  scale_y_continuous(breaks=seq(0, 1200, 200), limits=c(0,1200))+
  geom_point(aes(colour=factor(Year)),size=0.5)+
  geom_smooth(aes(linetype=factor(Year)), colour=1)

p2b<-ggplot(data=dat[dat$Colony=="LHI" & dat$HMMstates==2,],
           aes(x=binnedsstOi, y=ColDist/1000, colour=factor(Year)))+
  geom_boxplot(notch=T)


p3<-ggplot(data=dat[dat$Colony=="LHI"& dat$HMMstates==2,],
       aes(x=sstOi))+
  geom_density(aes(x=sstOi, (..scaled..), fill=factor(Year)),
               position="identity",alpha=0.5)+
  scale_x_continuous(breaks=seq(22.5, 27.5, 0.5), limits=c(22.5,27.5))

png("~/grive/phd/analyses/SIA/exploration/sst_lhi_foragepts_metrics_500Dcol.png", width =12 , height =12 , units ="in", res =600)
grid.arrange(p1,p2,p3 ,ncol=1,nrow=3)
dev.off()

# Lets get some summary tables together

gpssum<-read.csv("~/grive/phd/analyses/tracking_data_pot/GPS_141516_trip_summary_attrib.csv", h=T)

gpssum$DcolForMN<-0
gpssum$DcolForSD<-0
gpssum$sstMN<-0
gpssum$sstSD<-0
gpssum$chlMN<-0
gpssum$chlSD<-0

for(i in unique(gpssum$trip))
{
  gpssum[gpssum$trip==i,]$DcolForMN<-mean(dat[dat$trip_id==i & dat$HMMstates==2,]$ColDist, na.rm=T)
  gpssum[gpssum$trip==i,]$DcolForSD<-sd(dat[dat$trip_id==i & dat$HMMstates==2,]$ColDist, na.rm=T)
  gpssum[gpssum$trip==i,]$sstMN<-mean(dat[dat$trip_id==i,]$sstAG, na.rm=T)
  gpssum[gpssum$trip==i,]$sstSD<-mean(dat[dat$trip_id==i,]$sstAG, na.rm=T)
  gpssum[gpssum$trip==i,]$chlMN<-mean(dat[dat$trip_id==i,]$chlVH3_M, na.rm=T)
  gpssum[gpssum$trip==i,]$chlSD<-mean(dat[dat$trip_id==i,]$chlVH3_M, na.rm=T)
print(i)
  }

write.csv(gpssum, "~/grive/phd/analyses/SIA/spreads/GPS_141516_trip_summary_attrib_sstchl.csv", quote=F, row.names=F)

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

 p<-ggplot(data=gpssum[gpssum$Colony=="LHI" & gpssum$Returns2=="Y",], aes(x=factor(days2),
    y=DcolForMN/1000, fill=factor(Year)))+
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

 # Do birds follow water masses?
 
 setwd("~/grive/phd/analyses/SIA")
 
 dat<-read.csv("spreads/GPS_LT_141516_hmm_oceano_attribs.csv", h=T, strip.white=T)
 
 lhi14<-dat[dat$Colony=="LHI" & dat$Year=="2014",]
 lhi15<-dat[dat$Colony=="LHI" & dat$Year=="2015",]
 lhi16<-dat[dat$Colony=="LHI" & dat$Year=="2016",]
 
 lhi14$Date<-factor(lhi14$Date)
 lhi15$Date<-factor(lhi15$Date)
 lhi16$Date<-factor(lhi16$Date)
 
 
 setwd("~/grive/phd/analyses/SIA/spatial/oceano_weekly_ave")
 l1<-list.files()
 l1<-l1[-grep("aux", l1)]
 
 for(i in l1){
   if(which(i==l1)==1){r1<-raster(paste("~/grive/phd/analyses/SIA/spatial/oceano_weekly_ave/", i, sep=""))}else{
     r1<-stack(r1, raster(paste("~/grive/phd/analyses/SIA/spatial/oceano_weekly_ave/", i, sep="")))}
   print(i)}
 

 source("~/grive/phd/scripts/github_MIBA/batchUD.R")
 
# 2014
 for(j in c(95,50)) # using 95 and 50%
 {
   lhi14$ID<-j
   UD_out<-batchUD(lhi14[lhi14$Date %in% levels(lhi14$Date)[11:19],],
                   Scale = 15, UDLev = j)
   if(j==95){all_UDs<-UD_out}else{all_UDs<-spRbind(all_UDs, UD_out)}
   plot(all_UDs, border=factor(all_UDs$id))
 }
 
 all_UDs <- spTransform(all_UDs, CRS=CRS("+proj=longlat +ellps=WGS84"))
 #
 ud1<-all_UDs
 ud2<-all_UDs
 ud3<-all_UDs
 
 names(r1)
 
 # 29_35, 1-7 March
 # 36-42, 8-14 March
 # 43_49, 15-21 March
 # 50_56, 22-28 March
 # 57_63, 1-7 April
 # 64_70, 8-14 April
 # 71_77, 15-21 April
 # 78_84, 22-28 April
 
 levels(lhi14$Date)

 library(rasterVis)
 library(gridExtra)
 
 colr <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))
 
 # xlim=c(145, 165), ylim=c(-40, -20)
 lhi<-SpatialPoints(cbind(159.0602,-31.5249))
 fop<-SpatialPoints(cbind(lhi14[lhi14$HMMstates==2,]$Longitude,
                          lhi14[lhi14$HMMstates==2,]$Latitude))
 
m1<-levelplot(raster(r1, layer="SST2014_29_35"),
              col.regions=colr, margin=F,at=16:28, xlab=NULL, ylab=NULL, xlim=c(145, 165), ylim=c(-40, -20))+
            layer(sp.points(lhi, size=2))+layer(sp.lines(ud1[1,], col="grey", lwd=2))+
  layer(sp.lines(ud1[2,], col="brown", lwd=2))
  
m2<-levelplot(raster(r1, layer="SST2014_36_42"),
              col.regions=colr, margin=F,at=16:28, xlab=NULL, ylab=NULL, xlim=c(145, 165), ylim=c(-40, -20))+
              layer(sp.points(lhi, size=2))+layer(sp.lines(ud2[1,], col="grey", lwd=2))+
  layer(sp.lines(ud2[2,], col="brown", lwd=2))
m3<-levelplot(raster(r1, layer="SST2014_43_49"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
              layer(sp.points(lhi, size=2))+layer(sp.lines(ud3[1,], col="grey", lwd=2))+
  layer(sp.lines(ud3[2,], col="brown", lwd=2))

jpeg("~/grive/phd/analyses/SIA/exploration/sst_gps_2014.jpg", width =11.69 , height =8.27 , units ="in", res =300)
grid.arrange(m1,m2,m3, nrow=1, ncol=3) 
dev.off()

# 2015 
 
for(j in c(95,50)) # using 95 and 50%
{
  lhi15$ID<-j
  UD_out<-batchUD(lhi15[lhi15$Date %in% levels(lhi15$Date)[47:53],],
                  Scale = 15, UDLev = j)
  if(j==95){all_UDs<-UD_out}else{all_UDs<-spRbind(all_UDs, UD_out)}
  plot(all_UDs, border=factor(all_UDs$id))
}

all_UDs <- spTransform(all_UDs, CRS=CRS("+proj=longlat +ellps=WGS84"))
#
ud1<-all_UDs
ud2<-all_UDs
ud3<-all_UDs
ud4<-all_UDs
ud5<-all_UDs
ud6<-all_UDs
ud7<-all_UDs
ud8<-all_UDs

names(r1)

# 29_35, 1-7 March
# 36-42, 8-14 March
# 43_49, 15-21 March
# 50_56, 22-28 March
# 57_63, 1-7 April
# 64_70, 8-14 April
# 71_77, 15-21 April
# 78_84, 22-28 April

levels(lhi15$Date)

library(rasterVis)
library(gridExtra)

colr <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))

# xlim=c(145, 165), ylim=c(-40, -20)
lhi<-SpatialPoints(cbind(159.0602,-31.5249))
fop<-SpatialPoints(cbind(lhi14[lhi14$HMMstates==2,]$Longitude,
                         lhi14[lhi14$HMMstates==2,]$Latitude))

m1<-levelplot(raster(r1, layer="SST2015_15_21"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL, xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud1[1,], col="grey", lwd=2))+
  layer(sp.lines(ud1[2,], col="brown", lwd=2))

m2<-levelplot(raster(r1, layer="SST2015_22_28"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud2[1,], col="grey", lwd=2))+
  layer(sp.lines(ud2[2,], col="brown", lwd=2))
m3<-levelplot(raster(r1, layer="SST2015_29_35"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud3[1,], col="grey", lwd=2))+
  layer(sp.lines(ud3[2,], col="brown", lwd=2))
m4<-levelplot(raster(r1, layer="SST2015_36_42"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud4[1,], col="grey", lwd=2))+
  layer(sp.lines(ud4[2,], col="brown", lwd=2))

m5<-levelplot(raster(r1, layer="SST2015_43_49"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud5[1,], col="grey", lwd=2))+
  layer(sp.lines(ud5[2,], col="brown", lwd=2))
m6<-levelplot(raster(r1, layer="SST2015_50_56"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud6[1,], col="grey", lwd=2))+
  layer(sp.lines(ud6[2,], col="brown", lwd=2))
m7<-levelplot(raster(r1, layer="SST2015_57_63"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud7[1,], col="grey", lwd=2))+
  layer(sp.lines(ud7[2,], col="brown", lwd=2))
m8<-levelplot(raster(r1, layer="SST2015_64_70"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud8[1,], col="grey", lwd=2))+
  layer(sp.lines(ud8[2,], col="brown", lwd=2))

jpeg("~/grive/phd/analyses/SIA/exploration/sst_gps_2015.jpg", width =16 , height =8 , units ="in", res =300)
grid.arrange(m1,m2,m3,m4,m5,m6,m7,m8, nrow=2, ncol=4) 
dev.off()


# 2016
# 2015 

for(j in c(95,50)) # using 95 and 50%
{
  lhi16$ID<-j
  UD_out<-batchUD(lhi16[lhi16$Date %in% levels(lhi16$Date)[31:36],],
                  Scale = 15, UDLev = j)
  if(j==95){all_UDs<-UD_out}else{all_UDs<-spRbind(all_UDs, UD_out)}
  plot(all_UDs, border=factor(all_UDs$id))
}

all_UDs <- spTransform(all_UDs, CRS=CRS("+proj=longlat +ellps=WGS84"))
#
ud1<-all_UDs
ud2<-all_UDs
ud3<-all_UDs
ud4<-all_UDs
ud5<-all_UDs

names(r1)

# 29_35, 1-7 March
# 36-42, 8-14 March
# 43_49, 15-21 March
# 50_56, 22-28 March
# 57_63, 1-7 April
# 64_70, 8-14 April
# 71_77, 15-21 April
# 78_84, 22-28 April

levels(lhi15$Date)

library(rasterVis)
library(gridExtra)

colr <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))

# xlim=c(145, 165), ylim=c(-40, -20)
lhi<-SpatialPoints(cbind(159.0602,-31.5249))
fop<-SpatialPoints(cbind(lhi14[lhi14$HMMstates==2,]$Longitude,
                         lhi14[lhi14$HMMstates==2,]$Latitude))

m1<-levelplot(raster(r1, layer="SST2016_8_14"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL, xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud1[1,], col="grey", lwd=2))+
  layer(sp.lines(ud1[2,], col="brown", lwd=2))

m2<-levelplot(raster(r1, layer="SST2016_15_21"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud2[1,], col="grey", lwd=2))+
  layer(sp.lines(ud2[2,], col="brown", lwd=2))
m3<-levelplot(raster(r1, layer="SST2016_22_28"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud3[1,], col="grey", lwd=2))+
  layer(sp.lines(ud3[2,], col="brown", lwd=2))
m4<-levelplot(raster(r1, layer="SST2016_29_35"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud4[1,], col="grey", lwd=2))+
  layer(sp.lines(ud4[2,], col="brown", lwd=2))

m5<-levelplot(raster(r1, layer="SST2016_36_42"),
              col.regions=colr, margin=F, at=16:28, xlab=NULL, ylab=NULL,xlim=c(145, 165), ylim=c(-40, -20))+
  layer(sp.points(lhi, size=2))+layer(sp.lines(ud5[1,], col="grey", lwd=2))+
  layer(sp.lines(ud5[2,], col="brown", lwd=2))


jpeg("~/grive/phd/analyses/SIA/exploration/sst_gps_2016.jpg", width =12 , height =6 , units ="in", res =300)
grid.arrange(m1,m2,m3,m4,m5, nrow=2, ncol=3) 
dev.off()

# Read in fish data from revill et al 

dat<-read.csv("spreads/SSP_SIA_ready.csv", h=T)


p1<-ggplot(data=dat, aes(x=d13C_Mean, y=d15N_Mean, colour=Type))

plot1<-p1+

  geom_errorbar(aes(ymin=(d15N_Mean-d15N_SE), ymax=
                      (d15N_Mean+d15N_SE)))+
  geom_errorbarh(aes(xmin=(d13C_Mean-d13C_SE), xmax=
                      (d13C_Mean+d13C_SE)))+
  
  geom_point(size=2)+
  geom_text(aes(label=Species))+

  
  xlab(expression(δ^{13}~"C (‰)"))+
  ylab(expression(δ^{15}~"N (‰)"))+
  theme(legend.position=0,
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill=NA))




 