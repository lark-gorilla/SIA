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

p1<-ggplot(data=dat, aes(x=d13C_VDPB, y=d15N_Air))

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
for ( i in c(658,325,210))
{
  
  TBuffProj37 <- gBuffer(LHI_proj, width=(i*1000), quadsegs=50)
  TBuffProj37@polygons[[1]]@ID <- as.character(i)
  
  b1Wgs <- spTransform(TBuffProj37, CRS=CRS( "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  if(i==658){out<-b1Wgs}else{out<-spRbind(out, b1Wgs)}
  plot(out)
  print(i)
}


