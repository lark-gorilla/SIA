# 20/10/17, Musanze, Rwanda
# download and extract monthly chl and sst between 2001 and 2016

rm(list=ls())
# rerdap
library(rerddap)
library(raster)
library(ncdf4)

# We use rerddap package to make direct calls to the erddap server and get 
# gridded data via the OPeNDAP hyperslab protocol??

#product code from here:
#https://coastwatch.pfeg.noaa.gov/erddap/griddap/index.html?page=1&itemsPerPage=1000

# Chlorophyll products

info("erdSW1chlamday") # 1997 - 2010

ed_search(query = 'erdMH1chlamday', which = "grid")$info

info("erdMH1chlamday")# 2003-2017

# SST products

info("erdMTsstnmday") # 2000-2013

ed_search(query = 'erdMH1sstdmday', which = "grid")$info

info("erdMH1sstdmday") # 2003 - 2016
# backup SST: "erdAGsstamday"

monthz_early<-c('2001-02-10','2002-02-10')

monthz_later<-c('2003-02-10','2004-02-10', '2005-02-10','2006-02-10','2007-02-10','2008-02-10','2009-02-10',
          '2010-02-10','2011-02-10','2012-02-10','2013-02-10','2014-02-10','2015-02-10','2016-02-10')

# replace monthz_early or monthz_late in parameter monthz and with griddap in loop

k <- griddap("erdSW1chlamday",
                time = i,
             latitude = c(-18, -35),
             longitude = c(140, 165))

k <- griddap("erdMH1chlamday",
                     time = i,
             latitude = c(-18, -35),
             longitude = c(140, 165))

k <- griddap("erdMTsstnmday",
                     time = i,
             latitude = c(-18, -35),
             longitude = c(140, 165))

k <- griddap("erdMH1sstdmday",
                    time = i,
             latitude = c(-18, -35),
             longitude = c(140, 165))

monthz<-monthz_later

st1<-NULL
for( i in monthz)
{
  
    d1<-griddap("erdMH1sstdmday",
            time = c(i,paste(substr(i,1,9), 1, sep="")),
            latitude = c(-18, -35),
            longitude = c(140, 165))
  
    print(unique(d1$data$time))
    
    r1<-raster(xmn=min(d1$data$lon), xmx=max(d1$data$lon), ymn=min(d1$data$lat), ymx=max(d1$data$lat),
               nrows=length(unique(d1$data$lat)), ncols=length(unique(d1$data$lon)), 
               vals=d1$data[,4], crs=CRS("+proj=longlat +ellps=WGS84"))
    # the correct column needs to be set for vals, normally 4
    #r1<-flip(r1, direction="y") # turn off for chla
    
    if(which(i==monthz)==1){st1<-r1}else{
      st1<-stack(st1, r1)}
    print(i)
    plot(st1)
  }
  

st1<-setZ(st1, paste(substr(monthz, 1,4),
                        substr(monthz, 6,7),
                        substr(monthz, 9,10),sep=""))


# Save the raster file as a netCDF CHL!
outfile <- paste("chl_01_02_erdSW1chlamday.nc")
setwd("~/grive/phd/sourced_data/env_data/long_term_monthly_SST_CHL")
writeRaster(st1, outfile, overwrite=TRUE, format="CDF", varname="chl", varunit="mg m^-3", 
            longname="CHLA -- erdSW1chlamday", xname="lon", yname="lat",
            zname="Date", zunit="numeric")

# save out SST! 
outfile <- paste("sst_03_16_erdMH1sstdmday.nc")
setwd("~/grive/phd/sourced_data/env_data/long_term_monthly_SST_CHL")
writeRaster(st1, outfile, overwrite=TRUE, format="CDF", varname="sst", varunit="deg C", 
            longname="SST -- erdMH1sstdmday", xname="lon", yname="lat",
            zname="Date", zunit="numeric")


# Make buffers to extract within

colz<-data.frame(long=c(159.05991,151.913773), lat=c(-31.52459,-23.44306),
                 colony=c("LHI", "Heron"))

LHI<-SpatialPoints(colz[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

DgProj <- CRS("+proj=laea +lon_0=156 +lat_0=26")
LHI_proj <- spTransform(LHI, CRS=DgProj)

# buffer uses 300km from mcduie et al short trip zone.
library(rgeos)
library(maptools)
for (i in 1:2)
{
  
  TBuffProj37 <- gBuffer(LHI_proj[i,], width=(300*1000), quadsegs=50)
  TBuffProj37@polygons[[1]]@ID <- as.character(colz[i,]$colony)
  
  b1Wgs <- spTransform(TBuffProj37, CRS=CRS( "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  if(i==1){out<-b1Wgs}else{out<-spRbind(out, b1Wgs)}
  plot(out)
  print(i)
}

# extract data

setwd("~/grive/phd/sourced_data/env_data/long_term_monthly_SST_CHL")

# use 'brick' command instead of 'raster' to load in multiple bands

chl_old<-brick("chl_01_02_erdSW1chlamday.nc")
chl_new<-brick("chl_03_16_erdMH1chlamday.nc")
sst_old<-brick("sst_01_02_erdMTsstnmday.nc")
sst_new<-brick("sst_03_16_erdMH1sstdmday.nc")

namez2<-c('x2003','x2004', 'x2005','x2006','x2007','x2008','x2009',
          'x2010','x2011','x2012','x2013','x2014','x2015','x2016')

names(chl_new)<-namez2
names(sst_new)<-namez2

chl_old_ext<-extract(chl_old, out)
chl_new_ext<-extract(chl_new, out)
sst_old_ext<-extract(sst_old, out)
sst_new_ext<-extract(sst_new, out)

library(reshape2)

all_dat<-rbind(
data.frame(melt(data.frame(chl_old_ext[1])), data= "chl", colony="Lord Howe Island"),
data.frame(melt(data.frame(chl_old_ext[2])), data= "chl", colony="Heron Island"),
data.frame(melt(data.frame(chl_new_ext[1])), data= "chl", colony="Lord Howe Island"),
data.frame(melt(data.frame(chl_new_ext[2])), data= "chl", colony="Heron Island"),
data.frame(melt(data.frame(sst_old_ext[1])), data= "sst", colony="Lord Howe Island"),
data.frame(melt(data.frame(sst_old_ext[2])), data= "sst", colony="Heron Island"),
data.frame(melt(data.frame(sst_new_ext[1])), data= "sst", colony="Lord Howe Island"),
data.frame(melt(data.frame(sst_new_ext[2])), data= "sst", colony="Heron Island"))

all_dat$Year<-substr(all_dat$variable, 2,5)

qplot(data=all_dat[all_dat$data=="chl",], x=Year, y=value, geom="boxplot")+
      facet_wrap(~colony, scales="free")

# need to remove huge outliers from Heron. use 0.3 cutoff from Lord Howe

qplot(data=all_dat[all_dat$data=="chl" & all_dat$value<0.2,], x=Year, y=value, geom="boxplot")+
  facet_wrap(~colony)

# Looking at qgis it could be lower

hist(all_dat[all_dat$data=="chl" &
               all_dat$colony=="Heron Island"& all_dat$value<0.5,]$value)

ggplot(data=na.omit(all_dat[all_dat$data=="chl" & all_dat$value<1,]),
       aes(x=Year, y=value))+geom_violin()+ facet_wrap(~colony)


ggplot(data=na.omit(all_dat[all_dat$data=="chl" & all_dat$value<0.5,]),
       aes(x=Year, y=value))+geom_violin()+ facet_wrap(~colony)


qplot(data=all_dat[all_dat$data=="sst" & all_dat$value<30,], x=Year, y=value, geom="boxplot")+
  facet_wrap(~colony)

ggplot(data=na.omit(all_dat[all_dat$data=="sst" & all_dat$value<30,]),
       aes(x=Year, y=value))+geom_violin()+ facet_wrap(~colony)

m1<-lm(value~Year, data=
       all_dat[all_dat$data=="sst" & all_dat$colony=="Lord Howe Island" &
                  all_dat$value<30,])

library(agricolae)

m1_test<-HSD.test(m1, trt="Year", console=TRUE)

m1_testout<-cbind(m1_test$means, m1_test$groups[order(as.integer(as.character(m1_test$groups$trt))),])


# 2 standard errors
#limits <- aes(ymax = tProp + tPropSE+tPropSE, ymin=tProp)
limits <- aes(ymax = value + std, ymin=value - std)

pp_16<-ggplot(data=m1_testout, aes(x=as.numeric(as.character(trt)), y=value))+
  geom_point(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  
  geom_text( aes(x=as.numeric(as.character(trt)), y=value+std+0.25, label=M))+
  theme_classic()

# hmm letters dont look right

??? repeated measures anova?? 
THINK about what im actually doing???

