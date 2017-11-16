# 31/07/17 Musanze, Rwanda
# Production foraging strategy graphs for LHI 2004, 2015 and 2016
# Also now for Heron 2001 and 2003

rm(list=ls())
library(readxl)
library(ggplot2)
library(lubridate)

# set wd for where attendance data is stored
setwd("~/grive/phd/analyses/foraging_strategy")

# Not sourcing foragingStrat script now, will run manually
#source("~/grive/phd/scripts/WTSH_foraging_strategy/foragingStrat.R")

### 2016 ###

nest_comp_lhi16<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/LHI_2016_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)
nest_comp_lhi16$Date<-ymd(nest_comp_lhi16$Date)

unique(nest_comp_lhi16$NestID)

nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID %in%
                 c("1", "3", "6", "13", "20", "29", "35",
                   "37", "39", "40", "41", "51"),]

nest_comp<-nest_comp_lhi16
D1="2016-02-05"
D2="2016-03-05"
longallow=TRUE

all_trips<-NULL
for(i in unique(nest_comp$NestID))
{
  D1_mod<-nest_comp[nest_comp$NestID==i,]$Date[min( min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$LW_corr=="B")), 
                                                    min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$RW_corr=="B")))]
  
  for(j in c("LW", "RW"))
  {  
    if(longallow==TRUE)
    {
      backs<-NULL
      if(j=="LW")
      {
        backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")
        if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]))
        {
          backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod,]$LW_assn=="B")[length(backs)+1])
          backs<-na.omit(backs)
        }
      }
      
      if(j=="RW")
      {
        backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")
        if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]))
        {
          backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod,]$RW_assn=="B")[length(backs)+1])
          backs<-na.omit(backs)
        }
      }
    }
    
    if(longallow==FALSE)
    {  
      backs<-NULL
      if(j=="LW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")}
      if(j=="RW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")}
    }
    
    trip_lz<-diff(backs)
    if(length(backs)==1){trip_lz=NA}
    
    df_internal<-data.frame(NestID=i, BirdID=j, tLength=trip_lz)
    all_trips<-rbind(all_trips, df_internal)
  }
}

#
all_trips_16<-all_trips
# to calc hists per individual birds ~~**
all_trips$BirdID2<-paste(all_trips$NestID, all_trips$BirdID,sep="_")
trip_propz<-NULL
for(i in unique(all_trips$BirdID2))
{
  if(is.na(sum(all_trips[all_trips$BirdID2==i,]$tLength))){next} # this next catches any NA birds for whatever reason and removes. corrects warning created in max(backs) above
  df1<-data.frame(BirdID2=i,table(all_trips[all_trips$BirdID2==i,]$tLength))
  df1$Var1<-as.numeric(as.character(df1$Var1))
  df1$TotTrip<-df1$Var1 * df1$Freq 
  df1$TripProp<-df1$TotTrip/sum(df1$TotTrip)
  #print(i);print(sum(df1$TotTrip))
  trip_propz<-rbind(trip_propz, df1)
}

Nsample_birds<-length(unique(trip_propz$BirdID2))
uni_birds<-unique(trip_propz$BirdID2)
tripPropB<-NULL
tripProp_ktest<-NULL
for(i in 1:25)
{ 
  sample_bird_propz<-trip_propz[trip_propz$Var1==i,]$TripProp
  sample_bird_ID<-trip_propz[trip_propz$Var1==i,]$BirdID2
  
  if(length(sample_bird_propz)<Nsample_birds)
  {sample_bird_propz<-c(sample_bird_propz, 
                        rep(0, Nsample_birds-length(sample_bird_propz)))
  
  if(sum(sample_bird_propz)==0){sample_bird_ID<-uni_birds}else{
    sample_bird_ID<-c(as.character(sample_bird_ID), 
                      as.character(uni_birds[-which(uni_birds %in% sample_bird_ID)]))}
  }
  
  df1<-data.frame(tLength=i,
                  tFreq=sum(trip_propz[trip_propz$Var1==i,]$Freq),
                  tProp=mean(sample_bird_propz),
                  tPropSE=sd(sample_bird_propz)/sqrt(length(sample_bird_propz)))
  
  df2<-data.frame(bird_propz=sample_bird_propz, bird_ID= sample_bird_ID,
                  tLength=i)
  
  tripPropB<-rbind(tripPropB, df1)
  tripProp_ktest<-rbind(tripProp_ktest, df2)
}

# ~~

tripPropB[tripPropB$tFreq==0,]$tProp<-NA #change 0 to NA
tripPropB[tripPropB$tFreq==0,]$tLength<-NA

tripPropB_16<-tripPropB
ktest_16<-tripProp_ktest

### 2015 ###

nest_comp_lhi15<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/LHI_2015_nest_weights_attendance_cleaned_manually_assigned.csv", h=T, strip.white=T)

nest_comp_lhi15$Date<-ymd(nest_comp_lhi15$Date)

nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID %in%
                 c("1", "3", "4", "5", "7", "11", "18",
                   "22", "26", "27", "31", "47"),]

nest_comp<-nest_comp_lhi15
D1="2015-02-18"
D2="2015-03-18"
longallow=TRUE # specify longallow parameter for 2015 data as extends into April

all_trips<-NULL
for(i in unique(nest_comp$NestID))
{
  D1_mod<-nest_comp[nest_comp$NestID==i,]$Date[min( min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$LW_corr=="B")), 
                                                    min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$RW_corr=="B")))]
  
  for(j in c("LW", "RW"))
  {  
    if(longallow==TRUE)
    {
      backs<-NULL
      if(j=="LW")
      {
        backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")
        if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]))
        {
          backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod,]$LW_assn=="B")[length(backs)+1])
          backs<-na.omit(backs)
        }
      }
      
      if(j=="RW")
      {
        backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")
        if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]))
        {
          backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod,]$RW_assn=="B")[length(backs)+1])
          backs<-na.omit(backs)
        }
      }
    }
    
    if(longallow==FALSE)
    {  
      backs<-NULL
      if(j=="LW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")}
      if(j=="RW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")}
    }
    
      trip_lz<-diff(backs)
      if(length(backs)==1){trip_lz=NA}

    df_internal<-data.frame(NestID=i, BirdID=j, tLength=trip_lz)
    all_trips<-rbind(all_trips, df_internal)
  }
}

# remove 19 day trip from"1_LW" as at end of sampling
all_trips<-all_trips[all_trips$tLength<19,]
all_trips_15<-all_trips
# calc proportion of time per individual bird

# to calc hists per individual birds ~~**
all_trips$BirdID2<-paste(all_trips$NestID, all_trips$BirdID,sep="_")
trip_propz<-NULL
for(i in unique(all_trips$BirdID2))
{
  if(is.na(sum(all_trips[all_trips$BirdID2==i,]$tLength))){next} # this next catches any NA birds for whatever reason and removes. corrects warning created in max(backs) above
  df1<-data.frame(BirdID2=i,table(all_trips[all_trips$BirdID2==i,]$tLength))
  df1$Var1<-as.numeric(as.character(df1$Var1))
  df1$TotTrip<-df1$Var1 * df1$Freq 
  df1$TripProp<-df1$TotTrip/sum(df1$TotTrip)
  #print(i);print(sum(df1$TotTrip))
  trip_propz<-rbind(trip_propz, df1)
}

Nsample_birds<-length(unique(trip_propz$BirdID2))
uni_birds<-unique(trip_propz$BirdID2)
tripPropB<-NULL
tripProp_ktest<-NULL
for(i in 1:25)
{ 
  sample_bird_propz<-trip_propz[trip_propz$Var1==i,]$TripProp
  sample_bird_ID<-trip_propz[trip_propz$Var1==i,]$BirdID2
  
  if(length(sample_bird_propz)<Nsample_birds)
  {sample_bird_propz<-c(sample_bird_propz, 
                        rep(0, Nsample_birds-length(sample_bird_propz)))
  
  if(sum(sample_bird_propz)==0){sample_bird_ID<-uni_birds}else{
    sample_bird_ID<-c(as.character(sample_bird_ID), 
                      as.character(uni_birds[-which(uni_birds %in% sample_bird_ID)]))}
  }
  
  df1<-data.frame(tLength=i,
                  tFreq=sum(trip_propz[trip_propz$Var1==i,]$Freq),
                  tProp=mean(sample_bird_propz),
                  tPropSE=sd(sample_bird_propz)/sqrt(length(sample_bird_propz)))
  
  df2<-data.frame(bird_propz=sample_bird_propz, bird_ID= sample_bird_ID,
                  tLength=i)
  
  tripPropB<-rbind(tripPropB, df1)
  tripProp_ktest<-rbind(tripProp_ktest, df2)
}
# ~~

tripPropB[tripPropB$tFreq==0,]$tProp<-NA #change 0 to NA
tripPropB[tripPropB$tFreq==0,]$tLength<-NA

tripPropB_15<-tripPropB
ktest_15<-tripProp_ktest

### 2004 ###

nest_comp_lhi04<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/LHI_2004_nest_attendance_cleaned.csv", h=T)
nest_comp_lhi04$Date<-nest_comp_lhi04$Date_hack
nest_comp_lhi04$Date<-ymd(nest_comp_lhi04$Date)

nest_comp_lhi04<-nest_comp_lhi04[nest_comp_lhi04$NestID!="5a",] # remove 25% unknown nest
nest_comp_lhi04<-nest_comp_lhi04[nest_comp_lhi04$NestID!="3",] # remove 25% unknown nest

nest_comp<-nest_comp_lhi04
D1="2004-01-31"
D2="2004-02-28"


all_trips<-NULL
for(i in unique(nest_comp$NestID))
{
  D1_mod<-nest_comp[nest_comp$NestID==i,]$Date[min( min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$LW_corr=="B")), 
                                                    min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$RW_corr=="B")))]
  
  for(j in c("LW", "RW"))
  {  
   # longallow = FALSE loop
      backs<-NULL
      if(j=="LW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")}
      if(j=="RW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")}
    
    trip_lz<-diff(backs)
    if(length(backs)==1){trip_lz=NA}
    
    df_internal<-data.frame(NestID=i, BirdID=j, tLength=trip_lz)
    all_trips<-rbind(all_trips, df_internal)
  }
}
#
# quick fix to force "1_LW" bird to max trip length of 11 rather than 12
# days
all_trips[all_trips$tLength==12,]$tLength<-11
all_trips_04<-all_trips
# calc proportion of time per individual bird

# to calc hists per individual birds ~~**
all_trips$BirdID2<-paste(all_trips$NestID, all_trips$BirdID,sep="_")
trip_propz<-NULL
for(i in unique(all_trips$BirdID2))
{
  if(is.na(sum(all_trips[all_trips$BirdID2==i,]$tLength))){next} # this next catches any NA birds for whatever reason and removes. corrects warning created in max(backs) above
  df1<-data.frame(BirdID2=i,table(all_trips[all_trips$BirdID2==i,]$tLength))
  df1$Var1<-as.numeric(as.character(df1$Var1))
  df1$TotTrip<-df1$Var1 * df1$Freq 
  df1$TripProp<-df1$TotTrip/sum(df1$TotTrip)
  #print(i);print(sum(df1$TotTrip))
  trip_propz<-rbind(trip_propz, df1)
}

Nsample_birds<-length(unique(trip_propz$BirdID2))
uni_birds<-unique(trip_propz$BirdID2)
tripPropB<-NULL
tripProp_ktest<-NULL
for(i in 1:25)
{ 
  sample_bird_propz<-trip_propz[trip_propz$Var1==i,]$TripProp
  sample_bird_ID<-trip_propz[trip_propz$Var1==i,]$BirdID2
  
  if(length(sample_bird_propz)<Nsample_birds)
  {sample_bird_propz<-c(sample_bird_propz, 
                        rep(0, Nsample_birds-length(sample_bird_propz)))
  
  if(sum(sample_bird_propz)==0){sample_bird_ID<-uni_birds}else{
    sample_bird_ID<-c(as.character(sample_bird_ID), 
                      as.character(uni_birds[-which(uni_birds %in% sample_bird_ID)]))}
  }
  
  df1<-data.frame(tLength=i,
                  tFreq=sum(trip_propz[trip_propz$Var1==i,]$Freq),
                  tProp=mean(sample_bird_propz),
                  tPropSE=sd(sample_bird_propz)/sqrt(length(sample_bird_propz)))
  
  df2<-data.frame(bird_propz=sample_bird_propz, bird_ID= sample_bird_ID,
                  tLength=i)
  
  tripPropB<-rbind(tripPropB, df1)
  tripProp_ktest<-rbind(tripProp_ktest, df2)
}
# ~~

tripPropB[tripPropB$tFreq==0,]$tProp<-NA #change 0 to NA
tripPropB[tripPropB$tFreq==0,]$tLength<-NA

tripPropB_04<-tripPropB
ktest_04<-tripProp_ktest


## Heron ###

## 2001 ##

nest_comp_her01<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/Heron_2001_nest_attendance_cleaned.csv", h=T, strip.white=T)

nest_comp_her01$Date<-ymd(nest_comp_her01$Date_hack)

# Check which nests have lots of don't knows..
nest_check<-aggregate(LW_corr~NestID, nest_comp_her01, FUN=function(x){length(which(x=="D"))})

gd_nests<-nest_check[nest_check$LW_corr<4,]$NestID # gives 11 nests

nest_comp_her01<-nest_comp_her01[nest_comp_her01$NestID %in% gd_nests,]

nest_comp<-nest_comp_her01
D1="2001-02-07"
D2="2001-02-28"
longallow=TRUE # specify longallow parameter for 2015 data as extends into April

all_trips<-NULL
for(i in unique(nest_comp$NestID))
{
  D1_mod<-nest_comp[nest_comp$NestID==i,]$Date[min( min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$LW_corr=="B")), 
                                                    min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$RW_corr=="B")))]
  
  for(j in c("LW", "RW"))
  {  
    if(longallow==TRUE)
    {
      backs<-NULL
      if(j=="LW")
      {
        backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")
        if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]))
        {
          backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod,]$LW_assn=="B")[length(backs)+1])
          backs<-na.omit(backs)
        }
      }
      
      if(j=="RW")
      {
        backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")
        if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]))
        {
          backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod,]$RW_assn=="B")[length(backs)+1])
          backs<-na.omit(backs)
        }
      }
    }
    
    if(longallow==FALSE)
    {  
      backs<-NULL
      if(j=="LW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")}
      if(j=="RW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")}
    }
    
    trip_lz<-diff(backs)
    if(length(backs)==1){trip_lz=NA}
    
    df_internal<-data.frame(NestID=i, BirdID=j, tLength=trip_lz)
    all_trips<-rbind(all_trips, df_internal)
  }
}

all_trips_01<-all_trips
# calc proportion of time per individual bird

# to calc hists per individual birds ~~**
all_trips$BirdID2<-paste(all_trips$NestID, all_trips$BirdID,sep="_")
trip_propz<-NULL
for(i in unique(all_trips$BirdID2))
{
  if(is.na(sum(all_trips[all_trips$BirdID2==i,]$tLength))){next} # this next catches any NA birds for whatever reason and removes. corrects warning created in max(backs) above
  df1<-data.frame(BirdID2=i,table(all_trips[all_trips$BirdID2==i,]$tLength))
  df1$Var1<-as.numeric(as.character(df1$Var1))
  df1$TotTrip<-df1$Var1 * df1$Freq 
  df1$TripProp<-df1$TotTrip/sum(df1$TotTrip)
  #print(i);print(sum(df1$TotTrip))
  trip_propz<-rbind(trip_propz, df1)
}

Nsample_birds<-length(unique(trip_propz$BirdID2))
uni_birds<-unique(trip_propz$BirdID2)
tripPropB<-NULL
tripProp_ktest<-NULL
for(i in 1:25)
{ 
  sample_bird_propz<-trip_propz[trip_propz$Var1==i,]$TripProp
  sample_bird_ID<-trip_propz[trip_propz$Var1==i,]$BirdID2
  
  if(length(sample_bird_propz)<Nsample_birds)
  {sample_bird_propz<-c(sample_bird_propz, 
                        rep(0, Nsample_birds-length(sample_bird_propz)))
  
  if(sum(sample_bird_propz)==0){sample_bird_ID<-uni_birds}else{
  sample_bird_ID<-c(as.character(sample_bird_ID), 
                    as.character(uni_birds[-which(uni_birds %in% sample_bird_ID)]))}
  }
  
  df1<-data.frame(tLength=i,
                  tFreq=sum(trip_propz[trip_propz$Var1==i,]$Freq),
                  tProp=mean(sample_bird_propz),
                  tPropSE=sd(sample_bird_propz)/sqrt(length(sample_bird_propz)))
  
  df2<-data.frame(bird_propz=sample_bird_propz, bird_ID= sample_bird_ID,
                  tLength=i)
  
  tripPropB<-rbind(tripPropB, df1)
  tripProp_ktest<-rbind(tripProp_ktest, df2)
}
# ~~

tripPropB[tripPropB$tFreq==0,]$tProp<-NA #change 0 to NA
tripPropB[tripPropB$tFreq==0,]$tLength<-NA

tripPropB_01<-tripPropB
ktest_01<-tripProp_ktest

## 2003 ##

nest_comp_her03<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/Heron_2003_nest_attendance_cleaned.csv", h=T, strip.white=T)

nest_comp_her03$Date<-ymd(nest_comp_her03$Date_hack)

# Check which nests have lots of don't knows..
nest_check<-aggregate(LW_corr~NestID, nest_comp_her03, FUN=function(x){length(which(x=="D"))})

gd_nests<-nest_check[nest_check$LW_corr<4,]$NestID # gives 14 nests

gd_nests<-gd_nests[gd_nests!=5]
# REMOVE NEST 5
# ADD NEST 8
gd_nests<-c(8,gd_nests)

nest_comp_her03<-nest_comp_her03[nest_comp_her03$NestID %in% gd_nests,]

nest_comp<-nest_comp_her03
D1="2003-02-08"
D2="2003-03-08"
longallow=TRUE # specify longallow parameter for 2015 data as extends into April

all_trips<-NULL
for(i in unique(nest_comp$NestID))
{
  D1_mod<-nest_comp[nest_comp$NestID==i,]$Date[min( min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$LW_corr=="B")), 
                                                    min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$RW_corr=="B")))]
  
  for(j in c("LW", "RW"))
  {  
    if(longallow==TRUE)
    {
      backs<-NULL
      if(j=="LW")
      {
        backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")
        if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]))
        {
          backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod,]$LW_assn=="B")[length(backs)+1])
          backs<-na.omit(backs)
        }
      }
      
      if(j=="RW")
      {
        backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")
        if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]))
        {
          backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod,]$RW_assn=="B")[length(backs)+1])
          backs<-na.omit(backs)
        }
      }
    }
    
    if(longallow==FALSE)
    {  
      backs<-NULL
      if(j=="LW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")}
      if(j=="RW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")}
    }
    
    trip_lz<-diff(backs)
    if(length(backs)==1){trip_lz=NA}
    
    df_internal<-data.frame(NestID=i, BirdID=j, tLength=trip_lz)
    all_trips<-rbind(all_trips, df_internal)
  }
}

all_trips_03<-all_trips
# calc proportion of time per individual bird

# to calc hists per individual birds ~~**
all_trips$BirdID2<-paste(all_trips$NestID, all_trips$BirdID,sep="_")
trip_propz<-NULL
for(i in unique(all_trips$BirdID2))
{
  if(is.na(sum(all_trips[all_trips$BirdID2==i,]$tLength))){next} # this next catches any NA birds for whatever reason and removes. corrects warning created in max(backs) above
  df1<-data.frame(BirdID2=i,table(all_trips[all_trips$BirdID2==i,]$tLength))
  df1$Var1<-as.numeric(as.character(df1$Var1))
  df1$TotTrip<-df1$Var1 * df1$Freq 
  df1$TripProp<-df1$TotTrip/sum(df1$TotTrip)
  #print(i);print(sum(df1$TotTrip))
  trip_propz<-rbind(trip_propz, df1)
}

Nsample_birds<-length(unique(trip_propz$BirdID2))
uni_birds<-unique(trip_propz$BirdID2)
tripPropB<-NULL
tripProp_ktest<-NULL
for(i in 1:25)
{ 
  sample_bird_propz<-trip_propz[trip_propz$Var1==i,]$TripProp
  sample_bird_ID<-trip_propz[trip_propz$Var1==i,]$BirdID2
  
  if(length(sample_bird_propz)<Nsample_birds)
  {sample_bird_propz<-c(sample_bird_propz, 
                        rep(0, Nsample_birds-length(sample_bird_propz)))
  
  if(sum(sample_bird_propz)==0){sample_bird_ID<-uni_birds}else{
    sample_bird_ID<-c(as.character(sample_bird_ID), 
                      as.character(uni_birds[-which(uni_birds %in% sample_bird_ID)]))}
  }
  
  df1<-data.frame(tLength=i,
                  tFreq=sum(trip_propz[trip_propz$Var1==i,]$Freq),
                  tProp=mean(sample_bird_propz),
                  tPropSE=sd(sample_bird_propz)/sqrt(length(sample_bird_propz)))
  
  df2<-data.frame(bird_propz=sample_bird_propz, bird_ID= sample_bird_ID,
                  tLength=i)
  
  tripPropB<-rbind(tripPropB, df1)
  tripProp_ktest<-rbind(tripProp_ktest, df2)
}
# ~~

tripPropB[tripPropB$tFreq==0,]$tProp<-NA #change 0 to NA
tripPropB[tripPropB$tFreq==0,]$tLength<-NA

tripPropB_03<-tripPropB
ktest_03<-tripProp_ktest



### Plotting

# paper stats
# run kruskall wallace and pairwise Mann-whitney U (with bonferroni)
# as per Peck and Congdon 2005

library(agricolae)
# kruskal from agricolae runs both a kruskal-wallace and post hoc comp!

# remove trip lengths that weren't used
ktest_16<-ktest_16[-which(ktest_16$tLength %in% which(aggregate(bird_propz~tLength, ktest_16, sum)[,2]==0)),]
ktest_15<-ktest_15[-which(ktest_15$tLength %in% which(aggregate(bird_propz~tLength, ktest_15, sum)[,2]==0)),]
ktest_04<-ktest_04[-which(ktest_04$tLength %in% which(aggregate(bird_propz~tLength, ktest_04, sum)[,2]==0)),]
ktest_01<-ktest_01[-which(ktest_01$tLength %in% which(aggregate(bird_propz~tLength, ktest_01, sum)[,2]==0)),]
ktest_03<-ktest_03[-which(ktest_03$tLength %in% which(aggregate(bird_propz~tLength, ktest_03, sum)[,2]==0)),]


#Using Friedman instead of kruskal tests as birdID is repreatadly sampled

f_16<-with(ktest_16, friedman(judge=bird_ID,
          trt=tLength, evaluation=bird_propz, group=T, console=T))

f_15<-with(ktest_15, friedman(judge=bird_ID,
                              trt=tLength, evaluation=bird_propz, group=T, console=T))

f_04<-with(ktest_04, friedman(judge=bird_ID,
                              trt=tLength, evaluation=bird_propz, group=T, console=T))

f_01<-with(ktest_01, friedman(judge=bird_ID,
                              trt=tLength, evaluation=bird_propz, group=T, console=T))

f_03<-with(ktest_03, friedman(judge=bird_ID,
                              trt=tLength, evaluation=bird_propz, group=T, console=T))


fpl_16<-cbind(f_16$means, f_16$groups[order(as.integer(as.character(f_16$groups$trt))),])
fpl_15<-cbind(f_15$means, f_15$groups[order(as.integer(as.character(f_15$groups$trt))),])
fpl_04<-cbind(f_04$means, f_04$groups[order(as.integer(as.character(f_04$groups$trt))),])
fpl_01<-cbind(f_01$means, f_01$groups[order(as.integer(as.character(f_01$groups$trt))),])
fpl_03<-cbind(f_03$means, f_03$groups[order(as.integer(as.character(f_03$groups$trt))),])

          
# 2 standard errors
#limits <- aes(ymax = tProp + tPropSE+tPropSE, ymin=tProp)
limits <- aes(ymax = bird_propz + (std/sqrt(36))*2, ymin=bird_propz)

pp_16<-ggplot(data=fpl_16, aes(x=as.numeric(as.character(trt)), y=bird_propz))+
  geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(expand = c(0, 0), limits=c(0, 16), breaks=seq(1,15))+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 0.41), breaks=seq(0.1,0.41, 0.1))+
  xlab(NULL)+ylab(NULL)+
  geom_text( aes(x=as.numeric(as.character(trt)), y=bird_propz + (std/sqrt(36))*2+0.025, label=M))+
  geom_text(aes(x=14, y=0.35, label="(a) LHI  2016"), size=6)+
  theme_classic()+
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15))

pp_15<-ggplot(data=fpl_15, aes(x=as.numeric(as.character(trt)), y=bird_propz))+
  geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(expand = c(0, 0), limits=c(0, 16), breaks=seq(1,15))+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 0.41), breaks=seq(0.1,0.41, 0.1))+
  xlab(NULL)+ylab(NULL)+
  geom_text( aes(x=as.numeric(as.character(trt)), y=bird_propz + (std/sqrt(36))*2+0.025, label=M))+
  geom_text(aes(x=14, y=0.35, label="(b)  LHI 2015"), size=6)+
  theme_classic()+
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15))

pp_04<-ggplot(data=fpl_04, aes(x=as.numeric(as.character(trt)), y=bird_propz))+
  geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(expand = c(0, 0), limits=c(0, 16), breaks=seq(1,15))+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 0.41), breaks=seq(0.1,0.41, 0.1))+
  xlab(NULL)+ylab(NULL)+
  geom_text( aes(x=as.numeric(as.character(trt)), y=bird_propz + (std/sqrt(36))*2+0.025, label=M))+
  geom_text(aes(x=14, y=0.35, label="(c)  LHI 2004"), size=6)+
  theme_classic()+
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15))

pp_03<-ggplot(data=fpl_03, aes(x=as.numeric(as.character(trt)), y=bird_propz))+
  geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(expand = c(0, 0), limits=c(0, 16), breaks=seq(1,15))+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 0.41), breaks=seq(0.1,0.41, 0.1))+
  xlab(NULL)+ylab(NULL)+
  geom_text( aes(x=as.numeric(as.character(trt)), y=bird_propz + (std/sqrt(36))*2+0.025, label=M))+
  geom_text(aes(x=14, y=0.35, label="(d)  HER 2003"), size=6)+
  theme_classic()+
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15))

pp_01<-ggplot(data=fpl_01, aes(x=as.numeric(as.character(trt)), y=bird_propz))+
  geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(expand = c(0, 0), limits=c(0, 16), breaks=seq(1,15))+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 0.6), breaks=seq(0.1,0.6, 0.1))+
  xlab(NULL)+ylab(NULL)+
  geom_text( aes(x=as.numeric(as.character(trt)), y=bird_propz + (std/sqrt(36))*2+0.025, label=M))+
  geom_text(aes(x=14, y=0.55, label="(e)  HER 2001"), size=6)+
  theme_classic()+
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15))



library(gridExtra)
library(grid)

png("~/grive/phd/analyses/SIA/paper_plots/foraging_strat_new1_lores.jpg", width = 9, height =12 , units ="in", res =300)

grid.arrange(pp_16, pp_15, pp_04, pp_03, pp_01, ncol=1,nrow=5,
             left=textGrob("Mean time spent foraging (proportion)", rot=90,
                           gp=gpar(fontsize=20)),
             bottom=textGrob("Duration of foraging trip (days)",
                           gp=gpar(fontsize=20)),heights=c(1,1,1,1,1.5))

dev.off()

# getting mean trip length comparison between year stats

all_years<-rbind(data.frame(all_trips_01, year=as.character("2001")),
                 data.frame(all_trips_03, year=as.character("2003")),
                 data.frame(all_trips_04, year=as.character("2004")),
                 data.frame(all_trips_15, year=as.character("2015")),
                 data.frame(all_trips_16, year=as.character("2016")))

all_years$BirdID2<-paste(all_years$year, all_years$NestID, all_years$BirdID)

library(car) # fo
leveneTest(tLength~year, data=all_years, center=mean)# uneven variances
shapiro.test(all_years$tLength)#not normal
hist(all_years$tLength) # not normal, too much for poisson

k_allyr<-with(all_years, kruskal(tLength,year, console=T))

# Or try taking mean of each bird

all_yr_mn<-aggregate(tLength~BirdID2, data=all_years, mean)
all_yr_mn$year<-substr(all_yr_mn$BirdID2, 1,4)
m1<-lm(tLength~year, all_yr_mn)
lsmeans(m1, specs="year")

# nah dont like.

# But need to test LHI and Heron seperately

all_lhi<-rbind(data.frame(all_trips_04, year=as.character("2004")),
                 data.frame(all_trips_15, year=as.character("2015")),
                 data.frame(all_trips_16, year=as.character("2016")))

k_lhi<-with(all_lhi,kruskal(tLength,year, console=T))
k_lhi

all_her<-rbind(data.frame(all_trips_01, year=as.character("2001")),
                 data.frame(all_trips_03, year=as.character("2003")))

w_her<-wilcox.test(tLength~year, all_her)
w_her
# OLD

# pariwise Mann-whitney U test

library(multcompView)

# function yoinked from:
# https://fabiomarroni.wordpress.com/2017/03/25/perform-pairwise-wilcoxon-test-classify-groups-by-significance-and-plot-results/

tri.to.squ<-function(x)
{
  rn<-row.names(x)
  cn<-colnames(x)
  an<-unique(c(cn,rn))
  myval<-x[!is.na(x)]
  mymat<-matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
  for(ext in 1:length(cn))
  {
    for(int in 1:length(rn))
    {
      if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
      mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
      mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
    }
    
  }
  return(mymat)
}

pw_16<-pairwise.wilcox.test(ktest_16$AS_bird_propz, ktest_16$tLength, p.adj = "bonf", paired=FALSE)
mymat<-tri.to.squ(pw_16$p.value)
myletters16<-multcompLetters(mymat,compare="<=",threshold=0.05,Letters=letters)

pw_15<-pairwise.wilcox.test(ktest_15$AS_bird_propz, ktest_15$tLength, p.adj = "bonf", paired=FALSE)
mymat<-tri.to.squ(pw_15$p.value)
myletters15<-multcompLetters(mymat,compare="<=",threshold=0.05,Letters=letters)

pw_04<-pairwise.wilcox.test(ktest_04$AS_bird_propz, ktest_04$tLength, p.adj = "bonf", paired=FALSE)
mymat<-tri.to.squ(pw_04$p.value)
myletters04<-multcompLetters(mymat,compare="<=",threshold=0.05,Letters=letters)

le_16<-data.frame(letters=myletters16$Letters, tLength=dimnames(myletters16$LetterMatrix)[[1]])
le_15<-data.frame(letters=myletters15$Letters, tLength=dimnames(myletters15$LetterMatrix)[[1]])
le_04<-data.frame(letters=myletters04$Letters, tLength=dimnames(myletters04$LetterMatrix)[[1]])

le_16$ypos= tripPropB_16[-which(is.na(tripPropB_16$tLength)),]$tProp+((tripPropB_16[-which(is.na(tripPropB_16$tLength)),]$tPropSE)*2)+0.025
le_15$ypos= tripPropB_15[-which(is.na(tripPropB_15$tLength)),]$tProp+((tripPropB_15[-which(is.na(tripPropB_15$tLength)),]$tPropSE)*2)+0.025
le_04$ypos= tripPropB_04[-which(is.na(tripPropB_04$tLength)),]$tProp+((tripPropB_04[-which(is.na(tripPropB_04$tLength)),]$tPropSE)*2)+0.025

# add in letters from kruskal test
lk_15<-k_15$groups
lk_15$trt<-as.numeric(as.character(lk_15$trt))
lk_15<-lk_15[order(lk_15$trt),]
le_15$letters_krus<-lk_15$M

lk_16<-k_16$groups
lk_16$trt<-as.numeric(as.character(lk_16$trt))
lk_16<-lk_16[order(lk_16$trt),]
le_16$letters_krus<-lk_16$M

lk_04<-k_04$groups
lk_04$trt<-as.numeric(as.character(lk_04$trt))
lk_04<-lk_04[order(lk_04$trt),]
le_04$letters_krus<-lk_04$M

# 2 standard errors
#limits <- aes(ymax = tProp + tPropSE+tPropSE, ymin=tProp)
limits <- aes(ymax = bird_propz + (std/sqrt(36))*2, ymin=bird_propz)



pp_04<-ggplot(data=tripPropB_04, aes(x=tLength, y=tProp))+
  geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(expand = c(0, 0), limits=c(0, 14), breaks=seq(1,13))+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 0.41), breaks=seq(0.1,0.41, 0.1))+
  xlab(NULL)+ylab(NULL)+
  geom_text(data=le_04, aes(x=as.numeric(as.character(tLength)), y=ypos, label=letters))+
  geom_text(aes(x=13, y=0.35, label="(a)"), size=6)+
  theme_classic()+
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15))

pp_15<-ggplot(data=tripPropB_15, aes(x=tLength, y=tProp))+
  geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(expand = c(0, 0), limits=c(0, 14), breaks=seq(1,13))+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 0.41), breaks=seq(0.1,0.41, 0.1))+
  xlab(NULL)+ylab(NULL)+
  geom_text(data=le_15, aes(x=as.numeric(as.character(tLength)), y=ypos, label=letters))+
  geom_text(aes(x=13, y=0.35, label="(b)"), size=6)+
  theme_classic()+
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15))

pp_16<-ggplot(data=tripPropB_16, aes(x=tLength, y=tProp))+
  geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(expand = c(0, 0), limits=c(0, 14), breaks=seq(1,13))+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 0.41), breaks=seq(0.1,0.41, 0.1))+
  theme_classic()+ylab(NULL)+
  geom_text(data=le_16, aes(x=as.numeric(as.character(tLength)), y=ypos, label=letters))+
  geom_text(aes(x=13, y=0.35, label="(c)"), size=6)+
  xlab("Duration of foraging trip (days)")+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title.x = element_text(size =20))

# LHI 15 dual foraging plot

### 2015 ###
library(lubridate)

nest_comp_lhi15<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/LHI_2015_nest_weights_attendance_cleaned_manually_assigned.csv", h=T, strip.white=T)

nest_comp_lhi15$Date<-ymd_hms(paste(nest_comp_lhi15$Date, "00-00-00"))

nest_comp<-nest_comp_lhi15[nest_comp_lhi15$NestID %in%
                                   c("1", "3", "4", "5", "7", "11", "18",
                                     "22", "26", "27", "31", "47"),]

D1="2015-02-18"
D2="2015-03-18"

nest_comp<-nest_comp[nest_comp$Date>=D1 & nest_comp$Date<=D2,]

nc2<-NULL
for(i in 1:nrow(nest_comp)){
out<-rbind(nest_comp[i,], nest_comp[i,])
nc2<-rbind(nc2, out)
}

nc2<-nc2[nc2$NestID %in% c(7,26,22),]

#write.csv(nc2, "LHI_nests_for_atten_graph.csv", quote=F, row.names=F)


pdat<-read.csv("~/grive/phd/analyses/foraging_strategy/LHI_nests_for_atten_graph.csv", h=T)


pdat$DateTime<-ymd_hms(paste(pdat$Date, pdat$Time))

pdat1<-pdat
pdat2<-pdat

pdat1$RW_plot<-NULL
names(pdat1)[names(pdat1)=="LW_plot"]<-"plot_w"
pdat1$wing<-"L"

pdat2$LW_plot<-NULL
names(pdat2)[names(pdat2)=="RW_plot"]<-"plot_w"
pdat2$wing<-"R"

pdat3<-rbind(pdat1, pdat2)

library(ggplot2)
library(tidyverse)# this and below are for geom_line hack
library(signal)
#https://stackoverflow.com/questions/43771900/use-curved-lines-in-bumps-chart

#remove nas
pdat3<-na.omit(pdat3)

#make cleaver splines
pdat4<-pdat3
pdat4$DateTime<-as.integer(as.numeric(pdat4$DateTime))

#for nest 26
data_line_26 = pdat4[pdat4$NestID=="26",] %>%
  group_by(wing) %>%
  do({
    tibble(DateTime = seq(min(.$DateTime), max(.$DateTime),length.out=1000),
           plot_w = pchip(.$DateTime, .$plot_w, DateTime))})

data_line_26<-data.frame(data_line_26)
data_line_26$DateTime<- ymd_hms(paste(as.Date(as.POSIXlt(data_line_26$DateTime, origin="1970-01-01", "GMT")),
format((as.POSIXlt(data_line_26$DateTime, origin="1970-01-01", "GMT")), "%H-%M-%S")))

#for nest 22
data_line_22 = pdat4[pdat4$NestID=="22",] %>%
  group_by(wing) %>%
  do({
    tibble(DateTime = seq(min(.$DateTime), max(.$DateTime),length.out=1000),
           plot_w = pchip(.$DateTime, .$plot_w, DateTime))})

data_line_22<-data.frame(data_line_22)
data_line_22$DateTime<- ymd_hms(paste(as.Date(as.POSIXlt(data_line_22$DateTime, origin="1970-01-01", "GMT")),
                                      format((as.POSIXlt(data_line_22$DateTime, origin="1970-01-01", "GMT")), "%H-%M-%S")))
#for nest 7
data_line_7 = pdat4[pdat4$NestID=="7",] %>%
  group_by(wing) %>%
  do({
    tibble(DateTime = seq(min(.$DateTime), max(.$DateTime),length.out=1000),
           plot_w = pchip(.$DateTime, .$plot_w, DateTime))})

data_line_7<-data.frame(data_line_7)
data_line_7$DateTime<- ymd_hms(paste(as.Date(as.POSIXlt(data_line_7$DateTime, origin="1970-01-01", "GMT")),
                                      format((as.POSIXlt(data_line_7$DateTime, origin="1970-01-01", "GMT")), "%H-%M-%S")))


# make plots

# nest 26
p3<-ggplot(data=pdat3[pdat3$NestID=="26",], aes(x=DateTime, y=plot_w, colour=wing))+
  geom_line(data = data_line_26, aes(linetype=wing))+
  geom_point()+
  scale_x_datetime(date_minor_breaks = "1 week", limits=c(ymd_hms("2015-02-18 00-00-00"),
                                                          ymd_hms("2015-03-17 12-00-00")))+
  scale_y_discrete(limits=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
  labels=c("Colony", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  ylab("Trip length (days)")+xlab("Date")+
  theme_bw()+
  geom_text(aes(x=ymd_hms("2015-02-19 00-00-00"), y=12, label="c"), size=8, col=1)+
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
  theme(axis.text.x = element_text( size=14))+
  theme(axis.text.y = element_text( size=14))+
  theme(legend.position=0, axis.title.x=element_blank())

# nest 22
p2<-ggplot(data=pdat3[pdat3$NestID=="22",], aes(x=DateTime, y=plot_w, colour=wing))+
  geom_line(data = data_line_22, aes(linetype=wing))+
  geom_point()+
  scale_x_datetime(date_minor_breaks = "1 week", limits=c(ymd_hms("2015-02-18 00-00-00"),
                                                          ymd_hms("2015-03-17 12-00-00")))+
  
  scale_y_discrete(limits=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                   labels=c("Colony", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  ylab("Trip length (days)")+
  theme_bw()+
  geom_text(aes(x=ymd_hms("2015-02-19 00-00-00"), y=12, label="b"), size=8, col=1)+
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
  theme(axis.text.x = element_text( size=14))+
  theme(axis.text.y = element_text( size=14))+
  theme(legend.position=0, axis.title.x=element_blank())

# nest 22
p1<-ggplot(data=pdat3[pdat3$NestID=="7",], aes(x=DateTime, y=plot_w, colour=wing))+
  geom_line(data = data_line_7, aes(linetype=wing))+
  geom_point()+
  scale_x_datetime(date_minor_breaks = "1 week", limits=c(ymd_hms("2015-02-18 00-00-00"),
                                                          ymd_hms("2015-03-17 12-00-00")))+
  
  scale_y_discrete(limits=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                   labels=c("Colony", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  ylab("Trip length (days)")+
  theme_bw()+
  geom_text(aes(x=ymd_hms("2015-02-19 00-00-00"), y=12, label="a"), size=8, col=1)+
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
  theme(axis.text.x = element_text( size=14))+
  theme(axis.text.y = element_text( size=14))+
  theme(legend.position=0, axis.title.x=element_blank())

library(gridExtra)
library(grid)
png("/home/mark/grive/phd/analyses/SIA/paper_plots/attendanceplot2.jpg", width = 9, height =9 , units ="in", res =300)


grid.arrange( p1,  p2, p3, ncol=1,nrow=3, 
              bottom=textGrob("Date",gp=gpar(fontsize=18)))

dev.off()


