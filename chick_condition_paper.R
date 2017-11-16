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

# Now to run analyses for paper.
# Using Brad's method of regressing all chicks (per colony, years used)
# weight against tarsus then taking residuals of each year from the mean 
# line to see which years are better than others

# check whether to use just overlapping dates (I think best) or whole time

all_ck$ckID<-paste(all_ck$Year, all_ck$NestID)


lhi_paper<-all_ck[all_ck$Col=="LHI" & all_ck$ColYr!="LHI_2014",]

her_paper<-all_ck[all_ck$ColYr=="Heron_2001" | all_ck$ColYr=="Heron_2003" | all_ck$ColYr=="Heron_2015" ,]

aggregate(week~ColYr, lhi_paper, FUN=function(x){unique(x)})

aggregate(week~ColYr, her_paper, FUN=function(x){unique(x)})

# Use cutoff at week 10 = mid March

lhi_paper$ck_cul<-NULL
her_paper$ck_cul<-NULL

her_paper<-na.omit(her_paper)
lhi_paper<-na.omit(lhi_paper)

lhi_paper<-lhi_paper[lhi_paper$week<11,]

her_paper<-her_paper[her_paper$week<11,]

qplot(data=lhi_paper, y=ck_weight, x=ck_tar, colour=factor(Year))
qplot(data=her_paper, y=ck_weight, x=ck_tar, colour=factor(Year))

hist(lhi_paper$ck_weight) # looks normal
shapiro.test(lhi_paper$ck_weight) # hmm not sure about that

library(lme4)

lhi_mod<-lmer(sqrt(ck_weight)~ck_tar + (1|ckID), data=lhi_paper)

plot(lhi_mod)
qqnorm(resid(lhi_mod))

lhi_paper$resids<-residuals(lhi_mod)
lhi_paper$resids2<-(lhi_paper$ck_weight)-(fitted(lhi_mod)^2) # backtransformed
lhi_paper$resids3<-(lhi_paper$ck_weight)-(predict(lhi_mod, re.form=~0)^2) 
# resids 3 are predicting without the random effect


qplot(data=lhi_paper,x=factor(Year), group=Year, y=resids3, geom="boxplot")

library(lmerTest)

summary(lhi_mod)

lhi_paper$Year<-as.character(lhi_paper$Year)

lhi_mod2<-lmer(resids3~Year+ (1|ckID), data=lhi_paper)

anova(lhi_mod2)

lsmeansLT(lhi_mod2, test.effs='Year')

# now Heron

hist(her_paper$ck_weight)
hist(log(her_paper$ck_weight)) # looks better
shapiro.test(her_paper$ck_weight) # hmm

her_mod<-lmer(sqrt(ck_weight)~ck_tar + (1|ckID), data=her_paper)

plot(her_mod)
qqnorm(resid(her_mod))

plot(her_mod,4) # get rid of some outliers

her_paper<-her_paper[-which(row.names(her_paper)%in%c(23023, 23034, 23121)),]
her_paper<-na.omit(her_paper)
# remake
her_mod<-lmer(sqrt(ck_weight)~ck_tar + (1|ckID), data=her_paper)

her_paper$resids<-residuals(her_mod)
her_paper$resids2<-(her_paper$ck_weight)-(fitted(her_mod)^2) # backtransformed
her_paper$resids3<-(her_paper$ck_weight)-(predict(her_mod, re.form=~0)^2) # backtransformed


qplot(data=her_paper,x=factor(Year), group=Year, y=resids3, geom="boxplot")

summary(her_mod)

her_paper$Year<-as.character(her_paper$Year)

her_mod2<-lmer(resids3~Year+ (1|ckID), data=her_paper)

anova(her_mod2)

lsmeansLT(her_mod2, test.effs='Year')

# make plot with both colonies

df1<-rbind(data.frame(Colony="a",
           lsmeansLT(lhi_mod2, test.effs='Year')$ lsmeans.table),
           data.frame(Colony="b",
            lsmeansLT(her_mod2, test.effs='Year')$ lsmeans.table))

df1$ColYr<-paste(df1$Colony, df1$Year)

df1$Year<-relevel(df1$Year, ref="2003")
df1$Year<-relevel(df1$Year, ref="2001")


p<-ggplot(data=df1, aes(x=Year))
p2<-p+geom_point((aes(y=Estimate)), shape=1, size=3)+
  geom_hline(aes(yintercept=0), linetype=2)+
  geom_errorbar(aes(ymin=Lower.CI, ymax=Upper.CI))+
  facet_wrap(~Colony, scale="free_x")+
  ylab("Mean chick condition")+
  theme_bw()+theme(strip.text = element_text(size=20))+
  scale_y_continuous(limits=c(-30, 30), breaks=c(-30,-20,-10,0,10,20, 30))

jpeg("~/grive/phd/analyses/SIA/paper_plots/ck_cond2_CI.jpg", width =8 , height =4 , units ="in", res =300)
#A4 size
p2
dev.off()

#test if difference between Heron and LHI colony
# actually we are not doing this as per Brads suggestion to make
# each graph seperately then the 0 line should be roughly compararble
# also it questions a lot of work they have already published

p1<-data.frame(ck_tar=seq(18, 50,0.1))
p1$lhi<-predict(lhi_mod, newdata=p1)
p1$her<-predict(her_mod, newdata=p1)

library(reshape2)
p2<-melt(p1, id="ck_tar")

qplot(data=p2, x=ck_tar, y=value, colour=variable, geom="line")+ylab("sqrt(chick weight)")
# no different

her_paper$LOG_ck_weight<-NULL

d2<-rbind(lhi_paper, her_paper)

m1<-lm(sqrt(ck_weight)~ck_tar+Col, data=d2)

summary(m1)

anova(m1)

m2<-lm(sqrt(ck_weight)~ck_tar, data=d2)
plot(m2,1)
plot(m2,2)
plot(m2,4) 


d2$resids2<-(d2$ck_weight)-(fitted(m2)^2) # backtransformed

qplot(data=d2, x=factor(ColYr), group=ColYr, y=resids2, geom="boxplot")

library(agricolae)

m3<-lm(resids2~ColYr, data=d2)
anova(m3)

HSD.test(m3, trt='ColYr', console = T, alpha=0.001)
HSD.test(m3, trt='ColYr', console = T, alpha=0.05)

lsmeans(m3, specs='ColYr')


## Using the lhi_paper and her_paper data before the na.omit() and
## without removing the earlier tarsus NAs i.e giving all weights

setwd("~/grive/phd/analyses/foraging_strategy")

all_ck<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/ck_wgt_tar_LHI_Heron_ALL.csv", h=T, strip.white=T)

all_ck$ColYr<-paste(all_ck$Col, all_ck$Year, sep="_")

all_ck<-all_ck[all_ck$ck_weight<600,]

# remove chicks only measured in mornings (LHI 2014)
all_ck<-all_ck[-which(all_ck$NestID %in% c("c1", "c2", "c3", "c4", "c5")),]

# remove rows without a weight (will remove some tarsus measures)
all_ck<-all_ck[-which(is.na(all_ck$ck_weight)),]

# ok just some other exploratory graphs, while we have the data
all_ck$Date<-ymd(substr(all_ck$DateTime,1,10))
# add date

all_ck$ckID<-paste(all_ck$Year, all_ck$NestID)

all_ck$week<-week(all_ck$DateTime)

lhi_paper<-all_ck[all_ck$Col=="LHI" & all_ck$ColYr!="LHI_2014",]

her_paper<-all_ck[all_ck$ColYr=="Heron_2001" | all_ck$ColYr=="Heron_2003" | all_ck$ColYr=="Heron_2015" ,]

# Use cutoff at week 10 = mid March

lhi_paper$ck_cul<-NULL
her_paper$ck_cul<-NULL



lhi_paper<-lhi_paper[lhi_paper$week<11,]

her_paper<-her_paper[her_paper$week<11,]


all_dat<-rbind(her_paper, lhi_paper)

# only calc for chicks with tar

cksWtar<-unique(rbind(her_paper, lhi_paper)$ckID) # using her and lhi_paper data that DO have only tar meaurements

all_dat<-all_dat[all_dat$ckID %in% cksWtar,]

#fix ckID to include ID for colony too

all_dat$ckID<-paste(all_dat$Col, all_dat$ckID)

all_dat$hour<-hour(all_dat$DateTime)
all_dat<-all_dat[all_dat$hour>14,]

all_dat$Date<-date(all_dat$DateTime)

# loop to force one measurement per day. Set as earliest measurement to
# 14:00 if there are daily duplicates
out<-NULL
for( i in unique(all_dat$ckID))
{
  d1<-all_dat[all_dat$ckID==i,]
  
  if(TRUE %in% duplicated(d1[d1$ckID==i,]$Date)){
    
  d1<-d1[-which(duplicated(d1[d1$ckID==i,]$Date)),]}
  
  out<-rbind(out, d1)
}

# check for gaps
out$day<-day(out$DateTime)
out$daydiff<-0
for( i in unique(out$ckID))
{
  out[out$ckID==i,]$daydiff<-c(0,diff(out[out$ckID==i,]$day))
}

table(out$daydiff)
# manual clean of gaps> 2 days

out<-out[-which(out$Year==2001 & out$daydiff==3),]

out<-out[-which(out$ckID=="Heron 2003 22" & out$daydiff==0),]

out<-out[out$ckID!="LHI 2015 45",]

aggy1<-data.frame(aggregate(Date~ckID, out, FUN=function(x){length(x)}))

# remove poor smapled nests

out<-out[out$ckID %in% aggy1[aggy1$Date>10,]$ckID,]

# loop to calc chick differences
out$ckdiff<-0
for( i in unique(out$ckID))
{
  out[out$ckID==i,]$ckdiff<-c(0,diff(out[out$ckID==i,]$ck_weight))
}

# loop to calc proportion of nests fed each day
# run per dataset

d1<-out[out$ColYr=="Heron_2015" & out$week %in% c(7,8,9),]

dm_out<-NULL
for(i in unique(d1$Date))
{
  day0<-d1[d1$Date==i,]
  if(sum(day0$ckdiff)==0){next}
  
  day1<-day0[day0$ckdiff!=0,] # remove nests which still havent started
  
  df1<-data.frame(ColYr=unique(day1$ColYr), Date= unique(day1$Date),
                prop_nest_fed=nrow(day1[day1$ckdiff>0,])/nrow(day1), n_nest=nrow(day1))
  
  dm_out<-rbind(dm_out, df1)
}

her01<-dm_out
her03<-dm_out
her15<-dm_out

lhi04<-dm_out
lhi15<-dm_out
lhi16<-dm_out

propfd2<-rbind(her01, her03, her15, lhi04, lhi15, lhi16)

aggregate(prop_nest_fed~ColYr, propfd2, mean)

aggregate(prop_nest_fed~ColYr, propfd2, sd)

m1<-lm(prop_nest_fed~ColYr, propfd2)
summary(m1)

library(lsmeans)
lsmeans(m1, "ColYr")

library(agricolae)
tuk1<-HSD.test(m1,  "ColYr")

propfd2H<-rbind(her01, her03, her15 )

propfd2L<-rbind(lhi04, lhi15, lhi16 )



m1H<-lm(prop_nest_fed~ColYr, propfd2H)
summary(m1H)
lsmeans(m1H, "ColYr")
tuk1<-HSD.test(m1H,  "ColYr")
tuk1

m1L<-lm(prop_nest_fed~ColYr, propfd2L)
summary(m1L)
lsmeans(m1L, "ColYr")
tuk1<-HSD.test(m1L,  "ColYr")
tuk1






#average daily mass gains in chicks
agg1<-data.frame(aggregate(ckdiff~ckID, out[out$week %in% c(7,8,9) & out$ckdiff>0,],mean))

agg1$Col<-substr(agg1$ckID,1,3)

agg1$Year<-"2001"
agg1[25:44,]$Year<-"2003"
agg1[45:63,]$Year<-"2003"
agg1[64:83,]$Year<-"2004"
agg1[84:143,]$Year<-"2015"
agg1[144:185,]$Year<-"2016"


agg1$ColYr<-paste(agg1$Col, agg1$Year)

aggregate(ckdiff~ColYr, agg1 ,mean)

# still dont like it


# not needed
#loop to calc interval since feed

out$feed_interval<-999
for( i in unique(out$ckID))
{
  internaldf<-
  d2<-out[out$ckID==i,]
  
  int_bind<-NULL
  h=1
  while(h <= nrow(d2))
  {
    j=h
    if(j==nrow(d2) & d2[j,]$ckdiff<0.1){break}
    while(d2[j,]$ckdiff<0.1){j<-j+1}
    print(j)
    int_bind<-c(int_bind,j)
    h=j+1
  }
  
  data.frame(ckID=i,)
  out[out$ckID==i,]$ckdiff<-c(0,diff(out[out$ckID==i,]$ck_weight))
}


