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

her_paper<-her_paper[lhi_paper$week<11,]

qplot(data=lhi_paper, y=ck_weight, x=ck_tar, colour=factor(Year))
qplot(data=her_paper, y=ck_weight, x=ck_tar, colour=factor(Year))

hist(lhi_paper$ck_weight) # looks normal
shapiro.test(lhi_paper$ck_weight) # hmm not sure about that
lhi_mod<-lm(sqrt(ck_weight)~ck_tar, data=lhi_paper)
plot(lhi_mod,2)
plot(lhi_mod,4) # outliers fine, small


lhi_paper$resids<-residuals(lhi_mod)
lhi_paper$resids2<-(lhi_paper$ck_weight)-(fitted(lhi_mod)^2) # backtransformed

qplot(data=lhi_paper,x=factor(Year), group=Year, y=resids2, geom="boxplot")

library(agricolae)
library(lsmeans)

summary(lhi_mod)

lhi_mod2<-lm(resids2~factor(Year), data=lhi_paper)
anova(lhi_mod2)

HSD.test(lhi_mod2, trt='factor(Year)', console = T, alpha=0.001)

lsmeans(lhi_mod2, specs='Year')

# now Heron

hist(her_paper$ck_weight)
hist(log(her_paper$ck_weight)) # looks better
shapiro.test(her_paper$ck_weight) # hmm
her_mod<-lm(sqrt(ck_weight)~ck_tar, data=her_paper)
plot(her_mod,1)
plot(her_mod,2)

plot(her_mod,4) # get rid of some outliers

her_paper<-her_paper[-which(row.names(her_paper)%in%c(23023, 23034, 23121)),]
# remake
her_mod<-lm(sqrt(ck_weight)~ck_tar, data=her_paper)

her_paper$resids<-residuals(her_mod)
her_paper$resids2<-(her_paper$ck_weight)-(fitted(her_mod)^2) # backtransformed

qplot(data=her_paper,x=factor(Year), group=Year, y=resids2, geom="boxplot")

library(agricolae)

summary(her_mod)

her_mod2<-lm(resids2~factor(Year), data=her_paper)
anova(her_mod2)

HSD.test(her_mod2, trt='factor(Year)', console = T, alpha=0.001)

HSD.test(her_mod2, trt='factor(Year)', console = T, alpha=0.05)

lsmeans(her_mod2, specs='Year')

#test if difference between Heron and LHI colony
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


