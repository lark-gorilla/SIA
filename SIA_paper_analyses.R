# 13/07/17 Musanze, Rwanda
# Production of Chick and Adult blood and plasma SIA results for paper

rm(list=ls())
setwd("~/grive/phd/analyses/SIA")

dat<-read.csv("spreads/SIA_data.csv", h=T)

dat$Year<-"2015"
dat[dat$ColYr=="LHI16",]$Year<-"2016"
dat$Col<-"LHI"
dat[dat$ColYr=="HER15",]$Col<-"HER"

####### NEED TO RUN EACH TIME!! ###########
# Apply correction factor of +0.38 to chick pC whole blood from LHI in 2015
# after identified it is -0.38 inflated relative to blood cells in 2016

dat[dat$sampleType2=="whole_blood" & dat$ColYr=="LHI15",]$d13C_VDPB<-
dat[dat$sampleType2=="whole_blood" & dat$ColYr=="LHI15",]$d13C_VDPB+0.38  
dat[dat$sampleType2=="whole_blood" & dat$ColYr=="LHI15",]$sampleType2<-"blood_cells"

library(ggplot2)

p1<-ggplot(data=dat[dat$sampleType2=="blood_cells",], aes(x=d13C_VDPB, y=d15N_Air))

#png("exploration/blood_ad_ck_yr_WB_corrected.png", width = 9, height =6 , units ="in", res =600)
p1+geom_point(aes(color=Age), size=3)+theme_bw()+facet_wrap(~ColYr)
#dev.off()

## make up reults table

library(car) # for levene test
library(lme4)
library(MVN) # for Mardia's test9+

#test with manova using both N and C per factors age and location
# as per Alonso et al 2012. If significant test with mixed anova
# using nest as random effect

mardiaTest(dat[which(dat$sampleType2=="blood_cells"), 15:16])
# blood cells not multivariate normal need permanova
mardiaTest(dat[which(dat$sampleType2=="plasma"), 15:16])
# plasma multivariate normal can use manova

cor.test(dat[dat$sampleType2=="blood_cells",]$d13C_VDPB,
         dat[dat$sampleType2=="blood_cells",]$d15N_Air, method="spearman")
# sig corr
cor.test(dat[dat$sampleType2=="plasma",]$d13C_VDPB,
         dat[dat$sampleType2=="plasma",]$d15N_Air, method="spearman")
# not sig corr
#

leveneTest(d13C_VDPB~Age, data=dat[dat$sampleType2=="blood_cells",]
           , center=mean) #0.1183 0.7324
leveneTest(d13C_VDPB~Year, data=dat[dat$sampleType2=="blood_cells",]
           , center=mean)#9.8225 0.002999 **
leveneTest(d13C_VDPB~Col, data=dat[dat$sampleType2=="blood_cells",]
           , center=mean)#8.0694 0.006686 **

leveneTest(d15N_Air~Age, data=dat[dat$sampleType2=="blood_cells",]
           , center=mean)#1.2514 0.2691
leveneTest(d15N_Air~Year, data=dat[dat$sampleType2=="blood_cells",]
           , center=mean)#1.0615 0.3082
leveneTest(d15N_Air~Col, data=dat[dat$sampleType2=="blood_cells",]
           , center=mean)# 11.174 0.001656 **
# 50% ok 50% not - need permanova if including all. Age could be done with Manova IF normal?


leveneTest(d13C_VDPB~Year, data=dat[dat$sampleType2=="plasma",]
           , center=mean)#0.5687 0.4588
leveneTest(d13C_VDPB~Col, data=dat[dat$sampleType2=="plasma",]
           , center=mean)#0.4005 0.5333

leveneTest(d15N_Air~Year, data=dat[dat$sampleType2=="plasma",]
           , center=mean)#3.768 0.06516
leveneTest(d15N_Air~Col, data=dat[dat$sampleType2=="plasma",]
           , center=mean)#3.8307 0.06313 .
# manova is ok (just) and all above 0.05

# questions I actually want to answer

# Difference between adults and chicks
# Difference between colonies
# Difference between years

# plasma, difference between colonies
# plasma, difference between years

# PLasma first

plas_dat<-dat[dat$sampleType2=="plasma",]

man_plas<-manova(cbind(plas_dat$d13C_VDPB, plas_dat$d15N_Air)~Year+Col, data=plas_dat)

summary(man_plas, test="Wilks")

# significant so analyse N and C seprately

plas_c<-lm(d13C_VDPB~Year+Col, data=plas_dat)
summary(plas_c)

library(agricolae)
HSD.test(plas_c, trt='Year', console = T) # NS
HSD.test(plas_c, trt='Col', console = T) # sig @ 0.05 alpha
HSD.test(plas_c, trt=c('Col', 'Year'), console = T) # want to know diff.
#from each col YR

plas_n<-lm(d15N_Air~Year+Col, data=plas_dat)
summary(plas_n)

HSD.test(plas_n, trt='Year', console = T) # sig @ 0.05 alpha
HSD.test(plas_n, trt='Col', console = T) # sig @ 0.05 alpha
HSD.test(plas_n, trt=c('Col', 'Year'), console = T) 

# Time for blood!

library(vegan)
blood_dat<-dat[dat$sampleType2=="blood_cells",]


perm_blood<-adonis2(cbind(blood_dat$d13C_VDPB, blood_dat$d15N_Air)~
            Year+Col+Age, method="euclidean",
            data=blood_dat, by="margin", permutations=9999)
# Using Euclidean distances as per Mancini & Bugoni (2014)

summary(perm_blood)
perm_blood

blood_dat$Year<-as.factor(blood_dat$Year) # factor for kruskal.test
blood_dat$Col<-as.factor(blood_dat$Col)

kruskal.test(d13C_VDPB~Year, data=blood_dat)

kruskal.test(d13C_VDPB~Col, data=blood_dat)

kruskal.test(d13C_VDPB~Age, data=blood_dat)

kruskal.test(d15N_Air~Year, data=blood_dat)

kruskal.test(d15N_Air~Col, data=blood_dat)

kruskal.test(d15N_Air~Age, data=blood_dat)

#ok cool but really wanna know if chicks and adults vary in each col/yr
#dataset - see alonso for framework

library(nparcomp)
npar <- nparcomp(d13C_VDPB~Year, data=blood_dat, type="Tukey")
summary(npar)
