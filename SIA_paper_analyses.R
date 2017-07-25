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

mardiaTest(dat[which(dat$sampleType2=="blood_cells"), 13:14])
# blood cells not multivariate normal need permanova
mardiaTest(dat[which(dat$sampleType2=="blood_cells" &
                       dat$ColYr=="LHI15"), 13:14])
# blood cells LHI15 are multivariate normal can use manova
mardiaTest(dat[which(dat$sampleType2=="blood_cells"&
                       dat$ColYr=="HER15"), 13:14])
# blood cells LHI14 are multivariate normal can use manova
mardiaTest(dat[which(dat$sampleType2=="blood_cells"&
                       dat$ColYr=="LHI16"), 13:14])
# blood cells LHI16 are multivariate normal can use manova
mardiaTest(dat[which(dat$sampleType2=="plasma"), 13:14])
# plasma multivariate normal can use manova

cor.test(dat[dat$sampleType2=="blood_cells",]$d13C_VDPB,
         dat[dat$sampleType2=="blood_cells",]$d15N_Air, method="spearman")
# sig corr

cor.test(dat[dat$sampleType2=="blood_cells" &
               dat$ColYr=="LHI15",]$d13C_VDPB,
         dat[dat$sampleType2=="blood_cells"&
               dat$ColYr=="LHI15",]$d15N_Air, method="spearman")
# not sig

cor.test(dat[dat$sampleType2=="blood_cells"&
               dat$ColYr=="HER15",]$d13C_VDPB,
         dat[dat$sampleType2=="blood_cells"&
               dat$ColYr=="HER15",]$d15N_Air, method="spearman")
# sig corr

cor.test(dat[dat$sampleType2=="blood_cells"&
               dat$ColYr=="LHI16",]$d13C_VDPB,
         dat[dat$sampleType2=="blood_cells"&
               dat$ColYr=="LHI16",]$d15N_Air, method="spearman")
# not sig corr

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

# blood col_yr age
leveneTest(d13C_VDPB~Age, data=dat[dat$sampleType2=="blood_cells" & dat$ColYr=="LHI15",]
           , center=mean) #13.455 0.002533 ** sig BUT gonna run as manova
leveneTest(d13C_VDPB~Age, data=dat[dat$sampleType2=="blood_cells"& dat$ColYr=="LHI16",]
           , center=mean)#0.0337 0.8569
leveneTest(d13C_VDPB~Age, data=dat[dat$sampleType2=="blood_cells"& dat$ColYr=="HER15",]
           , center=mean)#0.3016 0.5915

leveneTest(d15N_Air~Age, data=dat[dat$sampleType2=="blood_cells" & dat$ColYr=="LHI15",]
           , center=mean) #13.0811 0.1011
leveneTest(d15N_Air~Age, data=dat[dat$sampleType2=="blood_cells"& dat$ColYr=="LHI16",]
           , center=mean)# 1.7115 0.2119
leveneTest(d15N_Air~Age, data=dat[dat$sampleType2=="blood_cells"& dat$ColYr=="HER15",]
           , center=mean)#2.5821 0.1304

 #plasma
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

man_plas<-manova(cbind(plas_dat$d13C_VDPB, plas_dat$d15N_Air)~ColYr, data=plas_dat)

summary(man_plas, test="Wilks")

# significant so analyse N and C seprately

plas_c<-lm(d13C_VDPB~ColYr, data=plas_dat)
summary(plas_c)

library(agricolae)
HSD.test(plas_c, trt='ColYr', console = T) # sig @ 0.05 alpha

plas_n<-lm(d15N_Air~ColYr, data=plas_dat)
summary(plas_n)

HSD.test(plas_n, trt='ColYr', console = T) # sig @ 0.05 alpha

# Time for blood!

library(vegan)
blood_dat<-dat[dat$sampleType2=="blood_cells",]


perm_blood<-adonis2(cbind(blood_dat$d13C_VDPB, blood_dat$d15N_Air)~
            ColYr, method="euclidean",
            data=blood_dat, by="margin", permutations=9999)
# Using Euclidean distances as per Mancini & Bugoni (2014)

perm_blood

library(nparcomp)
npar <- nparcomp(d13C_VDPB~ColYr, data=blood_dat, type="Tukey")
summary(npar)

npar <- nparcomp(d15N_Air~ColYr, data=blood_dat, type="Tukey")
summary(npar)

# Now blood age comparisons per sampling ColYr

lhi15_blood<-blood_dat[blood_dat$ColYr=="LHI15",]
lhi16_blood<-blood_dat[blood_dat$ColYr=="LHI16",]
her15_blood<-blood_dat[blood_dat$ColYr=="HER15",]


man_lhi16<-manova(cbind(lhi16_blood$d13C_VDPB, lhi16_blood$d15N_Air)~Age, data=lhi16_blood)

summary(man_lhi16, test="Wilks")

man_her15<-manova(cbind(her15_blood$d13C_VDPB, her15_blood$d15N_Air)~Age, data=her15_blood)

summary(man_her15, test="Wilks")

man_lhi15<-manova(cbind(lhi15_blood$d13C_VDPB, lhi15_blood$d15N_Air)~Age, data=lhi15_blood)

summary(man_lhi15, test="Wilks")

man_lhi15_NO_OUTLIER<-lhi15_blood[lhi15_blood$d15N_Air<10.5,]

man_lhi15_NOL<-manova(cbind(man_lhi15_NO_OUTLIER$d13C_VDPB, man_lhi15_NO_OUTLIER$d15N_Air)~Age, data=man_lhi15_NO_OUTLIER)

summary(man_lhi15_NOL, test="Wilks")

# analyse N and C seprately

c_lhi16<-lm(d13C_VDPB~Age, data=lhi16_blood)
summary(c_lhi16)
HSD.test(c_lhi16, trt='Age', console = T)

c_her15<-lm(d13C_VDPB~Age, data=her15_blood)
summary(c_her15)
HSD.test(c_her15, trt='Age', console = T)

c_lhi15<-lm(d13C_VDPB~Age, data=lhi15_blood)
summary(c_lhi15)
HSD.test(c_lhi15, trt='Age', console = T)

c_lhi15_NOL<-lm(d13C_VDPB~Age, data=man_lhi15_NO_OUTLIER)
summary(c_lhi15_NOL)
HSD.test(c_lhi15_NOL, trt='Age', console = T)

n_lhi16<-lm(d15N_Air~Age, data=lhi16_blood)
summary(n_lhi16)
HSD.test(n_lhi16, trt='Age', console = T)

n_her15<-lm(d15N_Air~Age, data=her15_blood)
summary(n_her15)
HSD.test(n_her15, trt='Age', console = T)

n_lhi15<-lm(d15N_Air~Age, data=lhi15_blood)
summary(n_lhi15)
HSD.test(n_lhi15, trt='Age', console = T)

n_lhi15_NOL<-lm(d15N_Air~Age, data=man_lhi15_NO_OUTLIER)
summary(n_lhi15_NOL)
HSD.test(n_lhi15_NOL, trt='Age', console = T)



# Ununsed
#kruskal.test(d13C_VDPB~Year, data=blood_dat)
#kruskal.test(d15N_Air~Year, data=blood_dat)

