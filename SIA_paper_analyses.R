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


## NEED TO REWORK AND TIDY SCRIPT
# Comparing adults to chicks at each colony:
# t-tests or Mann-whitey U for C and N seperately 

# Compring adult, then chick, then adult plasma between ColYrs:
# MANOVA (followed by tukey) or PERMANOVA followed by Kruskal wallace (followed by post hoc) fo C and N seperately

# spearmans rank or significant manova first to show variable significance

# TESTING differences between colyr for adult plasma, blood and ck blood

ad_plasma<-dat[dat$sampleType2=="plasma",]
ad_blood<-dat[dat$sampleType2=="blood_cells" & dat$Age=="AD",]
ck_blood<-dat[dat$sampleType2=="blood_cells" & dat$Age=="CK",]

# multivariate normal test

mardiaTest(ck_blood[,13:14])
# multivariate normal
mardiaTest(ad_blood[,13:14])
# multivariate normal
mardiaTest(ad_plasma[,13:14])
# plasma multivariate normal can use manova

# univariate normal test and homogeniety of variance

leveneTest(d13C_VDPB~ColYr, data=ck_blood, center=mean)
shapiro.test(ck_blood$d13C_VDPB)
leveneTest(d13C_VDPB~ColYr, data=ad_blood, center=mean)
shapiro.test(ad_blood$d13C_VDPB)
leveneTest(d13C_VDPB~ColYr, data=ad_plasma, center=mean)
shapiro.test(ad_plasma$d13C_VDPB)
# Carbon all fine

leveneTest(d15N_Air~ColYr, data=ck_blood, center=mean)
shapiro.test(ck_blood$d15N_Air) # Not NORMAL!
leveneTest(d15N_Air~ColYr, data=ad_blood, center=mean)
shapiro.test(ad_blood$d15N_Air) # Not NORMAL!
leveneTest(d15N_Air~ColYr, data=ad_plasma, center=mean)
shapiro.test(ad_plasma$d15N_Air)

# Manovas for each but will not use

man_plas<-manova(cbind(ad_plasma$d13C_VDPB, ad_plasma$d15N_Air)~ColYr,
                 data=ad_plasma)
summary(man_plas, test="Wilks")

man_plas<-manova(cbind(ad_blood$d13C_VDPB, ad_blood$d15N_Air)~ColYr,
                 data=ad_blood)
summary(man_plas, test="Wilks")

man_plas<-manova(cbind(ck_blood$d13C_VDPB, ck_blood$d15N_Air)~ColYr,
                 data=ck_blood)
summary(man_plas, test="Wilks")

# Manovas are fine and assumptions met HOWEVER, when I do post hoc tests
# Nitrogen is bimodal in whole blood and not normally distributed (according to Shapiro wilks)
# I'll run kruskal-wallace for these so not sure if valid?

#look for post hoc differnces


# Adult plasma
plas_c<-lm(d13C_VDPB~ColYr, data=ad_plasma)
summary(plas_c) # only just significant

library(agricolae)
HSD.test(plas_c, trt='ColYr', console = T) # two 2015 samples sig diff 
#from each other, but not from LHI2016

plas_n<-lm(d15N_Air~ColYr, data=ad_plasma)
summary(plas_n)

HSD.test(plas_n, trt='ColYr', console = T) # Heron and LHI 2015 the same, 
#LHI 2016 sig diff from both

# Adult blood
blod_Ac<-lm(d13C_VDPB~ColYr, data=ad_blood)
summary(blod_Ac)

HSD.test(blod_Ac, trt='ColYr', console = T) # Heron and LHI 2015 the same, 
#LHI 2016 sig diff from both

# kruskal from agricolae runs both a kruskal-wallace and post hoc comp!
kruskal(ad_blood$d15N_Air, ad_blood$ColYr, p.adj="bonferroni", console=T)
# added bonferroni for extra power?
# Heron and LHI 2015 the same, LHI 2016 sig diff from both

# Chick blood
blod_c<-lm(d13C_VDPB~ColYr, data=ck_blood)
summary(blod_c)

HSD.test(blod_c, trt='ColYr', console = T) # Heron and LHI 2016 the same, 
#LHI 2015 sig diff from both

kruskal(ck_blood$d15N_Air, ck_blood$ColYr, p.adj="bonferroni", console=T)
# added bonferroni for extra power?
# Heron and LHI 2015 the same, LHI 2016 sig diff from both


# Now differences between adults and chicks within each colony-year
# sampling combination

lhi15_blood<-dat[dat$sampleType2=="blood_cells" & dat$ColYr=="LHI15",]
lhi16_blood<-dat[dat$sampleType2=="blood_cells" & dat$ColYr=="LHI16",]
her15_blood<-dat[dat$sampleType2=="blood_cells" & dat$ColYr=="HER15",]


leveneTest(d13C_VDPB~Age, data=lhi15_blood, center=mean)# not normal
shapiro.test(lhi15_blood$d13C_VDPB)# just under 0.05
leveneTest(d13C_VDPB~Age, data=lhi16_blood, center=mean)#fine
shapiro.test(lhi16_blood$d13C_VDPB)#fine
leveneTest(d13C_VDPB~Age, data=her15_blood, center=mean)#fine
shapiro.test(her15_blood$d13C_VDPB)#fine


leveneTest(d15N_Air~Age, data=lhi15_blood, center=mean)#fine
shapiro.test(lhi15_blood$d15N_Air)#fine
leveneTest(d15N_Air~Age, data=lhi16_blood, center=mean)#fine
shapiro.test(lhi16_blood$d15N_Air)#fine
leveneTest(d15N_Air~Age, data=her15_blood, center=mean)#fine
shapiro.test(her15_blood$d15N_Air)#not homogeneous

# Manovas, not sure if using?

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

t.test(d13C_VDPB ~ Age, data=lhi16_blood)

t.test(d13C_VDPB ~ Age, data=her15_blood)

wilcox.test(d13C_VDPB ~ Age, data=lhi15_blood)

t.test(d15N_Air ~ Age, data=lhi16_blood)

t.test(d15N_Air ~ Age, data=lhi15_blood)

t.test(d15N_Air ~ Age, data=man_lhi15_NO_OUTLIER)

wilcox.test(d15N_Air ~ Age, data=her15_blood)

# plot data
rw_blood<-dat[dat$sampleType2=="blood_cells",]
mn_blood<-rbind(data.frame(aggregate(d15N_Air~ Age+ColYr, rw_blood, mean), id="mean"),
                data.frame(aggregate(d15N_Air~ Age+ColYr, rw_blood, sd), id="sd"))
d13C_VDPB<-c(aggregate(d13C_VDPB~ Age+ColYr, rw_blood, mean)[,3], 
  aggregate(d13C_VDPB~ Age+ColYr, rw_blood, sd)[,3])
mn_blood<-cbind(mn_blood, d13C_VDPB)

p1<-ggplot(data=mn_blood[mn_blood$id=="mean",], aes(x=d13C_VDPB, y=d15N_Air))

plot1<-p1+geom_point(data=rw_blood, aes(shape=ColYr, colour=Age))+
  
  geom_errorbar(aes(ymin=(mn_blood[mn_blood$id=="mean",]$d15N_Air-
                      mn_blood[mn_blood$id=="sd",]$d15N_Air), ymax=
                      (mn_blood[mn_blood$id=="mean",]$d15N_Air+
                         mn_blood[mn_blood$id=="sd",]$d15N_Air)))+
  
  geom_errorbarh(aes(xmin=(mn_blood[mn_blood$id=="mean",]$d13C_VDPB-
                            mn_blood[mn_blood$id=="sd",]$d13C_VDPB), xmax=
                      (mn_blood[mn_blood$id=="mean",]$d13C_VDPB+
                         mn_blood[mn_blood$id=="sd",]$d13C_VDPB)))+
  
  geom_point(aes(shape=ColYr, colour=Age), size=3)+
  xlab(expression(δ^{13}~"C (‰)"))+
  ylab(expression(δ^{15}~"N (‰)"))+
  theme_bw()+theme(legend.position=0)

jpeg("paper_plots/SIA_blood.jpg", width = 3, height =3 , units ="in", res =600)
plot1
dev.off()

# remove LHI15 adult outlier

mn_blood[mn_blood$ColYr=="LHI15" & mn_blood$Age=="AD" & mn_blood$id=="mean",]$d15N_Air<-
  mean(rw_blood[rw_blood$ColYr=="LHI15" & rw_blood$Age=="AD" & rw_blood$d15N_Air<10.5,]$d15N_Air)


mn_blood[mn_blood$ColYr=="LHI15" & mn_blood$Age=="AD" & mn_blood$id=="sd",]$d15N_Air<-
  sd(rw_blood[rw_blood$ColYr=="LHI15" & rw_blood$Age=="AD" & rw_blood$d15N_Air<10.5,]$d15N_Air)

#edit mn_blood[mn_blood$id=="mean",]
mn_blood$Age2<-"black"
mn_blood[mn_blood$Age=="CK",]$Age2<-NA

rw_blood$Age2<-"black"
rw_blood[rw_blood$Age=="CK",]$Age2<-NA

p1<-ggplot(data=mn_blood[mn_blood$id=="mean",], aes(x=d13C_VDPB, y=d15N_Air))

plot1<-p1+
  geom_point(data=rw_blood, aes(shape=ColYr, fill=Age2))+
  
  geom_errorbar(aes(ymin=(mn_blood[mn_blood$id=="mean",]$d15N_Air-
                            mn_blood[mn_blood$id=="sd",]$d15N_Air), ymax=
                      (mn_blood[mn_blood$id=="mean",]$d15N_Air+
                         mn_blood[mn_blood$id=="sd",]$d15N_Air)))+
  
  geom_errorbarh(aes(xmin=(mn_blood[mn_blood$id=="mean",]$d13C_VDPB-
                             mn_blood[mn_blood$id=="sd",]$d13C_VDPB), xmax=
                       (mn_blood[mn_blood$id=="mean",]$d13C_VDPB+
                          mn_blood[mn_blood$id=="sd",]$d13C_VDPB)))+
  
  geom_point(aes(shape=ColYr, fill=Age2), size=4)+
  scale_shape_manual(values=c(21,22,23)) +             
  scale_fill_identity(na.value=NA, guide="none")+
  
  xlab(expression(δ^{13}~"C (‰)"))+
  ylab(expression(δ^{15}~"N (‰)"))+
  theme(legend.position=0,
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill=NA))

jpeg("paper_plots/SIA_blood2_nooutlier.jpg", width = 6, height =6 , units ="in", res =600)
plot1
dev.off()


# Ununsed

#perm_blood<-adonis2(cbind(blood_dat$d13C_VDPB, blood_dat$d15N_Air)~
#                      ColYr, method="euclidean",
#                    data=blood_dat, by="margin", permutations=9999)
# Using Euclidean distances as per Mancini & Bugoni (2014)

#perm_blood

#library(nparcomp)
#npar <- nparcomp(d13C_VDPB~ColYr, data=blood_dat, type="Tukey")
#summary(npar)


