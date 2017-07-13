# 13/07/17 Musanze, Rwanda
# Production of Chick and Adult blood and plasma SIA results for paper

rm(list=ls())
setwd("~/grive/phd/analyses/SIA")

dat<-read.csv("spreads/SIA_data.csv", h=T)

library(ggplot2)