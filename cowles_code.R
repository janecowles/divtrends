### jane cowles
### code 14 November 2018

#clear memory
rm(list=ls())
graphics.off()

### general packages that I use often
library(plyr)
library(nlme)
library(lme4)
library(car)
library(ggplot2)
library(vegan)
library(readxl)
library(data.table)
library(tidyr)
library(gridExtra)
library(RColorBrewer)


#set wd
setwd("~/Dropbox/UMN Postdoc/divtrends")

# read in e001

e1 <- read.csv("~/Dropbox/UMN Postdoc/CDR DATA/e001mega18.csv")
str(e1)
e1<-e1[!is.na(e1$Year),]
ggplot(e1,aes(Year,TotBio,color=NTrt))+geom_point()
e1<-e1[e1$TotBio<4000,]

#ignoring fencing and burning for now bc it is weird.
names(e1)
e1$ExpYear <- 1+e1$Year-min(e1$Year,na.rm = T)

e1_yr1 <- e1[e1$ExpYear==1,]
e1_yr3 <- e1[e1$ExpYear==3,]
e1_yr10 <- e1[e1$ExpYear==10,]
e1_yrmax <- e1[e1$ExpYear==max(e1$ExpYear),]

e1_yr1MOD <- lm(Sr~NAdd+Field,e1_yr1)
summary(e1_yr1MOD)
ggplot(e1_yr1,aes(NAdd,Sr,color=Field))+geom_smooth()+geom_point()+labs(title="e1 yr 1")

e1_yr3MOD <- lm(Sr~NAdd+Field,e1_yr3)
summary(e1_yr3MOD)
ggplot(e1_yr3,aes(NAdd,Sr,color=Field))+geom_smooth()+geom_point()+labs(title="e1 yr 3")

e1_yr10MOD <- lm(Sr~NAdd+Field,e1_yr10)
summary(e1_yr10MOD)
ggplot(e1_yr10,aes(NAdd,Sr,color=Field))+geom_smooth()+geom_point()+labs(title="e1 yr 10")

e1_yrmaxMOD <- lm(Sr~NAdd+Field,e1_yrmax)
summary(e1_yrmaxMOD)
ggplot(e1_yrmax,aes(NAdd,Sr,color=Field))+geom_smooth()+geom_point()+labs(title="e1 yr max***")






e2 <- read.csv("~/Dropbox/UMN Postdoc/CDR DATA/2018 e002Mega.csv")
str(e2)
ggplot(e2,aes(Year,TotBio,color=Ntrt))+geom_point()
e2 <- e2[e2$TotBio<4000,]
e2<-e2[!is.na(e2$Exp),]
e2 <- e2[e2$NtrtRec==1,]
e2 <- e2[e2$BurnTrt==0,]
e2$ExpYear <- 1+e2$Year-min(e2$Year,na.rm = T)

### cut out peripheral treatments
table(e2$Ntrt,e2$NtrtRec)
table(e2$BurnTrt,e2$BurnRec,e2$Year)
table(e2$BurnTrt,e2$Year)
table(e2$Fencing,e2$Year) #not ideal

e2_yr1 <- e2[e2$ExpYear==1,]
e2_yr3 <- e2[e2$ExpYear==3,]
e2_yr10 <- e2[e2$ExpYear==10,]
e2_yrmax <- e2[e2$ExpYear==max(e2$ExpYear),]

e2_yr1MOD <- lm(Sr~NAdd+Field,e2_yr1)
summary(e2_yr1MOD)
ggplot(e2_yr1,aes(NAdd,Sr,color=Field))+geom_smooth()+geom_point()+labs(title="e2 yr 1")

e2_yr3MOD <- lm(Sr~NAdd+Field,e2_yr3)
summary(e2_yr3MOD)
ggplot(e2_yr3,aes(NAdd,Sr,color=Field))+geom_smooth()+geom_point()+labs(title="e2 yr 3")

e2_yr10MOD <- lm(Sr~NAdd+Field,e2_yr10)
summary(e2_yr10MOD)
ggplot(e2_yr10,aes(NAdd,Sr,color=Field))+geom_smooth()+geom_point()+labs(title="e2 yr 10")

e2_yrmaxMOD <- lm(Sr~NAdd+Field,e2_yrmax)
summary(e2_yrmaxMOD)
ggplot(e2_yrmax,aes(NAdd,Sr,color=Field))+geom_smooth()+geom_point()+labs(title="e2 yr 1")
