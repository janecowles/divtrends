### cowles
### analysis code
### use after running kk_code or use with pre-processed findat.csv

#### load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(data.table)
library(nlme)
library(ggplot2)

#read file in or run kk_code
findat <- read.csv(here::here("Data", "findat.csv"))
findat$NAdd<-as.factor(findat$NAdd)
#removing e098, e172 bc subsets of e001 and e002 therefore repetitive. removing e245 (for now!!!) because no n, removing e011 because uneven sampling?
df_n <- findat[findat$Exp%in%c("e001", "e002",  "e247", "e248")&!is.na(findat$SR),]
df_n$Exp <- factor(df_n$Exp)
# df_n[is.na(df_n$NAdd),]
# df_n[is.na(df_n$SR),]

#think about fencing and burning!

ggplot(df_n,aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3"))
ggplot(df_n,aes(TrtYear,SR,group=NAdd,color=NAdd))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3"))

ggplot(df_n,aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_line(stat="summary",size=1.5)+facet_grid(1~Exp)+scale_color_manual(values=c("darkblue","cyan3"))
ggplot(df_n,aes(TrtYear,SR,group=NAdd,color=NAdd))+geom_point()+geom_line(stat="summary",size=1.5)+facet_grid(1~Exp)+scale_color_manual(values=c("darkblue","cyan3"))
setDT(df_n)
df_n_first <- df_n[df_n$TrtYear==1,]
df_n_first <-df_n_first[,.(SR=mean(SR,na.rm=T)),by=.(Exp,Field,Plot,NTrt,NAdd,Subplot,Block,Water)]
df_n_first$TimePd <- "First"
df_n_early <- df_n[df_n$TrtYear%in%c(3:4),]
df_n_earlyAve <- df_n_early[,.(SR=mean(SR,na.rm=T)),by=.(Exp,Field,Plot,NTrt,NAdd,Subplot,Block,Water)]
df_n_earlyAve$TimePd <- "Early"
df_n_later <- df_n[df_n$TrtYear%in%c(8:10),]
df_n_laterAve <- df_n_later[,.(SR=mean(SR,na.rm=T)),by=.(Exp,Field,Plot,NTrt,NAdd,Subplot,Block,Water)]
df_n_laterAve$TimePd <- "Later"


#ok i added in this "superlater" stuff for the last many years of e001 and e002 but this is treading on another paper of mine and not relevant to this paper. It also leads to an unbalanced design -- i picked the last 3 groups bc there was a balance across experiments across time, this is only e001 and e002.

df_n_superlater <- df_n[df_n$TrtYear%in%c(30:37),]
df_n_superlaterAve <- df_n_superlater[,.(SR=mean(SR,na.rm=T)),by=.(Exp,Field,Plot,NTrt,NAdd,Subplot,Block,Water)]
df_n_superlaterAve$TimePd <- "SuperLater"

df_sum <- rbind(df_n_first,df_n_earlyAve,df_n_laterAve,df_n_superlaterAve)
df_sum$TimePd<-factor(df_sum$TimePd,levels=c("First","Early","Later","SuperLater"))

ggplot(df_sum,aes(TimePd,SR,color=NAdd,group=NAdd))+geom_point()+geom_line(stat="summary")
