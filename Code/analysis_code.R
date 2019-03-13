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
library(car)

#read file in or run kk_code
findat <- read.csv(here::here("Data", "findat.csv"))
findat$NAdd<-as.factor(findat$NAdd)
#removing e098, e172 bc subsets of e001 and e002 therefore repetitive. removing e245 (for now!!!) because no n, removing e011 because uneven sampling?
# df_n <- findat[findat$Exp%in%c("e001", "e002", "e247", "e248")&!is.na(findat$SR),]
 df_n <- findat[findat$Exp%in%c("e001", "e002")&!is.na(findat$SR),]

#missing data for fencing. e002 says (cdr methods) all fences removed in 2004.
df_n$Fenced[df_n$Exp=="e002"&df_n$Year>2004]<-0

df_n$Exp <- factor(df_n$Exp)
# df_n[is.na(df_n$NAdd),]
# df_n[is.na(df_n$SR),]

df_n$N_group <- factor(ifelse(df_n$NTrt==0,"None",ifelse(df_n$NTrt<5,"Low",ifelse(df_n$NTrt<10,"Med","High"))))
df_n$N_group<-factor(df_n$N_group,levels=c("None","Low","Med","High"))

#think about fencing and burning!
ggplot(df_n,aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3"))+facet_grid(Burned~Fenced)

ggplot(df_n,aes(Year,SR,group=NAdd,color=Field))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3","yellow","red"))+facet_grid(Burned~Fenced,labeller = label_both)

ggplot(df_n[df_n$NTrt>0,],aes(Year,SR))+geom_point()+geom_smooth(se=F)



summary(df_n[df_n$Exp=="e001",])
summary(df_n[df_n$Exp=="e002",])
table(df_n$Fenced,df_n$Year,df_n$Field,df_n$Exp)

ggplot(df_n,aes(Year,SR,group=NAdd,color=Exp))+geom_point()+geom_smooth(se=F)+facet_grid(Burned~Fenced)
ggplot(df_n,aes(Year,SR,group=NAdd,color=Exp))+geom_point()+geom_smooth(se=F)+facet_grid(1~Fenced)
ggplot(df_n,aes(Year,SR,group=NAdd,color=Exp))+geom_point()+geom_smooth(se=F)+facet_grid(1~Burned)




ggplot(df_n,aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3"))
ggplot(df_n,aes(TrtYear,SR,group=NAdd,color=NAdd))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3"))

ggplot(df_n,aes(TrtYear,SR,group=N_group,color=N_group))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3","yellow","red"))

ggplot(df_n,aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_line(stat="summary",size=1.5)+facet_grid(Field~Exp)+scale_color_manual(values=c("darkblue","cyan3"))

ggplot(df_n[df_n$Exp%in%c("e001","e002"),],aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_line(stat="summary",size=1.5)+facet_grid(Exp~Field)+scale_color_manual(values=c("darkblue","cyan3"))

ggplot(df_n[df_n$Exp%in%c("e001","e002"),],aes(Year,SR,group=NTrt,color=NTrt))+geom_line(stat="summary",size=1.5)+facet_grid(Exp~Field)#+scale_color_manual(values=c("darkblue","cyan3"))

ggplot(df_n[df_n$Exp%in%c("e001","e002"),],aes(Year,SR,group=N_group,color=N_group))+geom_line(stat="summary",size=1.5)+facet_grid(Exp~Field)#+scale_color_manual(values=c("darkblue","cyan3"))



ggplot(df_n,aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_line(stat="summary",size=1.5)+facet_grid(1~Exp)+scale_color_manual(values=c("darkblue","cyan3"))


ggplot(df_n,aes(TrtYear,SR,group=NAdd,color=NAdd))+geom_point()+geom_line(stat="summary",size=1.5)+facet_grid(1~Exp)+scale_color_manual(values=c("darkblue","cyan3"))

setDT(df_n)
df_n_first <- df_n[df_n$TrtYear==1,]
df_n_first <-df_n_first[,.(SR=mean(SR,na.rm=T)),by=.(Exp,Field,Plot,NTrt,NAdd,N_group,Subplot,Block,Water)]
df_n_first$TimePd <- "First"
df_n_early <- df_n[df_n$TrtYear%in%c(3:4),]
df_n_earlyAve <- df_n_early[,.(SR=mean(SR,na.rm=T)),by=.(Exp,Field,Plot,NTrt,NAdd,N_group,Subplot,Block,Water)]
df_n_earlyAve$TimePd <- "Early"
df_n_later <- df_n[df_n$TrtYear%in%c(8:10),]
df_n_laterAve <- df_n_later[,.(SR=mean(SR,na.rm=T)),by=.(Exp,Field,Plot,NTrt,NAdd,N_group,Subplot,Block,Water)]
df_n_laterAve$TimePd <- "Later"


df_sum <- rbind(df_n_first,df_n_earlyAve,df_n_laterAve)
df_sum$TimePd<-factor(df_sum$TimePd,levels=c("First","Early","Later"))

ggplot(df_sum,aes(TimePd,SR,color=NAdd,group=NAdd))+geom_point()+geom_line(stat="summary")

ggplot(df_sum,aes(TimePd,SR,color=N_group,group=N_group))+geom_point()+geom_line(stat="summary")

### aside about field d
yr1_mod_d <- lm(SR~N_group, data=df_n_first[df_n_first$Exp=="e001"&df_n_first$Field=="D",])
summary(yr1_mod_d)

yr1_mod_b <- lm(SR~N_group, data=df_n_first[df_n_first$Exp=="e001"&df_n_first$Field=="B",])
summary(yr1_mod_b)

sum_mod_d <- lme(SR~N_group*TimePd,random=~1|factor(Plot), data=df_sum[df_sum$Exp=="e001"&df_sum$Field=="D",])
summary(sum_mod_d)
Anova(sum_mod_d)

ggplot(df_sum[df_sum$Exp=="e001"&df_sum$Field=="D",],aes(TimePd,SR,color=N_group,group=N_group))+geom_point()+geom_line(stat="summary")+labs(title = "Field D")

ggplot(df_sum[df_sum$Exp=="e001",],aes(TimePd,SR,color=N_group,group=N_group))+geom_point()+geom_line(stat="summary")+facet_wrap(facets = "Field")

ggplot(df_sum,aes(TimePd,SR,color=N_group,group=N_group))+geom_point()+geom_line(stat="summary")+facet_wrap(facets = "Field")

ggplot(df_sum,aes(TimePd,SR,color=N_group,group=N_group))+geom_point()+geom_line(stat="summary")+facet_grid(Exp~Field)



yr1_mod <- lm(SR~N_group+Exp, data=df_n_first)
summary(yr1_mod)

early_mod <- lm(SR~N_group+Exp, data=df_n_earlyAve)
summary(early_mod)

later_mod <- lm(SR~N_groupExp, data=df_n_laterAve)
summary(later_mod)


#e248
yr1_mod_d <- lm(SR~N_group, data=df_n_first[df_n_first$Exp=="e248",])
summary(yr1_mod_d)









#ok i added in this "superlater" stuff for the last many years of e001 and e002 but this is treading on another paper of mine and not relevant to this paper. It also leads to an unbalanced design -- i picked the last 3 groups bc there was a balance across experiments across time, this is only e001 and e002.
# 
# df_n_superlater <- df_n[df_n$TrtYear%in%c(30:37),]
# df_n_superlaterAve <- df_n_superlater[,.(SR=mean(SR,na.rm=T)),by=.(Exp,Field,Plot,NTrt,NAdd,N_group,Subplot,Block,Water)]
# df_n_superlaterAve$TimePd <- "SuperLater"
# 
# df_sum <- rbind(df_n_first,df_n_earlyAve,df_n_laterAve,df_n_superlaterAve)
# df_sum$TimePd<-factor(df_sum$TimePd,levels=c("First","Early","Later","SuperLater"))
# 
# ggplot(df_sum,aes(TimePd,SR,color=NAdd,group=NAdd))+geom_point()+geom_line(stat="summary")
# 
# ggplot(df_sum,aes(TimePd,SR,color=NTrt,group=NTrt))+geom_point()+geom_line(stat="summary")


### fencing!!!!

e001 <- findat[findat$Exp%in%c("e001")&!is.na(findat$SR),]
e001 <- e001[e001$Field=="C"&e001$Year>2004,]
e001$TrtYear <- e001$Year-2004
e247 <- findat[findat$Exp%in%c("e247")&!is.na(findat$SR),]
e247$NAdd[e247$NPK=="Other"]<-NA
df_f <- rbind(e001,e247)
df_f <- df_f[!is.na(df_f$NAdd),]
df_f$ExpPlot <- as.factor(paste0(df_f$Exp,df_f$Plot))
ggplot(df_f,aes(TrtYear,SR,group=factor(Fenced),color=factor(Fenced)))+geom_smooth()+facet_grid(Exp~NAdd)

df_f_mod <- lme(SR~NAdd*factor(Fenced),random=~1|ExpPlot,data=df_f)
summary(df_f_mod)

df_f_mod <- lm(SR~NAdd*factor(Fenced),data=df_f[df_f$TrtYear==1,])

