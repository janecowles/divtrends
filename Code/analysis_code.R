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
findat$NPK[is.na(findat$NPK)]<-"Other"
#removing e098, e172 bc subsets of e001 and e002 therefore repetitive. removing e245 (for now!!!) because no n, removing e011 because uneven sampling?
# df_n <- findat[findat$Exp%in%c("e001", "e002", "e247", "e248")&!is.na(findat$SR),]
 df_n <- findat[findat$Exp%in%c("e001", "e002","e247","e248")&!is.na(findat$SR),]
 df_n <- df_n[df_n$NPK!="NPK"&df_n$TrtYear>0,]
#missing data for fencing. e002 says (cdr methods) all fences removed in 2004.
df_n$Fenced[df_n$Exp=="e002"&df_n$Year>2004]<-0

df_n$Exp <- factor(df_n$Exp)
# df_n[is.na(df_n$NAdd),]
# df_n[is.na(df_n$SR),]

df_n$N_group <- factor(ifelse(df_n$NTrt==0,"None",ifelse(df_n$NTrt<5,"Low",ifelse(df_n$NTrt<10,"Med","High"))))
df_n$N_group<-factor(df_n$N_group,levels=c("None","Low","Med","High"))

#think about fencing and burning!
ggplot(df_n,aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3"))+facet_grid(Burned~Fenced)

ggplot(df_n,aes(Year,SR,group=NAdd,color=Exp))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3","yellow","red"))+facet_grid(Burned~Fenced,labeller = label_both)




summary(df_n[df_n$Exp=="e001",])
summary(df_n[df_n$Exp=="e002",])
table(df_n$Fenced,df_n$Year,df_n$Field,df_n$Exp)

ggplot(df_n,aes(Year,SR,group=NAdd,color=Exp))+geom_point()+geom_smooth(se=F)+facet_grid(Burned~Fenced)
ggplot(df_n,aes(Year,SR,group=NAdd,color=Exp))+geom_point()+geom_smooth(se=F)+facet_grid(~Fenced)
ggplot(df_n,aes(Year,SR,group=NAdd,color=Exp))+geom_point()+geom_smooth(se=F)+facet_grid(~Burned)




ggplot(df_n,aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3"))
ggplot(df_n,aes(TrtYear,SR,group=NAdd,color=NAdd))+geom_point()+geom_smooth(se=F)+scale_color_manual(values=c("darkblue","cyan3"))


ggplot(df_n,aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_line(stat="summary",size=1.5)+facet_grid(Field~Exp)+scale_color_manual(values=c("darkblue","cyan3"))

ggplot(df_n[df_n$Exp%in%c("e001","e002"),],aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_line(stat="summary",size=1.5)+facet_grid(Exp~Field)+scale_color_manual(values=c("darkblue","cyan3"))


ggplot(df_n[df_n$Exp%in%c("e001","e002"),],aes(Year,SR,group=N_group,color=N_group))+geom_line(stat="summary",size=1.5)+facet_grid(Exp~Field)#+scale_color_manual(values=c("darkblue","cyan3"))



ggplot(df_n,aes(Year,SR,group=NAdd,color=NAdd))+geom_point()+geom_line(stat="summary",size=1.5)+facet_grid(~Exp)+scale_color_manual(values=c("darkblue","cyan3"))


ggplot(df_n,aes(TrtYear,SR,group=NAdd,color=NAdd))+geom_point()+geom_line(stat="summary",size=1.5)+facet_grid(~Exp)+scale_color_manual(values=c("darkblue","cyan3"))

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


### aside about field d
yr1_mod_d <- lm(SR~N_group, data=df_n_first[df_n_first$Exp=="e001"&df_n_first$Field=="D",])
summary(yr1_mod_d)

yr1_mod_b <- lm(SR~N_group, data=df_n_first[df_n_first$Exp=="e001"&df_n_first$Field=="B",])
summary(yr1_mod_b)

sum_mod_d <- lme(SR~N_group*TimePd,random=~1|factor(Plot), data=df_sum[df_sum$Exp=="e001"&df_sum$Field=="D",])
summary(sum_mod_d)
Anova(sum_mod_d)

ggplot(df_sum[df_sum$Exp=="e001"&df_sum$Field=="D",],aes(TimePd,SR,color=N_group,group=N_group))+geom_point()+geom_line(stat="summary")+labs(title = "Field D")


yr1_mod <- lm(SR~N_group+Exp, data=df_n_first)
summary(yr1_mod)

early_mod <- lm(SR~N_group+Exp, data=df_n_earlyAve)
summary(early_mod)

later_mod <- lm(SR~N_group+Exp, data=df_n_laterAve)
summary(later_mod)


#e248
yr1_mod_d <- lm(SR~N_group, data=df_n_first[df_n_first$Exp=="e248",])
summary(yr1_mod_d)



####USE FOR SPRING FLING?
ggplot(df_n,aes(TrtYear,SR,group=N_group,color=N_group))+
  geom_point(alpha= 0.5)+
  geom_smooth(se=F, size = 2)+
  scale_color_manual(values=c("grey","cyan3","orange","red")) +
  theme_classic() + 
  labs(x = "Treatment Year", y = "Species Richness", color = "Nitrogen \nlevel") +
  xlim(c(1,37)) +
  theme(axis.title = element_text(size = 18, face = "bold", color = "white"), 
        legend.title = element_text(size = 18, face = "bold",  color = "white"),
        axis.text = element_text(size = 16, face = "bold", color = "white"),
        plot.background = element_rect(fill = "black",colour = "black"),
        axis.line = element_line(color = "white"),
        panel.background = element_rect(fill = "black", color = "black"), 
        legend.background = element_rect(fill = "black", color = "black"),
        legend.text = element_text(size = 16, face = "bold", color = "white"))


ggplot(df_n[df_n$Exp%in%c("e001","e002"),],aes(Year,SR,group=N_group,color=N_group))+
  geom_line(stat="summary",size=1.5)+
  facet_grid(Exp~Field)+
  scale_color_manual(values=c("grey","cyan3","orange","red")) + 
  theme_linedraw()+
  labs(x = "Year", y = "Species Richness", color = "Nitrogen \nlevel") +
  theme(axis.title = element_text(size = 20, face = "bold", color = "white"), 
        legend.title = element_text(size = 20, face = "bold",  color = "white"),
        axis.text = element_text(size = 16, face = "bold", color = "white"),
        plot.background = element_rect(fill = "black",colour = "black"),
        axis.line = element_line(color = "white"),
        panel.background = element_rect(fill = "black", color = "black"), 
        legend.background = element_rect(fill = "black", color = "black"),
        legend.text = element_text(size = 16, face = "bold", color = "white"),
        panel.border = element_rect(fill = NA, linetype = 1, color = "white"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(size = 16, face = "bold", color = "black"), 
        strip.background = element_rect(fill = "white", color = "white"), 
        legend.key = element_rect(fill = "black", color = "black"))

ggplot(df_sum[df_sum$Exp%in%c("e001","e002")],aes(TimePd,SR,color=N_group,group=N_group))+
  geom_point()+
  geom_line(stat="summary")+
  facet_grid(Exp~Field)+
  scale_color_manual(values=c("grey","cyan3","orange","red")) +
  theme_linedraw()+
  labs(x = "Time Period", y = "Species Richness", color = "Nitrogen \nlevel") +
  theme(axis.title = element_text(size = 20, face = "bold", color = "white"), 
        legend.title = element_text(size = 20, face = "bold",  color = "white"),
        axis.text = element_text(size = 16, face = "bold", color = "white"),
        plot.background = element_rect(fill = "black",colour = "black"),
        axis.line = element_line(color = "white"),
        panel.background = element_rect(fill = "black", color = "black"), 
        legend.background = element_rect(fill = "black", color = "black"),
        legend.text = element_text(size = 16, face = "bold", color = "white"),
        panel.border = element_rect(fill = NA, linetype = 1, color = "white"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(size = 16, face = "bold", color = "black"), 
        strip.background = element_rect(fill = "white", color = "white"), 
        legend.key = element_rect(fill = "black", color = "black"))






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
df_f$Fenced <- factor(df_f$Fenced)
levels(df_f$Fenced) <- c("None", "Fenced")
levels(df_f$NAdd) <- c("None", "+N")

####USE FOR SPRING FLING?
ggplot(df_f,aes(TrtYear,SR,group=factor(Fenced),color=factor(Fenced)))+
  geom_point(alpha = 0.5)+
  xlim(c(1,15))+
  #geom_line(stat="summary",size=1.5)+
  geom_smooth(se=FALSE, size = 1.5) +
  facet_grid(Exp~NAdd)+
  scale_color_manual(values=c("blue","cyan3")) +
  theme_linedraw()+
  labs(x = "Treatment Year", y = "Species Richness", color = "Fenced") +
  theme(axis.title = element_text(size = 20, face = "bold", color = "white"), 
        legend.title = element_text(size = 20, face = "bold",  color = "white"),
        axis.text = element_text(size = 16, face = "bold", color = "white"),
        plot.background = element_rect(fill = "black",colour = "black"),
        axis.line = element_line(color = "white"),
        panel.background = element_rect(fill = "black", color = "black"), 
        legend.background = element_rect(fill = "black", color = "black"),
        legend.text = element_text(size = 16, face = "bold", color = "white"),
        panel.border = element_rect(fill = NA, linetype = 1, color = "white"),
        strip.text = element_text(size = 16, face = "bold", color = "black"), 
        strip.background = element_rect(fill = "white", color = "white"), 
        legend.key = element_rect(fill = "black", color = "black"))


ggplot(df_f,aes(TrtYear,SR,group=factor(Fenced),color=factor(Fenced)))+geom_line(stat="summary",size=1.5)+facet_grid(Exp~NAdd)+scale_color_manual(values=c("darkblue","cyan3"))


df_f_mod <- lme(SR~TrtYear*NAdd*factor(Fenced),random=~1|ExpPlot,data=df_f)
summary(df_f_mod)
Anova(df_f_mod)
df_f_mod1 <- lm(SR~NAdd*factor(Fenced),data=df_f[df_f$TrtYear==1,])
summary(df_f_mod1)
Anova(df_f_mod1)

View(dat.fin)


burn_sub <- dat.fin[-which(is.na(dat.fin$Burned)),]
burn_sub <- burn_sub[burn_sub$Field == "B" & burn_sub$Exp != "e098",]
#burn_sub <- burn_sub[burn_sub$Fenced == 1,]
ggplot(aes(x = TrtYear, y = SR), data = burn_sub) +
  geom_point(aes(color = factor(Burned))) +
  geom_smooth(aes(color = factor(Burned))) +
  facet_grid(NTrt~Fenced)
