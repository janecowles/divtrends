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

e1 <- read.csv("~/Dropbox/UMN Postdoc/DATA/e001mega18.csv")
str(e1)
ggplot(e1,aes(Year,TotBio,color=NTrt))+geom_point()

###oh right, trees.