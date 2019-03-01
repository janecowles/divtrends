# DivTrends Project
# Overall objective: Here we aim to determine how species richness responds to 
# long-term environmental manipulations in (1) the first year, (2) 0ver short 
# time periods (3 years), and (3) over long time periods (10+ years)
# We also aim to determine which manipulation leads to the greatest change in 
# diversity for a given time period.

# Create datafile with all of the experiments
# Columns: Experiment, Experiment Year, Species Richness, Treatment
# Will need to thinkk about how to code the treatments depending on if we want continuous or categorical variables. 
# Perhaps have a column for each treatment type and NA when that treatment is not applied in the experiment

#### load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(here)


#### load data
e001 <- read.csv(here::here("Data", "e001_dat.csv")) # Nitrogen addition, 82-present
e002 <- read.csv(here::here("Data", "e002_dat.csv")) # Nitrogen addition, 82-present
e011 <- read.csv(here::here("Data", "e011_dat.csv")) # Nitrogen addition, 84-92
e098 <- read.csv(here::here("Data", "e098_dat.csv")) # Nitrogen addition and fire, 82-11; subset of e002
e172 <- read.csv(here::here("Data", "e172_dat.csv")) # Nitrogen addition and herbivory, 82 - 11; subset of e001
e245 <- read.csv(here::here("Data", "e245_dat.csv")) # Enemy removal, 08-16
e247 <- read.csv(here::here("Data", "e247_dat.csv")) # Nitrogen addition and herbivory, 07 - 15
e248 <- read.csv(here::here("Data", "e248_dat.csv")) # Nitrogen and water addition, 07-16

# Need all column names to be the same for combining the datasets
# e001
e001 <- e001[,c(1:5,7:9)]
e001$Exp <- "e001"
names(e001)[8] <- "SR"
e001$TrtYear <- e001$Year - min(e001$Year) + 1
e001 <- e001[e001$NTrt != 9,] # Get rid of control with micronutrients
e001$NTrt <- e001$NAdd
e001$NAdd <- ifelse(e001$NTrt == 0, 0, 1)
e001$Fenced <- ifelse(e001$Fenced == "n", 0, 1)
e001$Burned <- ifelse(e001$Burned =="n", 0, 1)

# e002
e002$Exp <- "e002"
e002$FP <- paste(e002$Field,e002$Plot, sep = ".")
# Get only continuously fertilized plots
plotlist <- factor(e002$FP[e002$Year==2013 & e002$NtrtRec==1 & e002$BurnTrt==0])
e002 <-e002[e002$FP %in% plotlist,]
# Get rid of Ntrt 9
e002 <- e002[e002$Ntrt != 9,]
e002 <- e002[,c(1:7,10,12,13)]
names(e002) <- c("Exp", "Year", "Field", "Plot", "Subplot", "NTrt", "NAdd", 
                 "Burned", "SR", "Fenced")
e002$TrtYear <- e002$Year - 1981
e002$NTrt <- e002$NAdd
e002$NAdd <- ifelse(e002$NTrt == 0, 0, 1)

# e011
e011$Year <- substr(e011$Sampling.date..YYMMDD., 0, 2)
e011$Year <- as.numeric(e011$Year) + 1900
e011 <- e011[e011$Species.name != "Fungi" & e011$Species.name != "Misc. Litter" &
               e011$Species.name != "Miscellaneous Litter" & 
               e011$Species.name != "Mosses" & e011$Species.name != "Mosses & lichens" &
               e011$Species.name != "Mosses lichens",]
e011$present <- 1
e011 <- spread(data=unique(e011[,c(1:5,7,13,14)]), key = Species.name, value = present)
e011$SR <- rowSums(e011[,c(7:length(e011))], na.rm = TRUE)
e011 <- e011[,c(1:3,5,6,116)]
names(e011) <- c("Field", "Exp", "Plot", "NTrt", "Year", "SR")
e011$Exp <- "e011"
e011$TrtYear <- e011$Year - min(e011$Year) + 1
e011$NAdd <- ifelse(e011$NTrt == 0, 0, 1)
e011$NTrt <- e011$NTrt *.34 # Get amount of N added

# e098
# Subset of e002 - burning treatments
e098$Year<- (as.Date(as.character(e098$Sampling.date.mm.dd.yyyy.), format = "%m/%d/%y"))
e098$Year <- year(e098$Year)
e098 <- e098[e098$Species.Name != "Fungi" & e098$Species.Name != "Lichens" &
               e098$Species.Name != "Miscellaneous litter" & 
               e098$Species.Name != "Mosses" & e098$Species.Name != "Mosses & lichens" &
               e098$Species.Name != "Mosses & lichens 2" & e098$Species.Name != "Mosses and",]
e098$present <- 1
e098 <- spread(data=unique(e098[,c(1:3,5:8,10,11)]), key = Species.Name, value = present)
e098$SR <- rowSums(e098[,c(8:length(e098))], na.rm = TRUE)
e098 <- e098[,c(1:3,5:7, 104)]
names(e098) <- c("Field", "Exp", "Plot", "NTrt", "Burned", "Year", "SR")
e098$Exp <- "e098"
# I am starting this at 1992 because that is when fences were removed
e098 <- e098[e098$Year >= 1992,]
e098$TrtYear <- e098$Year - min(e098$Year) + 1
e098$NTrt <- e098$NTrt*0.34 # Get acutal amount of N added
e098$NAdd <- ifelse(e098$NTrt == 0, 0, 1)

# e172
# Subset of experiment 1 - fencing treatments
e172 <- e172[e172$Species != "Fungi" & e172$Species!= "Miscellaneous litter" & 
               e172$Species != "Mosses & lichens",]
e172$present <- 1
e172 <- spread(data=unique(e172[,c(1:6,9,10,12)]), key = Species, value = present)
e172$SR <- rowSums(e172[,c(8:length(e172))], na.rm = TRUE)
e172 <- e172[,c(1:4,6,7,141)]
names(e172)[5] <- "NTrt"
e172$Exp <- "e172"
# starting in 2005 when fences were removed
e172 <- e172[e172$Year >= 2005,]
e172$TrtYear <- e172$Year - min(e172$Year) + 1

# e245
e245 <- e245[e245$Species != " Bare ground" & e245$Species != " Fungi" &
               e245$Species != " Miscellaneous litter" & 
               e245$Species != " Mosses & lichens" & e245$Species != "Bare ground" & 
               e245$Species != "Miscellaneous litter" & e245$Species != "Moss" & 
               e245$Species != "Mosses",]
e245$present <- 1
e245 <- spread(data=unique(e245[,-c(4,6)]), key = Species, value = present)
e245$SR <- rowSums(e245[,c(5:length(e245))], na.rm = TRUE)
e245 <- e245[,c(1:4,153)]
e245 <- e245[e245$Treatment == "Control" | e245$Treatment == "Fenced",]
e245$Treatment <- ifelse(e245$Treatment == "Control", 0,1)
names(e245)[4] <- "Fenced"
e245$Exp <- tolower(e245$Exp)
e245$TrtYear <- e245$Year - min(e245$Year) + 1

# e247
e247$Taxon <- tolower(e247$Taxon)
e247 <- e247[e247$Taxon != "fungi sp." & e247$Taxon != "ground" & 
               e247$Taxon != "lichen" & e247$Taxon != "other" & 
               e247$Taxon != "other animal diggings" & e247$Taxon != "other litter",]
e247$present <- 1
e247 <- spread(data=unique(e247[,c(1,4:7,9:13,15,23)]), key = Taxon, value = present)
e247$SR <- rowSums(e247[,c(11:length(e247))], na.rm = TRUE)
# Subset out only N addition treatments
e247 <- e247[e247$P == 0 & e247$K == 0 & e247$Exclose == 0,]
e247 <- e247[,c(1:6,10, 116)]
names(e247) <- c("Year", "TrtYear", "Block", "Plot", "Subplot", "NAdd", "NTrt", "SR")
e247$Exp <- "e247"
e247$NAdd <- ifelse(e247$NTrt == 0, 0,1) # Need NAdd column to indicate N addition experiment


# e248
e248 <- e248[,c(1:4,6)]
names(e248) <- c("Year", "Plot", "NTrt", "Water", "SR")
e248$NTrt <- as.character(e248$NTrt)
e248$NTrt[e248$NTrt == "A"] <- 0 
e248$NTrt[e248$NTrt == "B"] <- 7
e248$NTrt[e248$NTrt == "C"] <- 14
e248$NTrt <- as.numeric(e248$NTrt)
e248$Exp <- "e248"
e248$TrtYear <- e248$Year - min(e248$Year) + 1
e248$NAdd <- ifelse(e248$NTrt == 0, 0,1)

## Combine datasets
dfs <- list(e001, e002, e011, e098, e172, e245, e247, e248)

findat <- bind_rows(dfs)

dat.fin <- bind_rows(dfs)

write.csv(dat.fin, here::here("Data", "findat.csv"))

## Column names are as follows
# Year - the actual calendar year ; Field - field letter where applicable
# Plot - numeric plot; NTrt - magnitude of nitrogen addition (g/m2/year)
# NAdd - Was nitrogen added or not: 0,1 (will be NA if not a N addition experiment)
# Fenced - Were the plots fenced: 0,1 (NA if not a treatment)
# Burned - Were the plots burned: 0,1 (NA if not a treatment)
# SR - species richness; Exp - experiment number; 
# TrtYear - year of treatment applied starting at 1
# Subplot and block - designated for some experiments
# Water - were the plots irrigated; 0,1 (NA if not a treatment)

