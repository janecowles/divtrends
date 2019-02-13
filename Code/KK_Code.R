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
library(tidyr)
library(lubridate)
library(here)


#### load data
e001 <- read.csv(here::here("Data", "e001_dat.csv")) # Nitrogen addition, 82-present
e002 <- read.csv(here::here("Data", "e002_dat.csv")) # Nitrogen addition, 82-present
e011 <- read.csv(here::here("Data", "e011_dat.csv")) # Nitrogen addition, 84-92
e098 <- read.csv(here::here("Data", "e098_dat.csv")) # Nitrogen addition and fire, 82-11
e172 <- read.csv(here::here("Data", "e172_dat.csv")) # Nitrogen addition and herbivory, 82 - 11
e245 <- read.csv(here::here("Data", "e245_dat.csv")) # Enemy removal, 08-16
e247 <- read.csv(here::here("Data", "e247_dat.csv")) # Nitrogen addition and herbivory, 07 - 15
e248 <- read.csv(here::here("Data", "e248_dat.csv")) # Nitrogen and water addition, 07-16

# Need all column names to be the same for combining the datasets
# e003 - e247 do not have a species richness column either - making that, too
# e001
e001 <- e001[,c(1:5,7:9)]
e001$Exp <- "e001"
names(e001)[8] <- "SR"
e001$TrtYear <- e001$Year - min(e001$Year) + 1


# e002
e002$Exp <- "e002"
e002 <- e002[,c(1:10,12,13)]
names(e002) <- c("Exp", "Year", "Field", "Plot", "Subplot", "NTrt", "NAdd", "NCess",
                 "NTrtRec", "Burned", "SR", "Fenced")
e002 <- e002[-which(is.na(e002$Year)),]
e002$TrtYear <- e002$Year - 1981
e002$FP <- paste(e002$Field,e002$Plot, sep = ".")
e002sub <- e002[which(e002$NTrtRec == 0),14]
e002sub <- unique(e002sub)
test <- e002[(e002$FP %in% e002sub),]

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
e011$NAdd <- 1

# e098
# This one may be included in e002 data...
## These rates are added twice a year. Actual annual N addition is calculated as: 0.34%N * rate (g/m2) * 2 times/year
e098$Year<- (as.Date(as.character(e098$Sampling.date.mm.dd.yyyy.), format = "%m/%d/%y"))
library(lubridate)
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
e098$TrtYear <- e098$Year - min(e098$Year) + 1
e098$NAdd <- 1

# e172
# Sub experiment of 1?
e172 <- e172[e172$Species != "Fungi" & e172$Species!= "Miscellaneous litter" & 
               e172$Species != "Mosses & lichens",]
e172$present <- 1
e172 <- spread(data=unique(e172[,c(1:6,9,10,12)]), key = Species, value = present)
e172$SR <- rowSums(e172[,c(8:length(e172))], na.rm = TRUE)
e172 <- e172[,c(1:4,6,7,141)]
names(e172)[5] <- "NTrt"
e172$Exp <- "e172"
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
names(e247) <- c("Year", "TrtYear", "Block", "Plot", "SubPlot", "NAdd", "NTrt", "SR")
e247$Exp <- "e247"
e247$NAdd <- 1 # Need NAdd column to indicate N addition experiment

# Get rid of P K columns
e247 <- e247[,-c(7,8)]



# e248
e248 <- e248[,c(1:4,6)]
names(e248) <- c("Year", "Plot", "NTrt", "WaterTrt", "SR")
e248$NTrt <- as.character(e248$NTrt)
e248[e248$NTrt == "A",] <- 0
e248[e248$NTrt == "B",] <- 7
e248[e248$NTrt == "C",] <- 14
e248$Exp <- "e248"
e248$TrtYear <- e248$Year - min(e248$Year) + 1
e248$NAdd <- 1

## Combine datasets


# Step 1: Find magnitude and direction of effect in year 1, 3 and 10 separately


# load data

# NewNTrt A = ambient; B = +7 g/m2/year; C = +14 g/m2/year
# WaterTrt 0 = ambient; 1 = ~ 1 inch/week/growing season

# Get columns that we need (Year, Treatment (Irrigation or N addition), and specie richness)
e248 <- e248[,c(1:4,6)]
# Rename columns
names(e248) <- c("Year", "Plot", "NTrt", "H2OTrt", "SR")
# Add experiment year column
e248$ExpYear <- e248$Year-2006

# Right now we will look at the magnitude and direction of change within each year only
# We may want to consider fitting a temporal trend and using the predicted values
# So we can account for year to year variation in the plots from climatic variables, too.

mod1 <- lm(SR ~ NTrt*H2OTrt, data = e248[e248$ExpYear ==1, ]) # All Interactions
summary(mod1)
mod2 <- lm(SR ~ NTrt + H2OTrt, data = e248[e248$ExpYear ==1, ]) # No interaction
summary(mod2)
## Conclusions: There is no significant difference between ambient and treated plots
## in the first year 
## It seems like we gain species in the first year in the High N and Irrigated plots

mod3 <- lm(SR ~ NTrt*H2OTrt, data = e248[e248$ExpYear ==3, ]) # All Interactions
summary(mod3)
mod4 <- lm(SR ~ NTrt + H2OTrt, data = e248[e248$ExpYear ==3, ]) # No interaction
summary(mod4)


mod5 <- lm(SR ~ NTrt*H2OTrt, data = e248[e248$ExpYear ==10, ]) # All Interactions
summary(mod5)
mod6 <- lm(SR ~ NTrt + H2OTrt, data = e248[e248$ExpYear ==10, ]) # No interaction
summary(mod6)
# Here N treatments significantly reduce species richness.

## THOUGHTS: SHould we pull all the Estimated values to put in another model? 
## OR - should we model everything together to begin with?