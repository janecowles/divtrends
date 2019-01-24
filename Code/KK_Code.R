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
library(here)
library(tidyr)

#### load data
e001 <- read.csv(here("Data", "e001_dat.csv")) # Nitrogen addition, 82-present
e002 <- read.csv(here("Data", "e002_dat.csv")) # Nitrogen addition, 82-present
e003 <- read.csv(here("Data", "e003_dat.csv")) # Nitrogen and water addition, 82-91
e005 <- read.csv(here("Data", "e005_dat.csv")) # Nitrogen addition and herbivory, 84-85
e011 <- read.csv(here("Data", "e011_dat.csv")) # Nitrogen addition and herbivory, 84-92
e098 <- read.csv(here("Data", "e098_dat.csv")) # Nitrogen addition and fire, 82-11
e172 <- read.csv(here("Data", "e172_dat.csv")) # Nitrogen addition and herbivory, 82 - 11
e244 <- read.csv(here("Data", "e244_dat.csv")) # Enemy removal, 08-16
e245 <- read.csv(here("Data", "e245_dat.csv")) # Enemy removal, 08-16
e247 <- read.csv(here("Data", "e247_dat.csv")) # Nitrogen addition and herbivory, 07 - 15
e248 <- read.csv(here("Data", "e248_dat.csv")) # Nitrogen and water addition, 07-16

# Need all column names to be the same for combining the datasets
# e003 - e247 do not have a species richness column either - making that, too
# e001
e001 <- e001[,c(1:5,7:9)]
e001$Exp <- "e001"
names(e001)[8] <- "SR" 

# e002
e002$Exp <- "e002"
e002 <- e002[,c(1:10,12,13)]
names(e002) <- c("Exp", "Year", "Field", "Plot", "Subplot", "NTrt", "NAdd", "NCess",
                 "NTrtRec", "Burned", "SR", "Fenced")

# e003
e003$Year <- format(as.Date(as.character(e003$Sampling.date..MMDDYY.), format = "%m/%d/%Y"), "%Y")
e003$Species.Name <- tolower(e003$Species.Name)
e003 <- e003[e003$Species.Name != "miscellaneous litter" & e003$Species.Name != "misellaneous litter" &
               e003$Species.Name != "fungi" & e003$Species.Name != "mosses & lichens" &
               e003$Species.Name != "alive" & e003$Species.Name != "mosses" & 
               e003$Species.Name != "mosses lichens" ,]
e003$present <- 1
e003 <- spread(data=e003[,c(1:4,6,12,13)], key = Species.Name, value = present)
e003$SR <- rowSums(e003[,c(6:length(e003))], na.rm = TRUE)
e003 <- e003[,c(1:5,111)]
names(e003) <- c("Field", "Exp", "Plot", "Treatment", "Year", "SR")

# Treatment Codes
# A	20 g/m2 NH4NO3
# B	35 g/m2 NaH2PO4-H2O + 35 g/m2 Na2HPO4
# C	87 g/m2 K2SO4
# D	75 g/m2 CaCO3
# E	60 g/m2 MgSO4
# F	71 g/m2 Na2SO4
# G	60 ml/m2 trace metal sand*
# H	4 gal/m2/week H2O
# I	no nutrients added

# e005
e005$Species.Name <- tolower(e005$Species.Name)
e005 <- e005[e005$Species.Name != "miscellaneous litter" &
               e005$Species.Name != "fungi" &  e005$Species.Name != "mosses",]
e005$present <- 1
e005 <- spread(data=unique(e005[,c(1,2,3,4,5,6,7,10,15)]), key = Species.Name, value = present)
e005$SR <- rowSums(e005[,c(8:length(e005))], na.rm = TRUE)
e005 <- e005[,c(1:4,6,7, 102)]
names(e005) <- c("Field", "Exp", "Plot", "Fenced", "NTrt", "Year")

# Nitrgoen added at ammonium nitrate and numbers are g/m2/y
# Fence treatment codes
#1 = All excluded,
#2=Voles excluded,
#3=Foliage-feeding insects excluded,
#4=Gophers excluded,
#5=Fenced control,
#6=Control-no treatment,
#7=Below-ground invertebrates excluded,
#8=Control-no treatment

# e011
e011$Year <- substr(e011$Sampling.date..YYMMDD., 0, 2)
e011 <- e011[e011$Species.name != "Fungi" & e011$Species.name != "Misc. Litter" &
               e011$Species.name != "Miscellaneous Litter" & 
               e011$Species.name != "Mosses" & e011$Species.name != "Mosses & lichens" &
               e011$Species.name != "Mosses lichens",]
e011$present <- 1
e011 <- spread(data=unique(e011[,c(1:5,7,13,14)]), key = Species.name, value = present)
e011$SR <- rowSums(e011[,c(7:length(e011))], na.rm = TRUE)
e011 <- e011[,c(1:3,5,6,116)]
names(e011) <- c("Field", "Exp", "Plot", "NTrt", "Year", "SR")

# e098
# This one may be included in e002 data...
## These rates are added twice a year. Actual annual N addition is calculated as: 0.34%N * rate (g/m2) * 2 times/year
e098$Year<- format(as.Date(as.character(e098$Sampling.date.mm.dd.yyyy.), format = "%m/%d/%y"),"%y")
e098 <- e098[e098$Species.Name != "Fungi" & e098$Species.Name != "Lichens" &
               e098$Species.Name != "Miscellaneous litter" & 
               e098$Species.Name != "Mosses" & e098$Species.Name != "Mosses & lichens" &
               e098$Species.Name != "Mosses & lichens 2" & e098$Species.Name != "Mosses and",]
e098$present <- 1
e098 <- spread(data=unique(e098[,c(1:3,5:8,10,11)]), key = Species.Name, value = present)
e098$SR <- rowSums(e098[,c(8:length(e098))], na.rm = TRUE)
e098 <- e098[,c(1:3,5:7, 104)]
names(e098) <- c("Field", "Exp", "Plot", "NTrt", "Burned", "Year", "SR")

# e172
# Sub experiment of 1?
e172 <- e172[e172$Species != "Fungi" & e172$Species!= "Miscellaneous litter" & 
               e172$Species != "Mosses & lichens",]
e172$present <- 1
e172 <- spread(data=unique(e172[,c(1:6,9,10,12)]), key = Species, value = present)
e172$SR <- rowSums(e172[,c(8:length(e172))], na.rm = TRUE)
e172 <- e172[,c(1:4,6,7,141)]
names(e172)[5] <- "NTrt"

# e244
e244 <- e244[e244$Species != " Fungi" & e244$Species != " Miscellaneous litter" & 
               e244$Species != " Mosses & lichens" & e244$Species != "Bare" & 
               e244$Species != "Bare ground" & e244$Species != "bareground" &
               e244$Species != "Miscellaneous litter" & e244$Species != "Moss" &
               e244$Species != "Mosses" & e244$Species != "Mosses 2" &
               e244$Species != "Mosses 3",]
e244$present <- 1
e244 <- spread(data=unique(e244[,-c(4,6)]), key = Species, value = present)
e244$SR <- rowSums(e244[,c(5:length(e244))], na.rm = TRUE)
e244 <- e244[,c(1:4,204)]
e244$Exp <- tolower(e244$Exp)

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

# e247
e247$Taxon <- tolower(e247$Taxon)
e247 <- e247[e247$Taxon != "fungi sp." & e247$Taxon != "ground" & 
               e247$Taxon != "lichen" & e247$Taxon != "other" & 
               e247$Taxon != "other animal diggings" & e247$Taxon != "other litter",]
e247$present <- 1
e247 <- spread(data=unique(e247[,c(1,4:7,9:13,15,23)]), key = Taxon, value = present)
e247$SR <- rowSums(e247[,c(11:length(e247))], na.rm = TRUE)
e247 <- e247[,c(1:10, 116)]
names(e247) <- c("Year", "ExpYear", "Block", "Plot", "SubPlot", "N", "P", "K", "NTrt", "SR")
e247$Exp <- "e247"

# e248
e248 <- e248[,c(1:4,6)]
names(e248) <- c("Year", "Plot", "NTrt", "WaterTrt", "SR")
e248$Exp <- "e248"


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