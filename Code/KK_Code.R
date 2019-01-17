# DivTrends Project
# Overall objective: Here we aim to determine how species richness responds to 
# long-term environmental manipulations in (1) the first year, (2) 0ver short 
# time periods (3 years), and (3) over long time periods (10+ years)
# We also aim to determine which manipulation leads to the greatest change in 
# diversity for a given time period.


# Step 1: Find magnitude and direction of effect in year 1, 3 and 10 separately

# load libraries
library(here)

# load data
e248 <- read.csv(here("Data", "e248_dat.csv")) 
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