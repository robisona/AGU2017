###########
## Categorization.R
## R Script to perform categorization of synoptic data and statistical analyses
## Github repository AGU2017
###########


###########
## Set working direction, load data, and get some brief descriptors ##
#Set working directory
setwd("C:\\Users\\Drew\\Documents\\UNH\\Research\\Synoptic-07-2017")

#Load data
Data <- read.table("Data.csv", sep=",", header = T, stringsAsFactors = F)
#head(Data)

str(Data)
NumSamp <- length(Data$Site) # We have 48 samples
NumSite <- length(unique(Data$Site)) # Two of the samples are duplicates
NumVar <- ncol(Data) # 48 columns



##########
## Start with exmamining the TYPE category, either headwater, tributary, or mainstem/mouth ##
##########
unique(Data$Type)

par(mfrow = c(2,2))
Data$Type <- factor(Data$Type, levels = c("HEAD","TRIB","MAIN"))
boxplot(Data$CO2_uatm ~ Data$Type, log = "y", ylab = expression('pCO'[2]*' ('*mu*'atm)')) 
boxplot(Data$CH4_uatm ~ Data$Type, log = "y", ylab = expression('CH'[4]*' ('*mu*'atm)')) 
boxplot(Data$N2O_uatm ~ Data$Type, log = "y", ylab = expression('N'[2]*'O ('*mu*'atm)')) 
boxplot(Data$N2.Ar_Diff ~ Data$Type, ylab = expression('N'[2]*':Ar'))

##########
## Test differences in gas data based on Type ##
##########

#Let's first looks at the range of certain land cover categories
hist(Data$forst_upstream_avg)

# The gas data is not normally distributed, thus we must use non-parametric tests. 



##########
## Categorize data into groups based on land cover ##
##########



Data$LandUse[Data$allhumn_upstream_avg > 40 & Data$forst_upstream_avg < 30] <- "Urban"
Data$LandUse[Data$forst_upstream_avg > 40 & Data$allhumn_upstream_avg < 30] <- "Forested"
Data$LandUse[is.na(Data$LandUse)] <- "Mixed"



