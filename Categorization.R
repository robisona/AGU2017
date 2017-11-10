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
## Categorize data into groups based on land cover ##
##########

#Let's first looks at the range of certain land cover categories
hist(Data$forst_upstream_avg)


Data$LandUse[Data$allhumn_upstream_avg > 40 & Data$forst_upstream_avg < 30] <- "Urban"
Data$LandUse[Data$forst_upstream_avg > 40 & Data$allhumn_upstream_avg < 30] <- "Forested"
Data$LandUse[is.na(Data$LandUse)] <- "Mixed"

