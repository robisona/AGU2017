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
NumSite <- length(unique(Data$Site)) # Two of the samples are duplicates, so 46 sites
NumVar <- ncol(Data) # 48 columns



##########
## Start with exmamining the TYPE category, either headwater, tributary, or mainstem/mouth ##
##########
unique(Data$Type)
pdf("Boxplot-Gas-Type.pdf")
par(mfrow = c(2,2))
Data$Type <- factor(Data$Type, levels = c("HEAD","TRIB","MAIN"))
boxplot(Data$CO2_uatm ~ Data$Type, log = "y", ylab = expression('pCO'[2]*' ('*mu*'atm)')) 
boxplot(Data$CH4_uatm ~ Data$Type, log = "y", ylab = expression('CH'[4]*' ('*mu*'atm)')) 
boxplot(Data$N2O_uatm ~ Data$Type, log = "y", ylab = expression('N'[2]*'O ('*mu*'atm)')) 
boxplot(Data$N2.Ar_Diff ~ Data$Type, ylab = expression('N'[2]*':Ar'))
dev.off() #finish pdf


##########
## Test differences in gas data based on Type ##
##########

#Let's first looks at the range of certain land cover categories
par(mfrow = c(1,1))
hist(Data$forst_upstream_avg)

# The gas data is not normally distributed, thus we must use non-parametric tests. 



##########
## Categorize data into groups based on land cover ##
##########

# Examine data to determine land use categories. Ideally want at least forested and urbanized, but perhaps a different
# category emerges (e.g. agricultural, wetland, etc.). Let's start by looking at spread of land use percent covers using
# histograms

# Agricultural
hist(Data$agri_upstream_avg) 
# Not really spread evenly, too many at low percentages. One site at high % might be good to keep in mind though if a
# unique signal emerges

# Forested
hist(Data$forst_upstream_avg)
# Good spread. See how many sites are above 40, and 50%
sum(Data$forst_upstream_avg > 50, na.rm = T) # Only 9 sites above 50 percent, maybe too small a group
sum(Data$forst_upstream_avg > 40, na.rm = T)# 14 sites above 40%, perhaps more useful

# Developed
hist(Data$humn_upstream_avg)
# Good spread. See how many sites are above 40, and 50%
sum(Data$humn_upstream_avg > 50, na.rm = T) # 11 sites above 50%
sum(Data$humn_upstream_avg > 40, na.rm = T)# 18 sites above 40%, perhaps more useful

# Wetlands
hist(Data$wetl_upstream_avg)
# Pretty heavily biased < 20%, perhaps not the best for a dominant land use category. Like agriculture, one site is
# much higher than the rest, so keep that in mind.

# Let's create a new column of data that is the ratio of forested land to developed land
Data$forst_humn_ratio <- Data$forst_upstream_avg/Data$humn_upstream_avg
Data$forst_humn_ratio
hist(Data$forst_humn_ratio)
hist(Data$forst_humn_ratio[Data$forst_humn_ratio < 20])

# Create new column with categorical land use type
Data$LandUse[Data$forst_humn_ratio < 0.6] <- "Urban"
Data$LandUse[Data$forst_humn_ratio > 1.5] <- "Forested"
Data$LandUse[Data$forst_humn_ratio <= 1.5 & Data$forst_humn_ratio >= 0.6] <- "Mixed"
Data$LandUse
summary(Data$LandUse)





# So forested and developed are the two best categories here, with 40% looking like a decent cutoff. But that could include
# both site (i.e. a site had 41% forest and 41% developed). Let's see if and how many times this happens
sum(Data$forst_upstream_avg > 40 & Data$humn_upstream_avg > 40, na.rm = T)
# It doesn't happen! So we could catergorize those forested and developed sites based on that 40% cutoff. Those between we 
# could class mixed, but let's take a look at the sites that would fall into this category to see if "Mixed"  would be an 
# appropriate name. To do that, subset the data where Forest < 40% and Developed < 40%.
Data.Mixed <- Data[Data$forst_upstream_avg < 40 & Data$humn_upstream_avg < 40,]
summary(Data.Mixed)
ForDecRatio <- Data$forst_upstream_avg/Data$humn_upstream_avg
ForDecRatio[Data$forst_upstream_avg > 40]
ForDecRatio[Data$humn_upstream_avg > 40]
ForDecRatio[Data$forst_upstream_avg < 40 & Data$humn_upstream_avg < 40]




##########
## Start with exmamining the TYPE category, either headwater, tributary, or mainstem/mouth ##
##########
pdf("Boxplot-Gas-LandUse.pdf")
par(mfrow = c(2,2))
boxplot(Data$CO2_uatm ~ Data$LandUse, log = "y", ylab = expression('pCO'[2]*' ('*mu*'atm)')) 
boxplot(Data$CH4_uatm ~ Data$LandUse, log = "y", ylab = expression('CH'[4]*' ('*mu*'atm)')) 
boxplot(Data$N2O_uatm ~ Data$LandUse, log = "y", ylab = expression('N'[2]*'O ('*mu*'atm)')) 
boxplot(Data$N2.Ar_Diff ~ Data$LandUse, ylab = expression('N'[2]*':Ar'))
dev.off() #finish pdf


##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

##########
## Run loops to plot all simple linear regressions for examination outside of R##
##########

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################


# We can only use numeric data, so we furst must subset the data to remove all non-numeric data
NumData = Data[,sapply(Data,is.numeric)]
str(NumData)


##########
## Start with N2O data ##
##########

##### N2O - All Sites #####
pdf("N2O-AllTypes.pdf") #Set up pdf for regressions of N2O vs. all other variables
par(mfrow=c(2,2)) #Each page of pdf has 2x2 plots

#Set up for loop for the number of columns of numerical data
for(i in 1:ncol(NumData)){
  x <- NumData[,i] #x variable is column i
  y <- NumData$N2O_uatm #y variable is N2O data
  mydata = data.frame(x,y) #combine into a single dataframe
  fit <- lm(y~x, data = mydata) #find the linear model of the data
  rmse <- round(sqrt(mean(resid(fit)^2)), 2) #find the rmse of the regression
  coefs <- coef(fit) #separate out the coefficients of the regression line
  b0 <- round(coefs[1], 2) #The y-intercept
  b1 <- round(coefs[2],2) #the slope
  r2 <- round(summary(fit)$r.squared, 2) #the r-squared value
  eqn <- bquote(r^2 == .(r2)) #combine the r2 and rmse into a text variable
  plot(x, y, xlab = names(NumData)[i], ylab = "N2O (uatm)") #plot the regression data
  abline(fit) #plot the regression line
  text(min(x, na.rm = TRUE)+sd(x, na.rm = TRUE), max(y, na.rm = TRUE)-sd(y, na.rm = TRUE), eqn, pos = 4) #print the r2 on each plot
}
dev.off() #finish pdf


##### N2O - Only Headwaters #####
pdf("N2O-HEAD.pdf") #Set up pdf for regressions of N2O vs. all other variables
par(mfrow=c(2,2)) #Each page of pdf has 2x2 plots

#Set up for loop for the number of columns of numerical data
for(i in 1:ncol(NumData)){
  x <- NumData[Data$Type == "HEAD",i] #x variable is column i
  y <- NumData$N2O_uatm[Data$Type == "HEAD"] #y variable is N2O data
  mydata = data.frame(x,y) #combine into a single dataframe
  fit <- lm(y~x, data = mydata) #find the linear model of the data
  rmse <- round(sqrt(mean(resid(fit)^2)), 2) #find the rmse of the regression
  coefs <- coef(fit) #separate out the coefficients of the regression line
  b0 <- round(coefs[1], 2) #The y-intercept
  b1 <- round(coefs[2],2) #the slope
  r2 <- round(summary(fit)$r.squared, 2) #the r-squared value
  eqn <- bquote(r^2 == .(r2)) #combine the r2 and rmse into a text variable
  plot(x, y, xlab = names(NumData)[i], ylab = "N2O (uatm)") #plot the regression data
  abline(fit) #plot the regression line
  text(min(x, na.rm = TRUE)+sd(x, na.rm = TRUE), max(y, na.rm = TRUE)-sd(y, na.rm = TRUE), eqn, pos = 4) #print the r2 on each plot
}
dev.off() #finish pdf



##########
## Now do CH4 ##
##########

##### CH4 - All Sites #####
pdf("CH4-AllTypes.pdf") #Set up pdf for regressions of N2O vs. all other variables
par(mfrow=c(2,2)) #Each page of pdf has 2x2 plots

#Set up for loop for the number of columns of numerical data
for(i in 1:ncol(NumData)){
  x <- NumData[,i] #x variable is column i
  y <- NumData$CH4_uatm #y variable is N2O data
  mydata = data.frame(x,y) #combine into a single dataframe
  fit <- lm(y~x, data = mydata) #find the linear model of the data
  rmse <- round(sqrt(mean(resid(fit)^2)), 2) #find the rmse of the regression
  coefs <- coef(fit) #separate out the coefficients of the regression line
  b0 <- round(coefs[1], 2) #The y-intercept
  b1 <- round(coefs[2],2) #the slope
  r2 <- round(summary(fit)$r.squared, 2) #the r-squared value
  eqn <- bquote(r^2 == .(r2)) #combine the r2 and rmse into a text variable
  plot(x, y, xlab = names(NumData)[i], ylab = "CH4 (uatm)") #plot the regression data
  abline(fit) #plot the regression line
  text(min(x, na.rm = TRUE)+sd(x, na.rm = TRUE), max(y, na.rm = TRUE)-sd(y, na.rm = TRUE), eqn, pos = 4) #print the r2 on each plot
}
dev.off() #finish pdf


##### CH4 - Only Headwaters #####
pdf("CH4-HEAD.pdf") #Set up pdf for regressions of N2O vs. all other variables
par(mfrow=c(2,2)) #Each page of pdf has 2x2 plots

#Set up for loop for the number of columns of numerical data
for(i in 1:ncol(NumData)){
  x <- NumData[Data$Type == "HEAD",i] #x variable is column i
  y <- NumData$CH4_uatm[Data$Type == "HEAD"] #y variable is N2O data
  mydata = data.frame(x,y) #combine into a single dataframe
  fit <- lm(y~x, data = mydata) #find the linear model of the data
  rmse <- round(sqrt(mean(resid(fit)^2)), 2) #find the rmse of the regression
  coefs <- coef(fit) #separate out the coefficients of the regression line
  b0 <- round(coefs[1], 2) #The y-intercept
  b1 <- round(coefs[2],2) #the slope
  r2 <- round(summary(fit)$r.squared, 2) #the r-squared value
  eqn <- bquote(r^2 == .(r2)) #combine the r2 and rmse into a text variable
  plot(x, y, xlab = names(NumData)[i], ylab = "CH4 (uatm)") #plot the regression data
  abline(fit) #plot the regression line
  text(min(x, na.rm = TRUE)+sd(x, na.rm = TRUE), max(y, na.rm = TRUE)-sd(y, na.rm = TRUE), eqn, pos = 4) #print the r2 on each plot
}
dev.off() #finish pdf


##########
## And finally do CO2 ##
##########

##### CO2 - All Sites #####
pdf("CO2-AllTypes.pdf") #Set up pdf for regressions of N2O vs. all other variables
par(mfrow=c(2,2)) #Each page of pdf has 2x2 plots

#Set up for loop for the number of columns of numerical data
for(i in 1:ncol(NumData)){
  x <- NumData[,i] #x variable is column i
  y <- NumData$CO2_uatm #y variable is N2O data
  mydata = data.frame(x,y) #combine into a single dataframe
  fit <- lm(y~x, data = mydata) #find the linear model of the data
  rmse <- round(sqrt(mean(resid(fit)^2)), 2) #find the rmse of the regression
  coefs <- coef(fit) #separate out the coefficients of the regression line
  b0 <- round(coefs[1], 2) #The y-intercept
  b1 <- round(coefs[2],2) #the slope
  r2 <- round(summary(fit)$r.squared, 2) #the r-squared value
  eqn <- bquote(r^2 == .(r2)) #combine the r2 and rmse into a text variable
  plot(x, y, xlab = names(NumData)[i], ylab = "CO2 (uatm)") #plot the regression data
  abline(fit) #plot the regression line
  text(min(x, na.rm = TRUE)+sd(x, na.rm = TRUE), max(y, na.rm = TRUE)-sd(y, na.rm = TRUE), eqn, pos = 4) #print the r2 on each plot
}
dev.off() #finish pdf


##### CO2 - Only Headwaters #####
pdf("CO2-HEAD.pdf") #Set up pdf for regressions of N2O vs. all other variables
par(mfrow=c(2,2)) #Each page of pdf has 2x2 plots

#Set up for loop for the number of columns of numerical data
for(i in 1:ncol(NumData)){
  x <- NumData[Data$Type == "HEAD",i] #x variable is column i
  y <- NumData$CO2_uatm[Data$Type == "HEAD"] #y variable is N2O data
  mydata = data.frame(x,y) #combine into a single dataframe
  fit <- lm(y~x, data = mydata) #find the linear model of the data
  rmse <- round(sqrt(mean(resid(fit)^2)), 2) #find the rmse of the regression
  coefs <- coef(fit) #separate out the coefficients of the regression line
  b0 <- round(coefs[1], 2) #The y-intercept
  b1 <- round(coefs[2],2) #the slope
  r2 <- round(summary(fit)$r.squared, 2) #the r-squared value
  eqn <- bquote(r^2 == .(r2)) #combine the r2 and rmse into a text variable
  plot(x, y, xlab = names(NumData)[i], ylab = "CO2 (uatm)") #plot the regression data
  abline(fit) #plot the regression line
  text(min(x, na.rm = TRUE)+sd(x, na.rm = TRUE), max(y, na.rm = TRUE)-sd(y, na.rm = TRUE), eqn, pos = 4) #print the r2 on each plot
}
dev.off() #finish pdf

