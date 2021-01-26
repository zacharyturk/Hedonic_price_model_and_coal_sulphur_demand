#The following code is in support of my first final project for the course.
#I use a hedonic price model to estimate the price U.S. power plants pay for the
#sulfur content of coal (should be a negative number) and test whether price discrimination
#is occuring based on FGD capacity at power plants.
#
#Preliminary setup:
getwd()
setwd("C:/Users/Zachary/Documents/R/R_final_projects/")
install.packages("foreign")
install.packages("fields")
library(foreign)
library(fields)
#
#To begin, I import the data I initially accumulated in another programming language:
dat<-read.dta("combined2008_2014.dta")
#354187 plant purchase level observations, however not all are relevant to this study.
#edit(dat)
#My data has a lot of information on non-coal purchases not relevant to this analysis.
#I subset to remove them from the analysis:
dat<-subset(dat, dat$FUEL_GROUP=="Coal")
#I'm now down to the 145,308 coal related purchases at U.S. power plants from 2008-2014.
#Next I subset out the columns that are irrelevant for cleanliness:
dat<-subset(dat, select=c(YEAR,MONTH,PlantId,PlantName,PlantState,PurchaseType,
ENERGY_SOURCE,CoalmineState,CoalmineMshaId,QUANTITY,AverageHeatContent,FUEL_COST,
Regulated,OperatorId,ReportingFrequency,PrimaryTransportationMode,AverageSulfurContent,
AverageAshContent,AverageMercuryContent,meanFGD,monthly))
#And renaming some of the more cumbersome variables:
colnames(dat)[colnames(dat)=="PlantId"]<-"plantID"
colnames(dat)[colnames(dat)=="ENERGY_SOURCE"]<-"Coaltype"
colnames(dat)[colnames(dat)=="CoalmineMshaId"]<-"mineID"
colnames(dat)[colnames(dat)=="QUANTITY"]<-"Quantity"
colnames(dat)[colnames(dat)=="AverageHeatContent"]<-"AveBTU"
colnames(dat)[colnames(dat)=="FUEL_COST"]<-"Fuelcost"
colnames(dat)[colnames(dat)=="AverageSulfurContent"]<-"Avesulfur"
colnames(dat)[colnames(dat)=="AverageAshContent"]<-"Aveash"
colnames(dat)[colnames(dat)=="AverageMercuryContent"]<-"Avemercury"
#I had also converted the data on some variables to basis points but prefer in percent:
dat$Avesulfur<-dat$Avesulfur/100
dat$Aveash<-dat$Aveash/100
head(dat)
summary(dat)
#And adjust my monthly trend to start at 0:
dat$monthly<-(dat$monthly-588)
#My basic dataset, less distance data that requires preparation as a separate file,
#is now ready for analysis and saved as an R data file:
#save(dat, file = "dat1.RData")
#I now use data gathered from the MSHA to calculate distances from mine to power plant
#to use as a price proxy for transportation:
distdat<-read.dta("mine_and_plant_coordinates.dta")
#Getting rid of irrelevant data again:
distdat<-subset(distdat, select=c(pLatitude,pLongitude,mLatitude,mLongitude,
PriTransportMode,PlantId,MshaId))
#Simplifying some data names:
colnames(distdat)[colnames(distdat)=="pLatitude"]<-"plat"
colnames(distdat)[colnames(distdat)=="pLongitude"]<-"plon"
colnames(distdat)[colnames(distdat)=="mLatitude"]<-"mlat"
colnames(distdat)[colnames(distdat)=="mLongitude"]<-"mlon"
colnames(distdat)[colnames(distdat)=="PriTransportMode"]<-"TransportMode"
colnames(distdat)[colnames(distdat)=="PlantId"]<-"plantID"
colnames(distdat)[colnames(distdat)=="MshaId"]<-"mineID"
head(distdat)
summary(distdat)
#Now switching to preparing the mine to plant distance data I'll be attaching to
#and using in my analysis:
#Making vector of mine and plant coordinates:
minecoords<-cbind(distdat$mlon,distdat$mlat)
plantcoords<-cbind(distdat$plon,distdat$plat)
#Calculating distances between coordinates:
distances<-rdist.earth(minecoords,plantcoords,miles=TRUE,R=NULL)
#Attaching the distances to the plantId and mineID file:
distdat$distances<-diag(distances)
#Merging to two datasets by the paired plant-mine transactions:
hedonicdat<-merge(dat,distdat,by=c("plantID","mineID")) 
summary(hedonicdat)
#My data now includes 92,050 complete observations in the variables of interest.
#Visually inspecting for outliers:
par(mfcol = c(2,3)) 
plot(hedonicdat$Fuelcost,hedonicdat$Avesulfur)
plot(hedonicdat$Fuelcost,hedonicdat$Aveash)
plot(hedonicdat$Fuelcost,hedonicdat$AveBTU)
plot(hedonicdat$Fuelcost,hedonicdat$Quantity)
plot(hedonicdat$Fuelcost,hedonicdat$meanFGD)
#Removing unusual outliers:
hedonicdat<-subset(hedonicdat, hedonicdat$Fuelcost<800)
hedonicdat<-subset(hedonicdat, hedonicdat$Fuelcost>0)
hedonicdat<-subset(hedonicdat, hedonicdat$Avesulfur<6)
hedonicdat<-subset(hedonicdat, hedonicdat$Aveash<50)
hedonicdat<-subset(hedonicdat, hedonicdat$AveBTU<28)
#hedonicdat<-subset(hedonicdat, hedonicdat$meanFGD>40 | hedonicdat$meanFGD<1)
par(mfcol = c(2,3)) 
plot(hedonicdat$Fuelcost,hedonicdat$Avesulfur)
plot(hedonicdat$Fuelcost,hedonicdat$Aveash)
plot(hedonicdat$Fuelcost,hedonicdat$AveBTU)
plot(hedonicdat$Fuelcost,hedonicdat$Quantity)
plot(hedonicdat$Fuelcost,hedonicdat$meanFGD)
summary(hedonicdat)
#Generating a dummy variable for whether the plant has FGD in operation:
hedonicdat$FGD<-as.integer(complete.cases(hedonicdat$meanFGD))
#Checking normality assumptions of the data:
par(mfcol = c(2,3)) 
hist(hedonicdat$Fuelcost)
hist(hedonicdat$Avesulfur)
hist(hedonicdat$Aveash)
hist(hedonicdat$AveBTU)
hist(hedonicdat$Quantity)
hist(hedonicdat$distances)
summary(hedonicdat)
#In switching to natural log forms of variables I first adjust for zero
#observations in the factor where they occur:
tempAveash<-hedonicdat$Aveash
tempAveash[tempAveash==0]<-0.01
#Doing log transformations for both interpretability and to aid normality assumptions:
hedonicdat$lnFuelcost<-log(hedonicdat$Fuelcost)
hedonicdat$lnAvesulfur<-log(hedonicdat$Avesulfur)
hedonicdat$lnAveash<-log(tempAveash)
hedonicdat$lnAveBTU<-log(hedonicdat$AveBTU)
hedonicdat$lnQuantity<-log(hedonicdat$Quantity)
hedonicdat$lndistances<-log(hedonicdat$distances)
#Checking whether natural log transformations improved distribution,
#obviously it will have little effect on some of the variables from above but
#are still useful in interpreting results:
par(mfcol = c(2,6))
hist(hedonicdat$Fuelcost)
hist(hedonicdat$lnFuelcost)
hist(hedonicdat$Avesulfur)
hist(hedonicdat$lnAvesulfur)
hist(hedonicdat$Aveash)
hist(hedonicdat$lnAveash)
hist(hedonicdat$AveBTU)
hist(hedonicdat$lnAveBTU)
hist(hedonicdat$Quantity)
hist(hedonicdat$lnQuantity)
hist(hedonicdat$distances) 
hist(hedonicdat$lndistances)
#Now performing the hedonic price model analysis:
#The basic principle of a hedonic price model is that a good can be described by
#its components and thus the contribution of each to the price can be estimated.
#As level-level:
summary(model1<-lm(Fuelcost~Avesulfur+FGD+AveBTU+Aveash+Quantity+distances+PlantState,data=hedonicdat))
#As log-log:
summary(model1<-lm(lnFuelcost~lnAvesulfur+FGD+lnAveBTU+lnAveash+lnQuantity+lndistances+PlantState,data=hedonicdat))
#Checking for issues in the data such as heteroskedasticity and autocorrelation (AC):
fits<-fitted(model1)
resids<-residuals(model1)
plot(hedonicdat$lnFuelcost,fits)
abline(0,1)
plot(resids~fits)
abline(h=0)
plot(resids)
acf(resids)
#(clearly autocorrelation)
hist(resids)
qqnorm(resids)
qqline(resids)
#(outliers still an issue after log transformation)
#So alternately using an LPM model to account for the AC issue:
#Log-log and pooling by PlantId:
summary(model2<-plm(lnFuelcost~lnAvesulfur+FGD+lnAveBTU+lnAveash+lnQuantity+lndistances+factor(YEAR),index=c('plantID'),data=hedonicdat))
#Level-level of preferred specification:
summary(model3<-plm(Fuelcost~Avesulfur+FGD+AveBTU+Aveash+Quantity+distances+factor(YEAR),index=c('plantID'),data=hedonicdat))
summary(hedonicdat)
#Preparing graphics to include in my report:
par(mfcol = c(2,3))
boxplot(Fuelcost~FGD,data=hedonicdat,
xlab='FGD',ylab='Fuel price in cents/mmBTU',main='Fuel cost vs FGD')
boxplot(Avesulfur~FGD,data=hedonicdat,
xlab='FGD',ylab='Sulfur content percent by weight',main='Sulfur content vs FGD')
boxplot(AveBTU~FGD,data=hedonicdat,
xlab='FGD',ylab='Heat content in mmBTU/ton',main='BTU content vs FGD')
boxplot(Aveash~FGD,data=hedonicdat,
xlab='FGD',ylab='Ash content percent by weight',main='Ash content vs FGD')
boxplot(Quantity~FGD,data=hedonicdat,
xlab='FGD',ylab='Quantity in tons',main='Quantity vs FGD')
boxplot(distances~FGD,data=hedonicdat,
xlab='FGD',ylab='Distance in miles',main='Distance vs FGD')
#ZMT