#This is the code for my second final project.  The first estimated a hedonic price model 
#for coal at U.S. power plants and whether price discrimination occurs at FGD enabled plants.
#This project tests whether plants with abatement technologies such as flue gas
#desulfurization (FGD) demand higher sulfur coal, denoted by higher mean sulfur content
#of the coal they consume.  It then breaks out FGD enabled plants by whether they also produce
#a marketable byproduct and then analyzes time trends.
#
getwd()
setwd("C:/Users/Zachary/Documents/R/R_final_projects/")
install.packages("foreign")
install.packages("fields")
library(foreign)
library(fields)
#
#To begin, I import the data I initially accumulated in another programming language:
ddat<-read.dta("sulfurdemand2008_2014.dta")
#I subset out unnecessary data:
ddat<-subset(ddat, select=c(PlantId,BoilerId,monthly,YEAR,MONTH,State,SO2controls,
SO2byproductprodn,switching,totalbtucoal,sulfurmbtu,totalbtugas,adjsulfur,id))
#A by-month trend, adjusting to this format such that the first month=0:
ddat$monthly<-(ddat$monthly-576)
head(ddat)
summary(ddat)
#Removing outliers, most sulfur content is <6%:
plot(ddat$sulfurmbtu,ddat$totalbtucoal)
ddat<-subset(ddat, ddat$sulfurmbtu<6)
plot(ddat$sulfurmbtu,ddat$totalbtucoal)
#Checking normality:
par(mfcol = c(2,2)) 
hist(ddat$sulfurmbtu)
hist(ddat$totalbtucoal)
hist(ddat$totalbtugas)
hist(ddat$adjsulfur)
plot(ddat$totalbtugas)
#Dealing with 0 observations in sulfur content for the purpose of log transformations only:
tempsulfurmbtu<-ddat$sulfurmbtu
tempsulfurmbtu[tempsulfurmbtu==0]<-0.01
ddat$lnsulfurmbtu<-log(tempsulfurmbtu)
tempadjsulfur<-ddat$adjsulfur
tempadjsulfur[tempadjsulfur==0]<-0.01
ddat$lnadjsulfur<-log(tempadjsulfur)
ddat$lntotalbtucoal<-log(ddat$totalbtucoal)
ddat$lntotalbtugas<-log(ddat$totalbtugas)
#Inspecting the natural log transformation of the data, some of these will end up not being used in the
#present analysis due to model specification but are still interesting to inspect:
par(mfcol = c(2,2)) 
hist(ddat$lnsulfurmbtu)
hist(ddat$lntotalbtucoal)
hist(ddat$lntotalbtugas)
hist(ddat$lnadjsulfur)
summary(ddat)
#Estimating the preferred specifications of my models:
#summary(model1<-lm(adjsulfur~SO2controls,data=ddat))
summary(model2<-plm(lnadjsulfur~SO2controls+monthly,data=ddat,index=c('PlantId')))
summary(model3<-plm(lnadjsulfur~SO2controls+SO2byproductprodn+monthly,data=ddat,index=c('PlantId')))
#summary(model4<-plm(lnadjsulfur~SO2controls+monthly+monthly*SO2controls,data=ddat,index=c('PlantId')))
summary(model5<-plm(lnadjsulfur~monthly*SO2controls*SO2byproductprodn,data=ddat,index=c('PlantId')))
#Preparing figure 1 to include in my report:
par(mfcol=c(1,2))
boxplot(adjsulfur~SO2controls,data=ddat,
xlab='FGD capability',ylab='Sulfur content in pounds per mmBTU',main='Sulfur vs FGD')
boxplot(adjsulfur~SO2byproductprodn,data=ddat,
xlab='Byproduct capability',ylab='Sulfur content in pounds per mmBTU',main='Sulfur vs Byproduct')
#Preparing figure 2 to include in my report:
par(mfcol = c(1,2))
boxplot(adjsulfur[SO2controls==1]~YEAR[SO2controls==1],data=ddat,
xlab='Year',ylab='Sulfur content in pounds per mmBTU',main='FGD enabled plants')
boxplot(adjsulfur[SO2controls==0]~YEAR[SO2controls==0],data=ddat,
xlab='Year',ylab='Sulfur content in pounds per mmBTU',main='Non-FGD plants')
#ZMT