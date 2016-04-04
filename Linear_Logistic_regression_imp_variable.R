#Linear Regression
myData <- read.csv("~/Downloads/RegressionData.csv")

#Convert Year into categorical data into new Column
myData$YearCategory <- factor(myData$Year)

#Drop year column
dropCol <- c("Year") # List of columns to be removed
cleanData <- myData[,!(names(myData) %in% dropCol)] #remove column "Year"

#Divide the data into training and testing into 75/25
set.seed(133)
random= sample(2,nrow(cleanData),replace=TRUE,prob = c(0.75,0.25))
train = cleanData[random==1,]
test= cleanData[random==2,]

#Linear regression Analysis on important variables
regModel=lm(Interfacial.Tension..mN.m.~Class+YearCategory+Total.Acid.Number..mgKOH..g.,data=train)
summary(regModel)

#prediction
prediction=predict(regModel,newdata = test, interval = "confidence")
prediction

#Logistic Regression on important variables
library(nnet)
fitLogit<-multinom(Class~Interfacial.Tension..mN.m.+Total.Acid.Number..mgKOH..g.+Colour,data=train)
summary(fitLogit)
predictCls=predict(fitLogit,test)
predictCls

#Confusion Matrix
cm <- table(predictCls,test$Class)
print(cm)

#ROC Curve
library(gplots)
library(ROCR)
rocData <- roc(Class~.,data=cleanData)
plot(rocData,auc=TRUE)

