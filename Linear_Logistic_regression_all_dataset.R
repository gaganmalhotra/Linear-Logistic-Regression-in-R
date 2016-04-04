#Linear Regression
myData <- read.csv("~/Desktop/db1.csv")

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

#Linear regression Analysis
regModel=lm(Interfacial.Tension..mN.m.~.,data=train)
summary(regModel)

#prediction
prediction=predict(regModel,newdata = test, interval = "confidence")
prediction

#Logistic Regression
library(nnet)
fit<-multinom(Class~.,data=train)
summary(fit)
predictClass=predict(fit,test)
predictClass

#Confusion Matrix
cm <- table(predictClass,test$Class)
print(cm)

#ROC Curve
aa <- roc(Class~.,data=cleanData)
plot(r.sbp,auc=TRUE)

