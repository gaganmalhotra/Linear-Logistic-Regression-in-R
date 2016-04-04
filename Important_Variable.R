#Finding the important variables
myData <- read.csv("~/Downloads/RegressionData.csv")

#Convert Year into categorical data into new Column
myData$YearCategory <- factor(myData$Year)

#Drop year column
dropCol <- c("Year") # List of columns to be removed
cleanData <- myData[,!(names(myData) %in% dropCol)] #remove column "Year"


#finding imp variables
library(randomForest)
important <- randomForest(cleanData$Class~., data=cleanData, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)
varImpPlot(important)


#Correlation matrix and plot
library(Hmisc)
library(corrplot)
correlations <- round(cor(cleanData[,c(0:6)]),2)  
correlations
corrplot(correlations,method = "color")



