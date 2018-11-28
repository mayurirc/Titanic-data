#Data Preparation:
library(dplyr)
library(caret)
library(gains)
TitanicData=read.csv('Titanic_Train.csv') 

 #Data Exploration 
 head(TitanicData)
str(TitanicData)
 summary(TitanicData)
 head(TitanicData)
 tail(TitanicData)
 dim(TitanicData)
 nrow(TitanicData) 
 ncol(TitanicData) 
 names(TitanicData) 
 colnames(TitanicData) 
 
 #Missing Data Treatment 
 colSums(is.na(TitanicData)) #Age variable has 177 NA's 
 summary(TitanicData$Age) 
 TitanicData$Age[is.na(TitanicData$Age)] <-  28.00   # replacing all the missing values with the central value (Median value)
 summary(TitanicData$Age)
 
 #Removing unwanted data and deriving new variable Family
 TitanicData=TitanicData[,c(-1,-5,-10,-11,-12,-13)]
TitanicData$family=TitanicData$SibSp+TitanicData$Parch
TitanicData=TitanicData[,c(-6,-7)]
summary(TitanicData)
#Performing one-hot encoding on nominal features 
library('dummies')
TitanicData1=dummy.data.frame(TitanicData, sep='_') 
names(TitanicData1)=make.names(names(TitanicData1))
saveRDS(TitanicData1, 'TitanicData1.rds')    # saving the final transformed data as a serialized file 
TitanicDataRDSFile=readRDS('TitanicData1.rds') 


#Creating sample of train and test data
sampleIndex=sample(1:nrow(TitanicData1), size=0.7*nrow(TitanicData1), replace=FALSE) 
Training=TitanicData1[sampleIndex,] 
Testing=TitanicData1[-sampleIndex,] 
 
# model building using logistic regression
LogisticReg.fit=glm(Survived~ . , data=Training, family=binomial) 
LogisticReg.fit=glm(Survived~Pclass+Sex+Age+family,data=Training, family=binomial)
Testing$defaultP=predict(LogisticReg.fit, newdata=Testing,type="response") 
Testing$Predicted=ifelse((Testing$defaultP>0.3838384),"Yes","No") 
table(Testing$Survived,Testing$Predicted) 
 #82.49

 #applying model on test data set
Titanicdata=read.csv("Titanic_Test.csv")
#Missing Data Treatment 
colSums(is.na(TitanicData)) #no NA
Titanicdata$family=Titanicdata$SibSp+Titanicdata$Parch
Titanicdata=Titanicdata[,c(-1,-4,-7,-8,-10,-11,-12)]
Titanicdata$Pclass=as.numeric(Titanicdata$Pclass)
Testing$defaultP=predict(LogisticReg.fit, newdata=Testing,type="response") 
head(Testing)  
Testing$Predicted=ifelse((Testing$defaultP>0.3838384),"Yes","No") 
Predicted
Survived=Predicted
result_test=data.frame(Titanicdata$PassengerId,Survived)
write.csv(result_test,"result_test.csv",row.names = F)
 

 
 
 