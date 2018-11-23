#required libraries
library(lattice)
library(ROCR)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
#loading the dataset
a1<-read.csv("C://Users//Pavi//Downloads//student-alcohol-consumption//student-por.csv")
View(a1)
str(a1)
summary(a1)

#basic EDA
dim(a1)

#histogram

hist(a1$G3)
attach(a1)
histogram(~age|sex,main='Age vs sex')
histogram(~G1|internet,main='grade of students using internet and non internet users')
histogram(~age|internet,main='age using internet and non internet users')
histogram(~G3|Pstatus,main='grade based on pstatus')
histogram(~G3|sex,main='grade based on sex')

#boxplot
bwplot(absences~internet,main='no.of absences based on internet usage')
bwplot(G3~paid,main='grade based on whether taking special classes')
bwplot(G3~activities,main='grade based on extra curricular activities')
bwplot(G3~sex,main='grade based on extra curricular activities')
ggplot(aes(factor(romantic), 
           absences), 
       data = a1) +
  geom_jitter( alpha = .3)  +
  geom_boxplot( alpha = .5,color = 'blue')

ggplot(aes(factor(higher), 
           G3), 
       data = a1) +
  
  geom_boxplot( alpha = .5,color = 'blue')

ggplot(aes(factor(higher), 
           G3), 
       data = a1) +
  
  geom_boxplot( alpha = .5,color = 'blue')
#barcharts
ggplot(a1, aes(higher, ..count..)) + geom_bar(aes(fill = sex), position = "dodge")
ggplot(a1, aes(higher, ..count..)) + geom_bar(aes(fill = romantic), position = "dodge")

cor(G3,age)
# Create Training and Test data -

set.seed(100)
trainingRowIndex <- sample(1:nrow(a1), 0.8*nrow(a1))
trainingData <- a1[trainingRowIndex, ]
testData  <- a1[-trainingRowIndex, ]
testData

model1 <- glm (higher~ ., data = trainingData, family = binomial)
summary(model1)

model <- glm (higher ~ sex+address+Medu+famsup+paid+activities+
                internet+romantic+famrel+goout+absences+G2+G3, data = trainingData, family = binomial)
summary(model)


#predicting the values in the model

res<-predict(model,testData,type="response")
head(res)
head(testData)
#confusion matrix
cm<-table(Actualvalue=testData$higher,PredictedValue=res>0.5)
cm
#accuracy check

acc<-(2+114)/(2+114+12+2)
acc

#confusion matrix
cm<-table(Actualvalue=testData$higher,PredictedValue=res>0.6)
cm
#accuracy check

acc<-(2+113)/(2+114+12+3)
acc
#ROCR Curve

res<-predict(model,trainingData,type="response")
library(ROCR)
ROCRpred <- prediction(res, trainingData$higher)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0.1,by=0.1))


#plot glm

library(ggplot2)
ggplot(trainingData, aes(x=age, y=higher)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)

library(ggplot2)
ggplot(trainingData, aes(x=G3, y=higher)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)

# decision tree

library(rpart)
library(rpart.plot)
tree1<-rpart(higher ~ age+famrel+freetime+sex+address+Medu+goout+absences+G2+G3,data = trainingData)
tree1
rpart.plot(tree1)


#predicting

predict<-predict(tree1,testData,type = 'class')
head(predict)
#accuracy
cm1<-confusionMatrix(table(predict,testData$higher))
cm1

#knn
install.packages('kknn')
library(kknn)
