#installing required packages
library(dplyr)
library(lattice)
library(corrplot)
#importing air compressor dataset as csv format
air<-read.csv("C://Users//Pavi//Downloads//air.csv")
View(air)

#converting dataframe into csv
airr<-data.frame(air)
View(airr)

# checking for null values if null values occur list them out
list_na <- colnames(airr)[ apply(airr, 2, anyNA) ]
list_na

#omitting the null values
airr_drop <-airr %>%na.omit()

#after omitting the null values check the dimension of the dataset
dim(airr_drop)

#claculating the mean value for those missing values
average_missing <- apply(airr[,colnames(airr) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)
average_missing

# replacing missing values with mean 
airr_replace <-airr  %>%
  mutate(Max_Pressure_bar  = ifelse(is.na(Max_Pressure_bar), average_missing[1], Max_Pressure_bar),
         Max_Pressure_psi = ifelse(is.na(Max_Pressure_psi), average_missing[2], Max_Pressure_psi),
         Free_Air_Delivery_min = ifelse(is.na(Free_Air_Delivery_min), average_missing[2], Free_Air_Delivery_min),
         Free_Air_Delivery_cfm = ifelse(is.na(Free_Air_Delivery_cfm), average_missing[2], Free_Air_Delivery_cfm),
         Length   = ifelse(is.na(Length), average_missing[2], Length  ),
         Breadth = ifelse(is.na(Breadth), average_missing[2], Breadth),
         Height = ifelse(is.na(Height), average_missing[2], Height),
         Weight = ifelse(is.na(Weight), average_missing[2], Weight),
         Noise = ifelse(is.na(Noise), average_missing[2], Noise),
         Air_receiver = ifelse(is.na(Air_receiver), average_missing[2], Air_receiver),
         compressor_rpm = ifelse(is.na(compressor_rpm), average_missing[2], compressor_rpm),
         no.of.cylinder = ifelse(is.na(no.of.cylinder), average_missing[2], no.of.cylinder),
         piston.displacement_imp = ifelse(is.na(piston.displacement_imp), average_missing[2], piston.displacement_imp),
         piston.displacement_cfm = ifelse(is.na(piston.displacement_cfm), average_missing[2], piston.displacement_cfm),
         working.pressure_bar = ifelse(is.na(working.pressure_bar), average_missing[2], working.pressure_bar),
         working.pressure_psi = ifelse(is.na(working.pressure_psi), average_missing[2], working.pressure_psi),
         capacity_m3.hr = ifelse(is.na(capacity_m3.hr), average_missing[2], capacity_m3.hr),
         capacity_cfm  = ifelse(is.na(capacity_cfm), average_missing[2],  capacity_cfm ))
View(airr_replace)

#viewing the structure and summary
str(airr_replace)
summary(airr_replace)


#histogram for different model compressor
eg<-airr_replace[c('Model','Max_Pressure_bar')]
View(eg)
model<-hist(eg$Max_Pressure_bar)
abline(v=mean(eg$Max_Pressure_bar),col="red",lwd=4)
abline(v=median(eg$Max_Pressure_bar),col="blue",lwd=4)
h1<-legend(x="topright",c("mean","median"),col=c("red","blue"),lwd=c(4,4))

eg1<-airr_replace[c('Model','Max_Pressure_psi')]
View(eg1)
model<-hist(eg1$Max_Pressure_psi)
abline(v=mean(eg1$Max_Pressure_psi),col="red",lwd=4)
abline(v=median(eg1$Max_Pressure_psi),col="blue",lwd=4)
h1<-legend(x="topright",c("mean","median"),col=c("red","blue"),lwd=c(4,4))


eg2<-airr_replace[c('Model','Height')]
model<-hist(eg2$Height)
abline(v=mean(eg2$Height),col="red",lwd=4)
abline(v=median(eg2$Height),col="blue",lwd=4)
h1<-legend(x="topright",c("mean","median"),col=c("red","blue"),lwd=c(4,4))


eg3<-airr_replace[c('Model','Weight')]
model<-hist(eg3$Weight)
abline(v=mean(eg3$Weight),col="red",lwd=4)
abline(v=median(eg3$Weight),col="blue",lwd=4)
h1<-legend(x="topright",c("mean","median"),col=c("red","blue"),lwd=c(4,4))

eg4<-airr_replace[c('Model','Length')]
model<-hist(eg4$Length)
abline(v=mean(eg4$Length),col="red",lwd=4)
abline(v=median(eg4$Length),col="blue",lwd=4)
h1<-legend(x="topright",c("mean","median"),col=c("red","blue"),lwd=c(4,4))

eg5<-airr_replace[c('Model','Breadth')]
model<-hist(eg5$Breadth)
abline(v=mean(eg5$Breadth),col="red",lwd=4)
abline(v=median(eg5$Breadth),col="blue",lwd=4)
h1<-legend(x="topright",c("mean","median"),col=c("red","blue"),lwd=c(4,4))

#comparing the model
attach(airr_replace)
histogram(~Length|Model,main="length of air compressor based on model")

histogram(~Weight|Model,main="weight of air compressor based on model")


#barplot
a<-aggregate(airr_replace$Air_receiver,by=list(airr_replace$Model),FUN=sum)
aa<-data.frame(a)
barchart(a$Group.1 ~ a$x,main="Air receiver based on model",horizontal=TRUE,xlab = "Air receiver",ylab = "model",las=4)

b<-aggregate(airr_replace$working.pressure_bar,by=list(airr_replace$Model),FUN=sum)
bb<-data.frame(b)
barchart(b$Group.1 ~ b$x,main="Working pressure based on model",horizontal=TRUE,xlab = "Air receiver",ylab = "model",las=4)

c<-aggregate(airr_replace$Free_Air_Delivery_min,by=list(airr_replace$Model),FUN=sum)
cc<-data.frame(c)
barchart(c$Group.1 ~ c$x,main="free air delivery based on model",horizontal=TRUE,xlab = "Air receiver",ylab = "model",las=4)


#corrplot
attach(airr_replace)
sapply(airr_replace, class)
M1<-cor(airr_replace[sapply(airr_replace, function(airr_replace) !is.factor(airr_replace))])
corrplot(M1,method = "number",type = "full")
cor(Length,Breadth)
cor(Max_Pressure_psi,Weight)
# Create Training and Test data -
set.seed(100)
trainingRowIndex <- sample(1:nrow(airr_replace), 0.8*nrow(airr_replace))
trainingData <- airr_replace[trainingRowIndex, ]
testData  <- airr_replace[-trainingRowIndex, ]
#linear regressiom

lm<-lm(Length ~ Breadth,data=trainingdata)
summary(lm)
Pred <- predict(lm, testData)
actuals_preds <- data.frame(cbind(actuals=testData$Length, predicteds=Pred))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy
DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)
plot(testData$Length,type = 'l',lty=1.8,col='green')
lines(Pred,type = 'l',col='blue')
