#install "caTools" 
#go to tools menu. Give name caTools and click install
#this is required for the splitting of data

library(caTools)|

#for naive bayes install "e1071" package in the same way as caTools
library(e1071)
library(lattice)
library(ggplot2)
library(caret)


library(rpart)
library(randomForest)

#open a dataset in the dormat of csv file

data <-read.csv(file = "/home/hp/Desktop/insuranceDMW/i2.csv",header=TRUE,sep=",")
View(data)
summary(data)
str(data)

#preprocessing the data by correcting missing values
data$age = ifelse(is.na(data$age),ave(data$age, FUN = function(x) mean(x, na.rm = 'TRUE')),data$age)
data$bmi = ifelse(is.na(data$bmi),ave(data$bmi, FUN = function(x) mean(x, na.rm = 'TRUE')),data$bmi)
data$charges = ifelse(is.na(data$charges),ave(data$charges, FUN = function(x) mean(x, na.rm = 'TRUE')),data$charges)
View(data)

#split the data
temp_field<-sample.split(data,SplitRatio = 0.9)

#keep 90% for training
train<-subset(data,temp_field==TRUE)
View(train)

#calculating the dimension of training 
dim(train)

#keep 30% for testinginstall.packages("e1071", dep = TRUE, type = "source")
test<-subset(data,temp_field==FALSE)
View(test)
dim(test)



############################################### decision tree #################################################################

fit<-rpart(train$insuranceclaim~.,data = train,method = "class")
plot(fit)
text(fit)
preds<-predict(fit,newdata=test[,-8],type=("class"))
mean(preds==test$insuranceclaim)
output1<-cbind(test,preds)
View(output1)
output1
confusionMatrix(table(preds,test$insuranceclaim,dnn=c("predicted","actual")))


############################################## random forest ###################################################################

data$insuranceclaim <- as.factor(data$insuranceclaim) 
table(data$insuranceclaim)
set.seed(123)
ind <- sample(2,nrow(data),replace=TRUE,prob = c(0.9,0.1))
train <- data[ind==1,]
test <- data[ind==2,]
 
set.seed(222)
rf <- randomForest(insuranceclaim~.,data=train)
print(rf)
#attributes(rf)

p2 <- predict(rf,test)
confusionMatrix(p2,test$insuranceclaim)
plot(rf, main ="RandomForest model")
plot(p2, test$insuranceclaim, main="Predicted vs Actual Random Forest ", ylab="Predicted", xlab="Actual")
#VarImpPlot(rf, sort=TRUE, main ="Random Forest Plot")

varImpPlot(rf, sort=TRUE, main ="Random Forest Plot")

####################################################### SVM ##################################################################
dataset <- data.frame(data$bmi,data$smoker,data$children,data$insuranceclaim)
View(dataset)
dataset$data.insuranceclaim= factor(dataset$data.insuranceclaim, levels = c(0, 1)) 
classifier = svm(formula = insuranceclaim ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear') 
pred = predict(classifier, newdata=test[,-8]) 
confusionMatrix(table(pred,test$insuranceclaim,dnn=c("predicted","actual")))
#plot(classifier, main ="svm model")
plot(pred, test$insuranceclaim, main="Predicted vs Actual svm ", ylab="Predicted", xlab="Actual")
#varImpPlot(classifier, sort=TRUE, main ="svm Plot")



################################################## naive bayes ################################################################

# use of naive bayes algorithm
#create a learning model
# in train data outcome field indicates class value (0/1)
# naive bayes want the output not in numeric form but in the category form so use "as.factor"
# use of "~." indicates the outcome to be learned against all other fields of dataset

my_model <-naiveBayes(as.factor(train$insuranceclaim)~.,train)

my_model


#prediction class
#from test data exclude 9th column which is outcome column


pred1<-predict(my_model,test[,-8])

pred1

# to display the probability of outcome use type="raw"

pred1<-predict(my_model,test[,-8],type="raw")


# to display the prediction class of outcome use type="class"

pred1<-predict(my_model,test[,-8],type="class")
pred1


confusionMatrix(table(pred1,test$insuranceclaim,dnn=c("actual","predicted")))
plot(pred1, test$insuranceclaim, main="Predicted vs Actual NB ", ylab="Predicted", xlab="Actual")



#save the prediction 
output<-cbind(test,pred1)
#confusionMatrix(pred1,test$insuranceclaim)
View(output)
dim(output)

