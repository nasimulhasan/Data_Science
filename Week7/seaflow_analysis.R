setwd("F:/Taufeeq/ML/Introduction to Data Science/Week7/Assessment")
getwd()

data <- read.csv("seaflow_21min.csv")
dim(data)
View(data)
names(data)
str(data)
summary(data$fsc_small)

#Q1
sum(data$pop == "synecho")
# 18146
table(data$pop)
# crypto    nano    pico synecho   ultra 
# 102   12698   20860   18146   20537 

#Q2
summary(data$fsc_small)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10000   31340   35480   34920   39180   65420 

#Q3
summary(trainData$time)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 12.0   174.0   362.0   341.4   502.0   643.0 

#Q4
qplot(pe, chl_small, data = data, col = pop)
#nano, pico

#Q5, 6, 7
library(caret)
set.seed(100)
partition <- createDataPartition(data$pop,
                                 p = 0.5,
                                 list = FALSE)
trainData <- data[partition, ]
dim(trainData)
names(trainData)
summary(trainData)

testData <- data[-partition, ]
dim(testData)

summary(trainData)
qplot(fsc_small, fsc_perp, col = pop, data = trainData)  
qplot(fsc_perp, col = pop, data = trainData)
qplot(fsc_big, col = pop, data = trainData)
qplot(pe, col = pop, data = trainData)
qplot(chl_big, col = pop, data = trainData)
qplot(chl_small, col = pop, data = trainData)

table(trainData$fsc_small)
table(trainData$fsc_perp)
table(trainData$fsc_big)
table(trainData$chl_big)
table(trainData$chl_small)
table(trainData$pe)


trainData[trainData == 0] <- NA
NAs <- apply(trainData, 2, 
                     function(x) { sum(is.na(x))})
trainData[is.na(trainData$chl_big) == TRUE, 11] <- 9909.179


#Q5, 6, 7
library(rpart)
library(rattle)
library(e1071)

ctrl <- trainControl(method = "cv",
                     number = 5
                     )

#decision tree
model.rpart <- train(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small,
               data = trainData,
               
               method = "rpart")

model <- rpart(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small,
               method="class", 
               data=trainData)

#random forest
t <- trainData[1:1000, ]
model.rf <- train(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small,
                  data = trainData,
                  trControl = ctrl,
                  nodesize = 15,
                  method = "rf")

#support vector machine
model.svm <- train(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small,
                     data = trainData.updated,
                   trControl = ctrl,
                     method = "svmRadial")
print(model)
rattle(model)


predict.rpart <- predict(model,
                         testData                         
                        )

accuracy <- sum(predict.rpart == (as.numeric(testData$pop) - 1))/ length(predict.rpart)
confusionMatrix(predict.rpart, testData$pop)

predict.svm <- predict(model.svm,
                       testData.updated
                       )

table(pred = predict.svm, true = testData$pop)
confusionMatrix(predict.svm, testData.updated$pop)
#Accuracy : 0.9204    
predict.rf <- predict(model.rf,
                       testData
                        )
confusionMatrix(predict.rf, testData$pop)
#pe 5004

#Variable importance
RocImp2 <- varImp(model.rf, scale = FALSE)
#update data set
dim(data)
data.updated <- data[data$file_id != 208, ]
dim(data.updated)

partition <- createDataPartition(data.updated$pop,
                                 p = 0.5,
                                 list = FALSE)
trainData.updated <- data[partition, ]

testData.updated <- data.updated[-partition, ]
