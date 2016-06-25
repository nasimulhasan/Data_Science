setwd("F:/Introduction to Data Science/Week5/Assessment/Kaggle_Higgs_Boson_Machine_Learning_Challenge")
getwd()


#load the data
training <- read.csv("training.csv")
dim(training)
View(training)
names(training)
str(training)
summary(training)

#split the data based on b vs s
background <- training[training$Label == "b", ]
dim(background)
View(background)
names(background)
str(background)
summary(background)

write.csv(background, "background.csv", row.names=FALSE)

signal <- training[training$Label == "s", ]
dim(signal)
View(signal)
names(signal)
str(signal)
summary(signal)

write.csv(signal, "signal.csv", row.names=FALSE)

#split the dataset based on DER and PRI
total <- which(grepl("^DER", 
                     colnames(training), 
                     ignore.case = F))
trainData.DER <- training[, total]
Weight.Label <- training[, 32:33]

tidyData.DER <- cbind(trainData.DER, Weight.Label)
dim(tidyData.DER)
View(tidyData.DER)
names(tidyData.DER)

write.csv(tidyData.DER, "tidyData.DER.csv", row.names=FALSE)


total.PRI <- which(grepl("^PRI", 
                     colnames(training), 
                     ignore.case = F))
trainData.PRI <- training[, total.PRI]
Weight.Label <- training[, 32:33]

tidyData.PRI <- cbind(trainData.PRI, Weight.Label)
dim(tidyData.PRI)
View(tidyData.PRI)
names(tidyData.PRI)

write.csv(tidyData.PRI, "tidyData.PRI.csv", row.names=FALSE)

#compute the meaningless variables
training[training==-999] <- NA
meaningless <- apply(training, 2, 
             function(x) { sum(is.na(x))})

meaningless

data.meaningless <- training[, meaningless != 0]
dim(data.meaningless)
str(data.meaningless)
View(data.meaningless)

#eliminating 71% (NA) included col
data1 <- data.meaningless[, 1]
data2 <- data.meaningless[, 6:8]
dataFrame <- data.frame(data1, data2)
dim(dataFrame)
View(dataFrame)

#replace missing value (NA) with col mean
mn1 <- mean(data.meaningless$DER_mass_MMC, na.rm = T)
data.meaningless[is.na(data.meaningless$DER_mass_MMC) == TRUE, 1] <- mn1

mn2 <- mean(data.meaningless$PRI_jet_leading_pt, na.rm = T)
data.meaningless[is.na(data.meaningless$PRI_jet_leading_pt) == TRUE, 6] <- mn2

mn3 <- mean(data.meaningless$PRI_jet_leading_eta, na.rm = T)
data.meaningless[is.na(data.meaningless$PRI_jet_leading_eta) == TRUE, 7] <- mn3

mn4 <- mean(data.meaningless$PRI_jet_leading_phi, na.rm = T)
data.meaningless[is.na(data.meaningless$PRI_jet_leading_phi) == TRUE, 8] <- mn4

View(data.meaningless)

meaningless.reshaped <- apply(data.meaningless, 2, 
                     function(x) { sum(is.na(x))})
data.meaningless.updated <- data.meaningless[, meaningless.reshaped == 0]

#update trianing data
mn1 <- mean(training$DER_mass_MMC, na.rm = T)
training[is.na(training$DER_mass_MMC) == TRUE, 2] <- mn1

mn2 <- mean(training$PRI_jet_leading_pt, na.rm = T)
training[is.na(training$PRI_jet_leading_pt) == TRUE, 25] <- mn2

mn3 <- mean(training$PRI_jet_leading_eta, na.rm = T)
training[is.na(training$PRI_jet_leading_eta) == TRUE, 26] <- mn3

mn4 <- mean(training$PRI_jet_leading_phi, na.rm = T)
training[is.na(training$PRI_jet_leading_phi) == TRUE, 27] <- mn4

View(training)

#update test data set
mn1 <- mean(testData$DER_mass_MMC, na.rm = T)
testData[is.na(testData$DER_mass_MMC) == TRUE, 2] <- mn1

mn2 <- mean(testData$PRI_jet_leading_pt, na.rm = T)
testData[is.na(testData$PRI_jet_leading_pt) == TRUE, 25] <- mn2

mn3 <- mean(testData$PRI_jet_leading_eta, na.rm = T)
testData[is.na(testData$PRI_jet_leading_eta) == TRUE, 26] <- mn3

mn4 <- mean(testData$PRI_jet_leading_phi, na.rm = T)
testData[is.na(testData$PRI_jet_leading_phi) == TRUE, 27] <- mn4

View(testData)
#eliminate missing values
NAs <- apply(training, 2, 
                     function(x) { sum(is.na(x))})

NAs

#new version of trianing data after eliminating NAs
training.elimNA <- training[, NAs == 0]
training.elimNA <- training.elimNA[, 1:26]
dim(training.elimNA)
View(training.elimNA)


#eliminate missing values
NAs <- apply(testData, 2, 
             function(x) { sum(is.na(x))})

NAs

#new version of trianing data after eliminating NAs
testData.elimNA <- testData[, NAs == 0]
testData.elimNA <- testData.elimNA[, 2:26]
dim(testData.elimNA)
View(testData.elimNA)

library(Hmisc)
m.cor <- rcorr(as.matrix(data.meaningless)) 

write.csv(training.elimNA, file = "trainingElimNA.csv")
write.csv(testData.elimNA, file = "testElimNA.csv")


testData.elimNA <- read.csv("testElimNA.csv")
dim(testData.elimNA)
#==============================
#load the updated dataset
training.elimNA <- read.csv("trainingElimNA.csv")
dim(training.elimNA)
View(training.elimNA)
print(object.size(training.elimNA), units = "Mb")
training.elimNA <- training.elimNA[, 2:26]

b_s <- table(training.elimNA$Label)
# b      s 
# 164333  85667 

#How unpredictable the data
#Expected information (Entropy) - decision
# entropy <- sum(pr * log(pr))
# entropy <- ((85667 / 250000 + log(85667 / 250000)) 
#           + (164333 / 250000 + log(164333 / 250000))
#0.9273489445004166 - python implementation

# Model fitting
library(caret)
library(kernlab)
library(ipred)
library(plyr)

set.seed(100)
partionData <- createDataPartition(y = training.elimNA$Label,
                                 p = 0.2,
                                 list = FALSE)
print(object.size(partionData), units = "Mb")

trainData <- training.elimNA[partionData, ]
print(object.size(trainData), units = "Mb")

dim(trainData)
View(trainData)
trainData.b_s <- table(trainData$Label)
trainData.b_s

# b     s 
# 98600 5140

testData <- training.elimNA[-partionData, ]
testData.b_s <- table(testData$Label)
testData.b_s
# b     s 
# 65733 34266

trControl <- trainControl(method = "cv", 
                          number = 5,
                          repeats = 3, #controls the number of repetitions
                          classProbs = TRUE #requirement for 'ROC'
                          )
ctrl <- trainControl(method = "cv", 
                     number = 5
                     )

#train rpart
modelFit.rpart <- train(trainData$Label ~. -Weight, 
                        method = "rpart",
                        trainData,
                        preProcess = c("scale", "center"),
                        tuneLength = 15, #how many candidate are evaluated
                        trControl = trControl,
                        metric = "ROC" #select the optimal model using the largest value.
                        )

# CART 
# 
# 150001 samples
# 24 predictors
# 2 classes: 'b', 's' 
# 
# Pre-processing: scaled, centered 
# Resampling: Cross-Validated (4 fold) 
# 
# Summary of sample sizes: 112501, 112501, 112500, 112501 
# 
# Resampling results across tuning parameters:
#         
#         cp        Accuracy  Kappa  Accuracy SD  Kappa SD
# 0.000623  0.816     0.583  0.00164      0.00597 
# 0.000674  0.815     0.579  0.00254      0.00931 
# 0.000739  0.814     0.575  0.00171      0.00578 
# 0.000768  0.814     0.575  0.00169      0.00491 
# 0.000885  0.813     0.574  0.00161      0.00496 
# 0.00106   0.812     0.572  0.00259      0.00632 
# 0.00125   0.811     0.571  0.00285      0.00542 
# 0.00126   0.811     0.571  0.00285      0.00542 
# 0.00133   0.811     0.569  0.00274      0.00647 
# 0.00145   0.811     0.569  0.00274      0.00647 
# 0.00355   0.804     0.561  0.00257      0.00792 
# 0.00453   0.803     0.556  0.00191      0.00503 
# 0.00733   0.802     0.554  0.00169      0.00456 
# 0.055     0.785     0.511  0.0121       0.0241  
# 0.174     0.716     0.245  0.0676       0.283   
# 
# Accuracy was used to select the optimal model using 
# the largest value.
# The final value used for the model was cp = 0.000623.  

rpart.predict <- predict(modelFit.rpart,
                         testData
                         #type = "prob" #compute class probabilities from the model
                         )

#for using type = prob
head(rpart.predict)
# b         s
# 3  0.6966089 0.3033911
# 4  0.8910352 0.1089648
# 7  0.2864907 0.7135093
# 13 0.2158590 0.7841410
# 14 0.6796091 0.3203909
# 21 0.8910352 0.1089648

#may get error message when predict() func use type = "prob"
rpart.accuracy <- sum(rpart.predict == testData$Label) / length(rpart.predict)
rpart.accuracy
#[1] 0.8153382

#may get error message when predict() func use type = "prob"
confusionMatrix(rpart.predict, testData$Label)
#Confusion Matrix and Statistics

# Reference
# Prediction     b     s
# b 58564 11297
# s  7169 22969
# 
# Accuracy : 0.8153          
# 95% CI : (0.8129, 0.8177)
# No Information Rate : 0.6573          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : NA              
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.8909          
# Specificity : 0.6703          
# Pos Pred Value : 0.8383          
# Neg Pred Value : 0.7621          
# Prevalence : 0.6573          
# Detection Rate : 0.5856          
# Detection Prevalence : 0.6986          
# Balanced Accuracy : 0.7806          
# 
# 'Positive' Class : b               
# 
# Warning message:
# In ni[1:m] * nj[1:m] : NAs produced by integer overflow


modelFit.svmRadial <- train(trainData$Label ~. -Weight, 
                        method = "svmRadial",
                        trainData,
                        preProcess = c("scale", "center"),
                        tuneLength = 15, #how many candidate are evaluated
                        trControl = ctrl
                        )

svmRadial.predict <- predict(modelFit.svmRadial,
                             training.elimNA
                         #type = "prob" #compute class probabilities from the model
                        )

svmRadial.accuracy <- sum(svmRadial.predict == testData$Label) / length(svmRadial.predict)
svmRadial.accuracy

confusionMatrix(svmRadial.predict, testData$Label)
#         FALSE   TRUE
#         0 143900  20433
#         1  27199  58468

# 2.61082
# 2.636187


modelFit.gbm <- train(trainData$Label ~. -Weight, 
                            method = "gbm",
                            trainData,
                            preProcess = c("scale", "center"),
                            tuneLength = 15, #how many candidate are evaluated
                            trControl = ctrl
                        )

gbm.predict <- predict(modelFit.gbm,
                             training.elimNA
                             #type = "prob" #compute class probabilities from the model
                        )

gbm.accuracy <- sum(gbm.predict == training.elimNA$Label) / length(gbm.predict)
gbm.accuracy
#0.83628
#[1] 0.838488 [train 75000]
confusionMatrix(gbm.predict, training.elimNA$Label)
#Reference
#Prediction      b      s
#b 147392  23437
#s  16941  62230


modelFit.treebag <- train(trainData$Label ~. -Weight, 
                      method = "treebag",
                      trainData,
                      preProcess = c("scale", "center"),
                      tuneLength = 15, #how many candidate are evaluated
                      trControl = ctrl
                )

treebag.predict <- predict(modelFit.treebag,
                           #training.elimNA[1:90000, ]
                           testData.elimNA
                       #type = "prob" #compute class probabilities from the model
)

treebag.accuracy <- sum(treebag.predict == training.elimNA[1:90000, 25]) / length(training.elimNA[1:90000, 25])
treebag.accuracy
#0.857468 
#0.9094381 [125k]

confusionMatrix(treebag.predict, training.elimNA[1:90000, 25])
# Reference
# Prediction      b      s
# b 148811  20111
# s  15522  65556
#===============================================
train_ams <- as.numeric(training.elimNA$Label) - 1
train_ams <- as.numeric(training.elimNA[1:90000, 25]) - 1
train_ams <- as.numeric(testData[1:50000, 25]) - 1
predict_ams <- as.numeric(treebag.predict) - 1

library(pROC)
auc = roc(train_ams, predict_ams)
plot(auc, print.thres=TRUE)

#Area under the curve: 0.8117 gbm [train 75000]
#Area under the curve: 0.8354 treebag [train 50000]
# Threshold to set results
threshold = 0.002


table(train_ams, predict_ams >= threshold)
# train_ams  FALSE   TRUE
#         0 143900  20433
#         1  27199  58468

#Reference - treebag
#Prediction      b      s
#b 148811  20111
#s  15522  65556


# This code creates a field for predicted response so you can calculate AMS
predicted <- rep("b",250000)
predicted <- rep("b",89999)
predicted <- rep("b",50000)
predicted[predict_ams >= threshold] <- "s"
AMS(pred=predicted,real=training.elimNA$Label,weight=training.elimNA$Weight)
AMS(pred=predicted,
    real=training.elimNA[1:90000, 25], 
    weight=training.elimNA[1:90000, 24])

AMS(pred=predicted,
    real=testData[1:50000, 25], 
    weight=testData[1:50000, 24])
# 2.61082 svm
# [1] 2.636187 gbm
#     2.846631 gbm[train 50000]
#     2.87835 gbm[train 75000]
#     3.184704 treebag [50000]
#     3.563175 treebag [125000]

AMS = function(pred,real,weight)
{
        #a = table(pred,real)
        pred_s_ind = which(pred=="s")                          # Index of s in prediction
        real_s_ind = which(real=="s")                          # Index of s in actual
        real_b_ind = which(real=="b")                          # Index of b in actual
        s = sum(weight[intersect(pred_s_ind,real_s_ind)])      # True positive rate
        b = sum(weight[intersect(pred_s_ind,real_b_ind)])      # False positive rate
        
        b_tau = 10                                             # Regulator weight
        ans = sqrt(2 * ((s+b+b_tau) * log(1 + s / (b+b_tau)) - s))
        return(ans)
}

#======================================================
#load test data set
testData <- read.csv("test.csv")


#compute the meaningless variables
testData[testData==-999] <- NA
testData <- apply(testData, 2, 
                     function(x) { sum(is.na(x))})
# Make predictins on test set and create submission file
svmRadial.predict.test <- predict(modelFit.svmRadial,
                                  testData
                                  #type = "prob" #compute class probabilities from the model
                                )

treebag.predict.test <- predict(modelFit.treebag,
                                  testData
                                  #type = "prob" #compute class probabilities from the model
                                )

predicted=rep("b",550000)
predicted[gbmTestPrediction>=threshold]="s"
weightRank = rank(gbmTestPrediction,ties.method= "random")


submission = data.frame(EventId = test$EventId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "submission.csv", row.names=FALSE)
