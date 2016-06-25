setwd("F:/Introduction to Data Science/Week5/Assessment/Kaggle_Higgs_Boson_Machine_Learning_Challenge")
getwd()


#load the updated dataset
training.elimNA <- read.csv("trainingElimNA.csv")

#discard weights
Label <- training.elimNA$Label
predictors <- training.elimNA[, 1:25]
tidyData <- cbind(predictors, Label)
dim(tidyData)
View(tidyData)
print(object.size(tidyData), units = "Mb")


#load the updated test data
testData <- read.csv("testElimNA.csv")
dim(testData)
View(testData)
names(testData)
print(object.size(testData), units = "Mb")

# Model fitting
library(caret)
library(kernlab)
library(ipred)
library(plyr)

set.seed(100)
partionData <- createDataPartition(y = tidyData$Label,
                                   p = 0.2,
                                   list = FALSE)
print(object.size(partionData), units = "Mb")
trainData <- tidyData[partionData, ]
dim(trainData)
names(trainData)

testData.train <- training.elimNA[-partionData, ]
dim(testData.train)
View(trainData)
names(trainData)
print(object.size(trainData), units = "Mb")

#Fit a model
ctrl <- trainControl(method = "cv", 
                     number = 5,
                     allowParallel = TRUE
                     )

modelFit.treebag <- train(Label~., 
                          method = "treebag",
                          data = trainData,
                          preProcess = c("scale", "center"),
                          
                          tuneLength = 15, #how many candidate are evaluated
                          trControl = ctrl
                        )
modelFit.treebag

#predicting on training data
treebag.predict.train1 <- predict(modelFit.treebag,
                           #training.elimNA[1:90000, ]
                           training.elimNA[1:50000, ]
                           #type = "prob" #compute class probabilities from the model
                        )

treebag.predict.train2 <- predict(modelFit.treebag,
                                  #training.elimNA[1:90000, ]
                                  training.elimNA[50001:100000, ]
                                  #type = "prob" #compute class probabilities from the model
                                )
treebag.predict.train3 <- predict(modelFit.treebag,
                                  #training.elimNA[1:90000, ]
                                  training.elimNA[100001:150000, ]
                                  #type = "prob" #compute class probabilities from the model
)

treebag.predict.train4 <- predict(modelFit.treebag,
                                  #training.elimNA[1:90000, ]
                                  training.elimNA[150001:200000, ]
                                  #type = "prob" #compute class probabilities from the model
                                )

treebag.predict.train5 <- predict(modelFit.treebag,
                                  #training.elimNA[1:90000, ]
                                  training.elimNA[200001:250000, ]
                                  #type = "prob" #compute class probabilities from the model
)

#create a vector of the predicted class
treebag.predict.train <- c(treebag.predict.train1, 
                           treebag.predict.train2, 
                           treebag.predict.train3,
                           treebag.predict.train4,
                           treebag.predict.train5
                           )
#Accuracy measure 
L <- as.numeric(training.elimNA$Label) 
accuracy.train <- sum(treebag.predict.train == L) / length(treebag.predict.train)
confusionMatrix(treebag.predict.train, L)                

#predicting on the training set test data
treebag.predict.train.test <- predict(modelFit.treebag,
                                  #training.elimNA[1:90000, ]
                                  testData.train[1:50000, ]
                                  #type = "prob" #compute class probabilities from the model
)

treebag.predict.train.test2 <- predict(modelFit.treebag,
                                  #training.elimNA[1:90000, ]
                                  testData.train[50001:100000, ]
                                  #type = "prob" #compute class probabilities from the model
)

treebag.predict.train.test3 <- predict(modelFit.treebag,
                                  #training.elimNA[1:90000, ]
                                  testData.train[100001:149999, ]
                                  #type = "prob" #compute class probabilities from the model
)

treebag.predict.train.test <- c(treebag.predict.train.test,
                                treebag.predict.train.test2,
                                treebag.predict.train.test3
                                )
#Accuracy measure 
L <- as.numeric(testData.train$Label) 
accuracy.train.test <- sum(treebag.predict.train.test == L) / length(treebag.predict.train.test)
confusionMatrix(treebag.predict.train.test, L) 


#predicting on test data and create submission file
treebag.predict <- predict(modelFit.treebag,
                           #training.elimNA[1:90000, ]
                           testData[1:50000, ]
                           #type = "prob" #compute class probabilities from the model
                        )
treebag.predict2 <- predict(modelFit.treebag,
                           #training.elimNA[1:90000, ]
                           testData[50001:100000, ] #actually test.elimNA
                           #type = "prob" #compute class probabilities from the model
                        )
treebag.predict3 <- predict(modelFit.treebag,
                           #training.elimNA[1:90000, ]
                           testData[100001:150000, ]
                           #type = "prob" #compute class probabilities from the model
                        )
treebag.predict4 <- predict(modelFit.treebag,
                            #training.elimNA[1:90000, ]
                            testData[150001:200000, ]
                            #type = "prob" #compute class probabilities from the model
                        )
treebag.predict5 <- predict(modelFit.treebag,
                            #training.elimNA[1:90000, ]
                            testData[200001:250000, ]
                            #type = "prob" #compute class probabilities from the model
                        )
treebag.predict6 <- predict(modelFit.treebag,
                            #training.elimNA[1:90000, ]
                            testData[250001:300000, ]
                            #type = "prob" #compute class probabilities from the model
                        )
treebag.predict7 <- predict(modelFit.treebag,
                            #training.elimNA[1:90000, ]
                            testData[300001:350000, ]
                            #type = "prob" #compute class probabilities from the model
                        )
treebag.predict8 <- predict(modelFit.treebag,
                            #training.elimNA[1:90000, ]
                            testData[350001:400000, ]
                            #type = "prob" #compute class probabilities from the model
                        )
treebag.predict9 <- predict(modelFit.treebag,
                            #training.elimNA[1:90000, ]
                            testData[400001:450000, ]
                            #type = "prob" #compute class probabilities from the model
                        )
treebag.predict10 <- predict(modelFit.treebag,
                            #training.elimNA[1:90000, ]
                            testData[450001:500000, ]
                            #type = "prob" #compute class probabilities from the model
                        )
treebag.predict11 <- predict(modelFit.treebag,
                            #training.elimNA[1:90000, ]
                            testData[500001:550000, ]
                            #type = "prob" #compute class probabilities from the model
                        )

#create column vector of the predicted class 
treebag.predict.test <- c(treebag.predict, 
                          treebag.predict2, 
                          treebag.predict3,
                          treebag.predict4,
                          treebag.predict5,
                          treebag.predict6,
                          treebag.predict7,
                          treebag.predict8,
                          treebag.predict9,
                          treebag.predict10,
                          treebag.predict11
                          )

# Threshold to set results
threshold = 0.002
treebag.predict.test <- treebag.predict.test - 1

predicted=rep("b",550000)
predicted[treebag.predict.test>=threshold]="s"
weightRank = rank(treebag.predict.test, ties.method= "random")


submission = data.frame(EventId = testData$EventId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "submission_version1.csv", row.names=FALSE)






train_ams <- as.numeric(training.elimNA$Label) - 1
train_ams <- as.numeric(testData.train$Label) - 1
train_ams <- L - 1
predict_ams <- as.numeric(treebag.predict.train) - 1

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
predicted <- rep("b",199999)
predicted <- rep("b",50000)
predicted[predict_ams >= threshold] <- "s"
AMS(pred=predicted,real=training.elimNA$Label,weight=training.elimNA$Weight)
AMS(pred=predicted,
    real=testData.train$Label, 
    weight=testData.train$Weight)

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


#===================
ROC.imp <- filterVarImp(x = trainData[, -ncol(trainData)],
                        y = trainData$Label)
# ROC.imp
# b         s
# X                           0.5016555 0.5016555
# EventId                     0.5016555 0.5016555
# DER_mass_MMC                0.6048648 0.6048648
# DER_mass_transverse_met_lep 0.7402352 0.7402352
# DER_mass_vis                0.5881139 0.5881139
# DER_pt_h                    0.6188535 0.6188535
# DER_deltar_tau_lep          0.5133194 0.5133194
# DER_pt_tot                  0.5111608 0.5111608
# DER_sum_pt                  0.6324012 0.6324012
# DER_pt_ratio_lep_tau        0.6639560 0.6639560
# DER_met_phi_centrality      0.6385535 0.6385535
# PRI_tau_pt                  0.6951879 0.6951879
# PRI_tau_eta                 0.5013400 0.5013400
# PRI_tau_phi                 0.5054810 0.5054810
# PRI_lep_pt                  0.5261457 0.5261457
# PRI_lep_eta                 0.5019938 0.5019938
# PRI_lep_phi                 0.5042687 0.5042687
# PRI_met                     0.5332799 0.5332799
# PRI_met_phi                 0.5020346 0.5020346
# PRI_met_sumet               0.6082765 0.6082765
# PRI_jet_num                 0.5899707 0.5899707
# PRI_jet_leading_pt          0.5414440 0.5414440
# PRI_jet_leading_eta         0.4976433 0.4976433
# PRI_jet_leading_phi         0.4999396 0.4999396
# PRI_jet_all_pt              0.6108971 0.6108971


treebag.imp <- varImp(modelFit.treebag, 
                      scale = FALSE)

# treebag.imp
# treebag variable importance
# 
# only 20 most important variables shown (out of 25)
# 
# Overall
# DER_mass_MMC                   9102
# DER_mass_transverse_met_lep    6923
# PRI_tau_pt                     6752
# DER_mass_vis                   5813
# DER_met_phi_centrality         4571
# DER_pt_ratio_lep_tau           4503
# DER_deltar_tau_lep             3990
# DER_pt_h                       3287
# DER_sum_pt                     3101
# DER_pt_tot                     2699
# X                              2652
# EventId                        2383
# PRI_met                        2375
# PRI_lep_eta                    2095
# PRI_tau_eta                    2044
# PRI_jet_leading_eta            1948
# PRI_tau_phi                    1869
# PRI_met_sumet                  1827
# PRI_lep_phi                    1721
# PRI_met_phi                    1700

