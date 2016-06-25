setwd("F:/Introduction to Data Science/Week5/Assessment/Kaggle_Higgs_Boson_Machine_Learning_Challenge")
getwd()

training.elimNA <- read.csv("trainingElimNA.csv")

total <- which(grepl("^DER", 
                     colnames(training.elimNA), 
                     ignore.case = F))
trainData.DER <- training.elimNA[, total]

Label <- training.elimNA[, 26:27]

tidyData.DER <- cbind(trainData.DER, Label)
dim(tidyData.DER)
View(tidyData.DER)

library(caret)
library(kernlab)
library(ipred)
library(plyr)

set.seed(100)
partionData <- createDataPartition(y = tidyData.DER$Label,
                                   p = 0.25,
                                   list = FALSE)
print(object.size(partionData), units = "Mb")
trainData.DER <- tidyData.DER[partionData, ]
testData.DER <- tidyData.DER[-partionData, ]

ctrl <- trainControl(method = "cv", 
                     number = 5
                )
modelFit.treebag <- train(Label~., 
                          method = "treebag",
                          data = trainData.DER,
                          preProcess = c("scale", "center"),
                          
                          tuneLength = 15, #how many candidate are evaluated
                          trControl = ctrl
)
#Accuracy .819

modelFit.rf <- train(Label~ DER_mass_MMC + DER_mass_transverse_met_lep + DER_mass_vis + DER_pt_h + DER_deltar_tau_lep + DER_pt_tot + DER_sum_pt + DER_pt_ratio_lep_tau + DER_met_phi_centrality, 
                          method = "rf",
                          data = trainData.DER,
                          preProcess = c("scale", "center"),
                          nodesize = 15,
                          tuneLength = 15, #how many candidate are evaluated
                          trControl = ctrl
                        )
#0.827

modelFit.gbm <- train(Label~ DER_mass_MMC + DER_mass_transverse_met_lep + DER_mass_vis + DER_pt_h + DER_deltar_tau_lep + DER_pt_tot + DER_sum_pt + DER_pt_ratio_lep_tau + DER_met_phi_centrality, 
                     method = "gbm",
                     data = trainData.DER,
                     preProcess = c("scale", "center"),
                     #nodesize = 15,
                     tuneLength = 15, #how many candidate are evaluated
                     trControl = ctrl
)

treebag.predict.DER <- predict(modelFit.treebag,
                               #training.elimNA[1:90000, ]
                               testData.DER
                               #type = "prob" #compute class probabilities from the model
)

treebag.predict.DER <- predict(modelFit.rf,
                               #training.elimNA[1:90000, ]
                               testData.DER
                               #type = "prob" #compute class probabilities from the model
)

gbm.predict.DER <- predict(modelFit.gbm,
                               #training.elimNA[1:90000, ]
                               testData.DER
                               #type = "prob" #compute class probabilities from the model
)

L <- as.numeric(testData.DER$Label) - 1
p <- as.numeric(gbm.predict.DER) - 1
accuracy.train.test <- sum(p == L) / length(p)
confusionMatrix(p, L) 
table(L, p)
#.8145 treebag
#0.8243 rf
threshold <- 0.002

predicted <- rep("b",250000)
predicted <- rep("b",199999)
predicted <- rep("b",50000)
predicted[p >= threshold] <- "s"
AMS(pred=predicted,real=testData.DER$Label,weight=testData.DER$Weight)
AMS(pred=predicted,
    real=training.elimNA$Label, 
    weight=training.elimNA$Weight)

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

#load the updated test data
testData <- read.csv("testElimNA.csv")
dim(testData)
View(testData)
names(testData)

predict.test <- predict(modelFit.gbm,
                               #training.elimNA[1:90000, ]
                        testData
                               #type = "prob" #compute class probabilities from the model
                        )
pred <- as.numeric(predict.test) - 1
predicted=rep("b",550000)
predicted[predict.test>=threshold]="s"
weightRank = rank(predict.test, ties.method= "random")


submission = data.frame(EventId = testData$EventId, 
                        RankOrder = weightRank, 
                        Class = predict.test)
write.csv(submission, "submission_version3.csv", row.names=FALSE)
