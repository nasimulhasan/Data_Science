setwd("F:/Introduction to Data Science/Week5/Assessment/Kaggle_Higgs_Boson_Machine_Learning_Challenge")
getwd()


training <- read.csv("training.csv")
dim(training)
View(training)

test <- read.csv("test.csv")
View(test)
names(test)
dim(test)


train <- training[1:10000, ]
sum(training == -999)
sum(training[, 32] < 1)

(8250000 - 1580052) / 250000
# 27 % (26.67979) of the entry are meaningless(-999)

#discard NAs
#discard NA
training[training==-999] <- NA
NAs <- apply(training, 2, 
              function(x) { sum(is.na(x))})

test[test==-999] <- NA
NAs.test <- apply(test, 2, 
             function(x) { sum(is.na(x))})

NAs.test

(38114 / 250000) * 100
#[1] 15.2456
(99913 / 250000) * 100
#[1] 39.9652
(177457 / 250000) * 100
#[1] 70.9828

train.update <- training[, which(NAs == 0)]
dim(train.update)
View(train.update)

train.mVal <- training[, which(NAs != 0)]
View(train.mVal)
test.update <- test[, which(NAs.test == 0)]
dim(test.update)
head(test.update)

#plot
plot(train.update[, 4], train.update[, 5])

library(caret)

trControl <- trainControl(method = "cv", number = 4)
model.rpart <- train(Label ~ . - (Weight + EventId), 
                   method = "rpart",
                   train.update[1:150000, ])
# DIM                           Accuracy        Kappa
#train.update[1:200000, ]        0.769          NaN 
#train.update[1:50000, ]         0.768          0.467
#train.update[1:20000, ]         0.761          0.452
#train.update[1:150000, ]        0.77           0.472

model.gbm <- train(Label ~ . - (Weight + EventId), 
                     method = "gbm",
                     
                     train.update[1:150000, ])
#0.811 [1:20000, ]

model.rf <- train(Label ~ . - (Weight + EventId),
                     method = "rf",
                     
                     train.update[1:12000, ])
#1:500  0.77
#1:10000 0.809
predict.rpart <- predict(model.rpart, 
                       train.update[160000:250000, ])

predict.gbm <- predict(model.gbm, 
                       train.update[160000:250000, ])

#====================================
predict.gbm = predict(model.gbm, 
                      train.update)
# Change Label to 0,1 for GBM model. b = 0, s = 1
train.update$Label=as.numeric(train.update$Label)-1
predict.gbm = as.numeric(predict.gbm) - 1

# Threshold to set results
threshold = 0.002

table(train.update$Label, predict.gbm>=threshold)

# This code creates a field for predicted response so you can calculate AMS
predicted=rep(0,250000)
predicted[predict.gbm>=threshold]=1

#save response vector to use later
train.y = train.update[,21:22]
AMS(pred=predicted,real=train.y$Label,weight=train.update$Weight)

# Make predictins on test set and create submission file
gbmTestPrediction = predict(model.gbm, 
                            newdata=test)

predicted=rep("b",550000)
predicted[gbmTestPrediction>=threshold]="s"
weightRank = rank(gbmTestPrediction,ties.method= "random")


## Function to calculate AMS from predictions
# Modified from TomHall's code to use s and b instead of 0 and 1
# https://www.kaggle.com/c/higgs-boson/forums/t/8216/r-code-for-ams-metric

AMS = function(pred,real,weight)
{
        #a = table(pred,real)
        pred_s_ind = which(pred==0)                          # Index of s in prediction
        real_s_ind = which(real==0)                          # Index of s in actual
        real_b_ind = which(real==1)                          # Index of b in actual
        s = sum(weight[intersect(pred_s_ind,real_s_ind)])      # True positive rate
        b = sum(weight[intersect(pred_s_ind,real_b_ind)])      # False positive rate
        
        b_tau = 10                                             # Regulator weight
        ans = sqrt(2*((s+b+b_tau)*log(1+s/(b+b_tau))-s))
        return(ans)
}
#==========================================

predict.rf <- predict(model.rf, 
                       train.update[40000:250000, ])

predict.accuracy <- sum(predict.gbm == train.update[160000:250000, 22]) / length(train.update[160000:250000, 22])
predict.accuracyRpart <- sum(predict.rpart == train.update[160000:250000, 22]) / length(train.update[160000:250000, 22])
predict.accuracyGbm <- sum(predict.gbm == train.update[40000:250000, 22]) / length(train.update[40000:250000, 22])

predict.accuracyRf <- sum(predict.rf == train.update[40000:250000, 22]) / length(train.update[40000:250000, 22])

#prediction accuracy
#0.7232156 (rpart)      
#0.774482  0.815[1:10000] (rf)
#0.8138866[1:20000, ] 0.815502[1:150000, ] (gbm)
# Change Label to 0,1 for GBM model. b = 0, s = 1
training$Label=as.numeric(training$Label)-1

training$Label=as.character(as.numeric(training$Label)-1)
training$Label = training[, 33]

set.seed(100)

fit.lm <- lm(Label~.-Weight, training)

fit.glm <- train(Label ~ DER_mass_MMC + DER_mass_transverse_met_lep + 
                         DER_mass_vis + DER_pt_h + DER_mass_jet_jet + DER_prodeta_jet_jet + 
                         DER_deltar_tau_lep + DER_sum_pt + DER_pt_ratio_lep_tau + 
                         DER_met_phi_centrality + DER_lep_eta_centrality + PRI_tau_pt + 
                         PRI_lep_pt + PRI_met + PRI_met_sumet + PRI_jet_num + PRI_jet_leading_phi + 
                         PRI_jet_subleading_eta + PRI_jet_subleading_phi
        , method = "glm"
        , training[1:10000, ]
        ,cv.folds = 5,                   # 5 fold cross validation
                  # Set observation weights 
        interaction.depth=2,            # Set interaction depth
        verbose=TRUE)

backElim <- step(fit.lm, direction = "backward")

fit.gbm <- train(Label~.-Weight, method = "lm", training[500:1000, ])


predict.glm <- predict(fit, training[12000:15000, ])
