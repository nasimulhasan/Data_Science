#set up directory
setwd("F:/Introduction to Data Science/Week5/Assessment/Kaggle_Higgs_Boson_Machine_Learning_Challenge")
getwd()

#load the trianing data
training <- read.csv("training.csv")
dim(training)
View(training)
names(training)
