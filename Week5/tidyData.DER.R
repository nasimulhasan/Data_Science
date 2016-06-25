setwd("F:/Introduction to Data Science/Week5/Assessment/Kaggle_Higgs_Boson_Machine_Learning_Challenge")
getwd()

tidyData.DER <- read.csv("tidyData.DER.csv")
dim(tidyData.DER)
View(tidyData.DER)
names(tidyData.DER)
str(tidyData.DER)
summary(tidyData.DER)