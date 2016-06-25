setwd("F:/Introduction to Data Science/Week5/Assessment/Kaggle_Higgs_Boson_Machine_Learning_Challenge")
getwd()

tidyData.PRI <- read.csv("tidyData.PRI.csv")
dim(tidyData.PRI)
View(tidyData.PRI)
names(tidyData.PRI)
str(tidyData.PRI)
summary(tidyData.PRI)