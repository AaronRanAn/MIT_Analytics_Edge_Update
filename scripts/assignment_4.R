library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(ROCR)
library(tidyr)
library(mice)
library(caTools)
library(rpart)
library(rpart.plot)


stv = read_csv("../../data/week 4/stevens.csv")

glimpse(stv)

set.seed(3000)

spl = sample.split(stv$Reverse, 0.7)

stv %>% filter(spl==T) -> stv_train

stv %>% filter(spl == F) -> stv_test

### build the tree 

stv_train %>% 
        rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, ., 
              method = "class", minbucket = 25) -> stv_tree

prp(stv_tree)

pred_cart = predict(stv_tree, stv_test, type = "class")

# validate the accuracy using the confusion matrix

table(stv_test$Reverse, pred_cart)

# get ROC

predictROC = predict(stv_tree, stv_test)

pred = prediction(predictROC[,2], stv_test$Reverse)

pref = performance(pred, "tpr", "fpr")

plot(pref)

# get the AUC

as.numeric(performance(pred, "auc")@y.values)

# change the min bucket to 5

stv_train %>% 
        rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, ., 
              method = "class", minbucket = 5) -> stv_tree_2

prp(stv_tree_2)

stv_train %>% 
        rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, ., 
              method = "class", minbucket = 100) -> stv_tree_3

prp(stv_tree_3)

## Random Forrest  ---  build a large number of CART tree

## training data is selection randomly and with replacement

install.packages("randomForest")
library(randomForest)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stv_train, ntree=200, nodesize=25 )


