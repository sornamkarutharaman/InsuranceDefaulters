#======================================================
#			InsuranceDefault-Decision Trees
#======================================================

#Loading all the required libraries
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(data.table)
library(ROCR)
library(StatMeasures)
set.seed(420)

#Setting the control parameters
r.ctrl = rpart.control(minsplit=15, minbucket = 5, cp = 0, xval = 5)

#Building the CART model
Decision_Model1 <- rpart(formula = Premium_Defaulted~., data = Insurance_train, method = "class", control = r.ctrl)
Decision_Model1

#Displaying the decision tree
fancyRpartPlot(Decision_Model1)

# Find the CP to prune the tree
printcp(Decision_Model1)
plotcp(Decision_Model1)
Decision_Model1$cptable[which.min(Decision_Model1$cptable[,"xerror"]),"CP"]
# Pruned tree

ptree<- prune(Decision_Model1, cp= 0.001714776 ,"CP")
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")

#Scoring/Predicting the training dataset

Decision_trainpredictclass <- predict(ptree, Insurance_train, type="class")
Decision_trainpredictscore <- predict(ptree, Insurance_train)
head(Decision_trainpredictscore)

library(pROC)
#Building the ROC curve and lift charts
predDT <- prediction(Decision_trainpredictscore[,2], Insurance_train$Premium_Defaulted)
perf <- performance(predDT, "tpr", "fpr")
plot(perf,main = "ROC curve")
rocDT.info <- roc(Insurance_train$Premium_Defaulted ,Decision_trainpredictscore[,2],legacy.axes = TRUE)
rpcDT.df <- data.frame(tpp = rocDT.info$sensitivities*100, fpp = (1-rocDT.info$specificities)*100, thresholds=rocDT.info$thresholds)
write.csv(rpcDT.df , "ROC_DT.xls")


Decision_trainpredictclass
confusion_DT_train <- confusionMatrix(Decision_trainpredictclass, Insurance_train$Premium_Defaulted,positive = '1')
confusion_DT_train


# Scoring test sample and validating the same
Decision_testpredictclass <- predict(ptree, Insurance_test, type="class")
Decision_testpredictscore <- predict(ptree, Insurance_test)
confusion_DT_test <- confusionMatrix(Decision_testpredictclass, Insurance_test$Premium_Defaulted,positive = '1')
confusion_DT_test
predDTtest <- prediction(Decision_testpredictscore[,2], Insurance_test$Premium_Defaulted)
perftest <- performance(predDTtest, measure = "auc")
perftest <- perftest@y.values[[1]]
plot(perftest,main = "ROC curve",print.auc = true)


