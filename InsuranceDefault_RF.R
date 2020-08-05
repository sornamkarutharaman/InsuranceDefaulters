
#======================================================
#			InsuranceDefault-RF
#======================================================

library(randomForest)
set.seed(420)

#Baseline Model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
x = Insurance_train[,-14]
y = Insurance_train[,14]
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rfDefault = randomForest(Premium_Defaulted ~ ., data = Insurance_train, 
                         method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)

print(rfDefault)

#Grid Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(420)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(Premium_Defaulted~., data=Insurance_train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

RndForTraintuned = tuneRF(x = Insurance_train[,-14], 
                 y=Insurance_train$Premium_Defaulted,
                 mtryStart = 6, 
                 ntreeTry = 51, 
                 stepFactor = 1.5, 
                 improve = 0.0001, 
                 trace=TRUE, 
                 plot = TRUE,
                 doBest = TRUE,
                 nodesize = 200, 
                 importance=TRUE
)

#important variables

importance(RndForTraintuned)
impVar <- round(randomForest::importance(rndForTrain1), 2)
impVar[order(impVar[,3], decreasing=TRUE),]


# optimum number of trees

plot(RndForTraintuned, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")

# Model performance measures on training dataset

train_perf_dataset<-Insurance_train

#predictions on the training data and measure the prediction error rate. 
RFtrainperfpredictclass = predict(RndForTraintuned, Insurance_train, type="class")
RFtrainperfpredictprob = predict(RndForTraintuned, Insurance_train, type="prob")[,"1"]
train_perf_dataset$deciles <- decile(RFtrainperfpredictprob[,2])

Decision_trainpredictclass
confusion_RF_train <- confusionMatrix(RFtrainpredictclass, Insurance_train$Premium_Defaulted,positive = '1')
confusion_RF_train

RFtestpredictclass = predict(RndForTraintuned, Insurance_test, type="class")
RFtestpredictprob = predict(RndForTraintuned, Insurance_test, type="prob")

confusion_RF_test <- confusionMatrix(RFtestpredictclass[,-14], Insurance_test$Premium_Defaulted,positive = '1')
confusion_RF_test
predRFtest <- prediction(RFtestpredictprob, Insurance_test$Premium_Defaulted)
perftestRF <- performance(predRFtest, measure = "auc")
plot(perftestRF,main = "ROC curve")

perftest <- performance(predDTtest, measure = "auc")
perftestRF <- perftestRF@y.values[[1]]
plot(perftest,main = "ROC curve",print.auc = true)

mean((Insurance_test$Premium_Defaulted[RFtestpredictprob>threshold])=="1")

predRFtest <- prediction(RFtestpredictprob[,2], Insurance_test$Premium_Defaulted)
perftestRF <- performance(predRFtest, measure = "auc")
perftestRF <- perftestRF@y.values[[1]]

