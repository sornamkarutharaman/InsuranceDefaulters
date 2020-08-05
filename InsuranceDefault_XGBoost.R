#======================================================
#			InsuranceDefault-XGBoost
#======================================================

library(gbm)          
library(xgboost)      
library(caret)        

xg_Train<-model.matrix(~.+0,data = Insurance_train[,1:13],with=F)
xg_test <- model.matrix(~.+0,data = Insurance_test[,1:13],with=F)
labels <- as.numeric(as.factor(Insurance_train$Premium_Defaulted))-1
test_label <- as.numeric(as.factor(Insurance_test$Premium_Defaulted))-1

#preparing matrix
dtrain <- xgb.DMatrix(data = xg_Train,label = labels)
dtest <- xgb.DMatrix(data = xg_test,label=test_label)

#Default parameters
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta=0.1,
  gamma=0,
  max_depth=7,
  min_child_weight=7.53,
  subsample=0.835,
  colsample_bytree=0.953
)

xgbcv <- xgb.cv(params = params
                ,data = dtrain
                ,nrounds = 100
                ,nfold = 5
                ,showsd = T
                ,stratified = T
                ,print.every.n = 10
                ,early.stop.round = 20
                ,maximize = F
)



#Build Model
xgb1 <- xgb.train(
  params = params
  ,data = dtrain
  ,nrounds = 8
  ,watchlist = list(train=dtrain,test=dtest)
  ,print.every.n = 10
  ,early.stop.round = 10
  ,maximize = F
  ,eval_metric = "error"
)



# Training & test error plot
e <- data.frame(xgb1$evaluation_log)
plot(e$iter, e$train_error, col = 'blue')
plot(e$iter, e$test_error, col = 'red')

# Feature importance
imp <- xgb.importance(colnames(dtrain), model = xgb1)
print(imp)
xgb.plot.importance(imp)

# Prediction 
XG_Train_Predict <- predict(xgb1,dtrain, type="response")

library(pROC)
rocXG.info <- roc(Insurance_train$Premium_Defaulted ,XG_Train_Predict,legacy.axes = TRUE)

#Probability cut-offs 
rpcXG.df <- data.frame(tpp = rocXG.info$sensitivities*100, fpp = (1-rocXG.info$specificities)*100, thresholds=rocXG.info$thresholds)
write.csv(rpcXG.df , "ROC_XG.xls")
write.csv(rpcXG.df[rpcXG.df$tpp > 65 & rpcXG.df$tpp < 85, ] , "ROC_XG_60.xls")
head(rpcXG.df)
tail(rpcXG.df)

#plot ROC with different probability cutoff to find the cutoff with highest AUC
predicted_response_XG0.5 <- as.numeric(ifelse(XG_Train_Predict >= 0.5, '1', '0'))
predicted_response_XG0.42 <- as.numeric(ifelse(XG_Train_Predict >= 0.426, '1', '0'))
predicted_response_XG0.34 <- as.numeric(ifelse(XG_Train_Predict >= 0.3447, '1', '0'))
predicted_response_XG0.35 <- as.numeric(ifelse(XG_Train_Predict >= 0.3548, '1', '0'))



## draw ROC and AUC using pROC
library(pROC)
library(ROCR)
plot.new()
par(pty = "s")

roc(Insurance_train$Premium_Defaulted ,predicted_response_XG0.5, plot = TRUE, legacy.axes = TRUE,
    percent = TRUE, xlab = "False positive percentage", ylab = "True positive percentage",
    col = "#377eb8" , lwd = 5, print.auc = TRUE)

plot.roc(Insurance_train$Premium_Defaulted ,predicted_response_XG0.34,percent = TRUE,
         col = "#D6604D", lwd = 5,print.auc = TRUE, add= TRUE, print.auc.y = 20 )

plot.roc(Insurance_train$Premium_Defaulted ,predicted_response_XG0.35,percent = TRUE,
         col = "#FF3399", lwd = 5,print.auc = TRUE, add= TRUE, print.auc.y = 10 )

legend("bottomright", legend = c("0.5","0.34","0.35"),
       col = c("#377eb8","#D6604D","#FF3399"), lwd = 5)

#FInal probability cutoff - 0.35
predicted_responseXG0.35 <- factor(ifelse(XG_Train_Predict >= 0.35, '1', '0'))
confusion_XG0.35 <- confusionMatrix(predicted_responseXG0.35, Insurance_train$Premium_Defaulted,positive = '1')
confusion_XG0.35


XG_test_predict<-predict(xgb1, dtest, type="response")
predicted_responsetest_XG0.35 <- factor(ifelse(XG_test_predict >= 0.35, '1', '0'))
confusion_XG_test <- confusionMatrix(predicted_responsetest_XG0.35, Insurance_test$Premium_Defaulted,positive = '1')
confusion_XG_test


predicted_test_XG <- as.numeric(ifelse(XG_test_predict >= 0.35, '1', '0'))

plot.new()
roc(Insurance_test$Premium_Defaulted ,predicted_test_XG, plot = TRUE, legacy.axes = TRUE,
    percent = TRUE, xlab = "False positive percentage", ylab = "True positive percentage",
    col = "#377eb8" , lwd = 5, print.auc = TRUE)

plot.roc(Insurance_train$Premium_Defaulted ,predicted_response_XG0.35,percent = TRUE,
         col = "#D6604D", lwd = 5,print.auc = TRUE, add= TRUE, print.auc.y = 30 )


legend("bottomright", legend = c("Test","Train"),
       col = c("#377eb8","#D6604D"), lwd = 5)

