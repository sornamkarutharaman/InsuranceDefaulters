#======================================================
#			InsuranceDefault-LR
#======================================================

library (MASS)
library(car)
library(caret)
library(logistf)

#Build Baseline LR Model
LR_Model <- glm(Premium_Defaulted~., data=Insurance_train, family=binomial)
summary(LR_Model)
LR_train_predict<-predict(LR_Model, Insurance_train[1:19], type="response")

#ROC
library(pROC)
roc.info <- roc(Insurance_train$Premium_Defaulted ,LR_train_predict,legacy.axes = TRUE)
rpcLR.df <- data.frame(tpp = roc.info$sensitivities*100, fpp = (1-roc.info$specificities)*100, thresholds=roc.info$thresholds)

#Probability cut-offs 
write.csv(rpcLR.df[rpcLR.df$tpp > 35 & rpcLR.df$tpp < 85, ] , "ROC_LR_60_new.xls")
head(rpcLR.df)
tail(rpcLR.df)

#Try different probabilities to plot ROC
predicted_response_LR0.02 <- as.numeric(ifelse(LR_train_predict >= 0.0291, '1', '0'))
predicted_response_LR0.04 <- as.numeric(ifelse(LR_train_predict >= 0.0441, '1', '0'))
predicted_train_LR0.05 <- as.numeric(ifelse(LR_train_predict >= 0.0525, '1', '0'))
predicted_response_LR0.5 <- as.numeric(ifelse(LR_train_predict >= 0.0525, '1', '0'))

library(pROC)
library(ROCR)
plot.new()
par(pty = "s")

roc(Insurance_train$Premium_Defaulted ,predicted_response_LR0.5, plot = TRUE, legacy.axes = TRUE,
    percent = TRUE, xlab = "False positive percentage", ylab = "True positive percentage",
    col = "#377eb8" , lwd = 5, print.auc = TRUE)

plot.roc(Insurance_train$Premium_Defaulted ,predicted_response_LR0.04,percent = TRUE,
         col = "#4daf4a", lwd = 5,print.auc = TRUE, add= TRUE, print.auc.y = 40 )

plot.roc(Insurance_train$Premium_Defaulted ,predicted_train_LR0.05,percent = TRUE,
         col = "#D6604D", lwd = 5,print.auc = TRUE, add= TRUE, print.auc.y = 30 )

plot.roc(Insurance_train$Premium_Defaulted ,predicted_response_LR0.5,percent = TRUE,
         col = "#FF3399", lwd = 5,print.auc = TRUE, add= TRUE, print.auc.y = 20 )

legend("bottomright", legend = c("0.0291","0.0441","0.0525","0.5"),
       col = c("#377eb8","#4daf4a","#D6604D","#FF3399"), lwd = 5)

#Probability 0.0525 gives the highest AUC and chosen to be the final probability cutoff.

table(LR_train_predict>0.0525, Insurance_train$Premium_Defaulted)
predicted_response_LR0.05 <- factor(ifelse(LR_train_predict >= 0.0525, '1', '0'))
confusion_LR_0.05 <- confusionMatrix(predicted_response_LR0.05,Insurance_train$Premium_Defaulted,positive='1')
confusion_LR_0.05

LR_test_predict<-predict(LR_Model, Insurance_test[1:19], type="response")
predicted_responsetest_LR0.05 <- factor(ifelse(LR_test_predict >= 0.0525, '1', '0'))
confusion_LR_test <- confusionMatrix(predicted_responsetest_LR0.05, Insurance_test$Premium_Defaulted,positive='1')
confusion_LR_test

Insurance_test$PredictedResponseLR <- factor(ifelse(LR_test_predict >= 0.0525, '1', '0'))

write.csv(Insurance_test , "Insurance_Dataset_LRtest.xls")

predicted_test_LR <- as.numeric(ifelse(LR_test_predict >= 0.0525, '1', '0'))

plot.new()
roc(Insurance_test$Premium_Defaulted ,predicted_test_LR, plot = TRUE, legacy.axes = TRUE,
    percent = TRUE, xlab = "False positive percentage", ylab = "True positive percentage",
    col = "#377eb8" , lwd = 5, print.auc = TRUE)

plot.roc(Insurance_train$Premium_Defaulted ,predicted_train_LR0.05,percent = TRUE,
         col = "#D6604D", lwd = 5,print.auc = TRUE, add= TRUE, print.auc.y = 30 )


legend("bottomright", legend = c("Test","Train"),
       col = c("#377eb8","#D6604D"), lwd = 5)

