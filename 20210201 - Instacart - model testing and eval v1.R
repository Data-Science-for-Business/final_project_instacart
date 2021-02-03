
#------------------------------------------------------------------------------|
##Step 7: RUNNING AND TESTING MODELS
#------------------------------------------------------------------------------|

#----------------------------------------------------------------------------#
#--------------------XG BOOST MODEL -----------------------------------------#
#----------------------------------------------------------------------------#


training.XG <-model.matrix(re_order~., data = TRAIN_USERS_vF)
testing.XG <-model.matrix(re_order~., data = TEST_USERS_vF)

#colSums(is.na(training.XG))#just double checking
#colSums(is.na(testing.XG))

model_XGboost<-xgboost(data = data.matrix(training.XG[,-1]), 
                       label = as.numeric(as.character(TRAIN_USERS_vF$re_order)), 
                       eta = 0.1,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic")

XGboost_prediction<-predict(model_XGboost, newdata=testing.XG[,-1], type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.2,1,0)),
                TEST_USERS_vF$re_order,
                positive="1") #Display confusion matrix


####ROC Curve
XGboost_pred_testing <- prediction(XGboost_prediction, TEST_USERS_vF$re_order) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_pred_testing,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_pred_testing,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
#code not done: plotLift(XGboost_prediction, insta_table_prior$XXXXXXXXXX, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
