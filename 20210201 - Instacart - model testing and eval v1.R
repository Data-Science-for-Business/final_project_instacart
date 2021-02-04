#------------------------------------------------------------------------------|
##Step 7: RUNNING AND TESTING MODELS
#------------------------------------------------------------------------------|

#----------------------------------------------------------------------------#
#--------------------XG BOOST MODEL -----------------------------------------#
#----------------------------------------------------------------------------#
pacman::p_load("caret","partykit","ROCR","lift","rpart","e1071", "base")

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

XGboost_prediction<-predict(model_XGboost, newdata=testing.XG[,-1], type="probability") #Predict classification (for confusion matrix)

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

#importance of each feature to model
importanceTable <- xgb.importance(colnames(training.XG), model_XGboost)
head(importanceTable)


#put probabilities in with customer IDs and product IDs (use TEST_USERS becuase that is last DF that has IDs still)
prediction_df <- cbind(TEST_USERS, XGboost_prediction)
prediction_df$user_id <- as.integer(prediction_df$user_id)
prediction_df$product_id <- as.integer(prediction_df$product_id)



#sort by user ID in ascending order, then by prediction probability in descending order
prediction_sorted <- prediction_df[with(prediction_df, order(prediction_df$user_id , -prediction_df$XGboost_prediction))]
View(prediction_sorted)

#output top X products for user Y
sample_user <- 3201
number_recs <- 5
user_output <- prediction_sorted[prediction_sorted$user_id == sample_user]
for(i in 1:number_recs){
  print(user_output[i]$product_id)
  
}




#-----------------------------------------------------------------------
#------------------------CART Model-------------------------------------
#-----------------------------------------------------------------------


CART_cp = rpart.control(cp = 0.0001) #set cp to a small number to "grow" a large tree

rpart_tree<-rpart(re_order~.,data=TRAIN_USERS_vF, method="class", control=CART_cp) #"Grow" a tree on training data

prunned_rpart_tree<-prune(rpart_tree, cp=0.001) #Prune the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

# Understand the relationship between the cross-validated error, size of the tree and cp.
plotcp(rpart_tree) # Use printcp(rpart_tree) to print the values. As a rule of thumb pick up the largest cp which does not give a substantial drop in error

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=TEST_USERS_vF, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class,TEST_USERS_vF$re_order,positive = "1") #Display confusion matrix

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=TEST_USERS_vF,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], TEST_USERS_vF$re_order) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class,  TEST_USERS_vF$re_order, cumulative = TRUE, n.buckets = 10) # Plot Lift chart




