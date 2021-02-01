#------------------------------------------------------------------------------#
####################INSTACART MARKET BASKET ANALYSIS############################
#------------------------------------------------------------------------------#

##Step 1: IMPORT LIBRARIES
#-------------------------
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071", "tidyverse", "dplyr", "GGally", "data.table", "readr", "plyr", "sqldf")

##Step 2: LOAD THE CSV FILE
#--------------------------
insta_table_1 <- fread(file.choose(), showProgress = FALSE) #choose csv part 1
insta_table_2 <- fread(file.choose(), showProgress = FALSE) #choose csv part 2
insta_table_3 <- fread(file.choose(), showProgress = FALSE) #choose csv part 3
insta_table_4 <- fread(file.choose(), showProgress = FALSE) #choose csv part 4

insta_table <- rbind(insta_table_1, insta_table_2, insta_table_3, insta_table_4)

head(insta_table, 10)
class(insta_table)
str(insta_table)
nrow(insta_table_test)
glimpse(insta_table)

##Step 3: CLEAN THE DATA
#-----------------------
insta_table$user_id <- as.factor(insta_table$user_id)
insta_table$day_of_the_week <- as.factor(insta_table$day_of_the_week)
insta_table$order_id <- as.factor(insta_table$order_id)
insta_table$product_id <- as.factor(insta_table$product_id)
insta_table$order_hour_of_day <- as.factor(insta_table$order_hour_of_day)
insta_table$aisle_id <- as.factor(insta_table$aisle_id)
insta_table$department_id <- as.factor(insta_table$department_id)
insta_table$reordered <- as.factor(insta_table$reordered)

#insta_table <- insta_table %>% add_column(Prediction=0,.after= "reordered")
#insta_table$Prediction <- as.factor(insta_table$Prediction)

glimpse(insta_table)
map(insta_table, class)
colSums(is.na(insta_table))

##Step 4: SPLIT THE DATA
#-----------------------
insta_table_prior <- insta_table[insta_table$eval_set == 'prior',] #filter the table to only get the prior orders
insta_table_train <- insta_table[insta_table$eval_set == 'train',] #filter the table to only get the train orders

nrow(insta_table_prior)
nrow(insta_table_train)

##Step 5: FEATURE ENGINEERING AND TRANSFORM THE DATA TO A PRODUCT_CUSTOMER PAIR
#------------------------------------------------------------------------------
insta_table_prior <- (head(insta_table_prior, 100))
View(insta_table_prior_top_1000)
memory.limit()
memory.limit(size=56000)

user_product_prior_df <- sqldf("SELECT user_id, product_id, product_name, COUNT(*) as Freq
                          FROM insta_table_prior
                          GROUP BY user_id, product_id, product_name")

user_product_train_df <- sqldf("SELECT user_id, product_id, product_name, COUNT(*) as Freq
                          FROM insta_table_train
                          GROUP BY user_id, product_id, product_name")

joined_prior_train <- left_join(user_product_prior_df, user_product_train_df, by = c("user_id", "product_id"))

names(joined_prior_train)[names(joined_prior_train) == "Freq.x"] <- "prior_purchase_count" #number of times the product has already been purchased
joined_prior_train$Freq.y[is.na(joined_prior_train$Freq.y)] <- 0 #Replace NA's with a 0
names(joined_prior_train)[names(joined_prior_train) == "Freq.y"] <- "re_order" #number of times the product has already been purchased
final_df <- subset(joined_prior_train, select = -c(product_name.y))

final_df





#----------------------------------------------------------------------------#
#--------------------XG BOOST MODEL -----------------------------------------#
#----------------------------------------------------------------------------#

pacman::p_load("caret","ROCR","lift","xgboost") #Check, and if needed install the necessary packages

#use same training and testing data to ensure like-for-like comparison
#note - additional memory must be allocated to r before running this code

memory.limit(size=6000000)

training.XG <-model.matrix(reordered~., data = insta_table_prior)
testing.XG <-model.matrix(reordered~., data = insta_table_train)

model_XGboost<-xgboost(data = data.matrix(training.XG[,-1]), 
                       label = as.numeric(as.character(insta_table_prior$reordered)), 
                       eta = 0.1,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost,newdata=testing.XG[,-1], type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.22,1,0)),insta_table_prior$reordered,positive="1") #Display confusion matrix

####ROC Curve
XGboost_pred_testing <- prediction(XGboost_prediction, insta_table_prior$reordered) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_pred_testing,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_pred_testing,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(XGboost_prediction, insta_table_prior$XXXXXXXXXX, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


