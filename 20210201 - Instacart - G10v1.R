#------------------------------------------------------------------------------#
####################INSTACART MARKET BASKET ANALYSIS############################
#------------------------------------------------------------------------------#

#-------------------------|
##Step 1: IMPORT LIBRARIES|
#-------------------------|

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071", "tidyverse", "dplyr", "GGally", "data.table", "readr", "plyr", "sqldf")

memory.limit()
memory.limit(size=56000)

#-------------------------------------|
##Step 2: LOAD AND MERGE THE CSV FILES|
#-------------------------------------|
#step a): rbind the orders file
order_products_prior <- fread(file.choose(), showProgress = FALSE)
order_products_train <- fread(file.choose(), showProgress = FALSE) 

joined_order_products <- rbind(order_products_prior, order_products_train)

#Step b): left-join the orders to joined_order_products
order_csv <- fread(file.choose(), showProgress = FALSE)
expanded_orders <- right_join(order_csv, joined_order_products, by = c("order_id")) ##---> here we lose the 75k test orders

nrow(expanded_orders)

#Step c): Add products, aisles, and departments
products_csv <- fread(file.choose(), showProgress = FALSE)
department_csv <- fread(file.choose(), showProgress = FALSE)
aisles_csv <- fread(file.choose(), showProgress = FALSE)

expanded_orders_v2 <- left_join(expanded_orders, products_csv, by = c("product_id"))
expanded_orders_v3 <- left_join(expanded_orders_v2, department_csv, by = c("department_id"))
insta_table <- left_join(expanded_orders_v3, aisles_csv, by = c("aisle_id"))

insta_table$days_since_prior_order[is.na(insta_table$days_since_prior_order)] <- 0 #Replace NA's with a 0

str(insta_table) #this is the core combined table including all the records from Kaggle
colSums(is.na(insta_table)) #check if there are still NA's

head(insta_table, 10)
class(insta_table)
str(insta_table)
nrow(insta_table_test)
glimpse(insta_table)

#--------------------------------|
##Step 3: TRANSFORM THE DATATYPES|
#--------------------------------|
#insta_table$user_id <- as.factor(insta_table$user_id)
insta_table$day_of_the_week <- as.factor(insta_table$order_dow)
insta_table$order_id <- as.factor(insta_table$order_id)
insta_table$product_id <- as.factor(insta_table$product_id)
insta_table$order_hour_of_day <- as.factor(insta_table$order_hour_of_day)
insta_table$days_since_prior_order <- as.factor(insta_table$days_since_prior_order) ## NOTE: DO NOT TAKE FIRST ORDER INTO ACCOUNT WHEN CALCULATING MEAN AT LATER STAGE BC ITS ZERO
insta_table$aisle_id <- as.factor(insta_table$aisle_id)
insta_table$department_id <- as.factor(insta_table$department_id)
insta_table$reordered <- as.factor(insta_table$reordered)

glimpse(insta_table)
map(insta_table, class)
colSums(is.na(insta_table))

#-----------------------|
##Step 4: SPLIT THE DATA|
#-----------------------|
insta_table_prior <- insta_table[insta_table$eval_set == 'prior',] #filter the table to only get the prior orders
insta_table_train <- insta_table[insta_table$eval_set == 'train',] #filter the table to only get the train orders

nrow(insta_table_prior)

#->Create list of 'train' and 'test' user_ids
train_user_ids <- sqldf("SELECT DISTINCT user_id
                          FROM insta_table_train")


nrow(train_user_ids)
str(train_user_ids) #-> this is a unique list of user_ids that are part of the training set

#->Select only the insta_table_prior records that belong to eventual train user_ids
insta_table_prior_trainers_only <- right_join(insta_table_prior, train_user_ids, by = c("user_id")) #--> this join works as a filter, removing all prior orders from test users

#------------------------------------------------------------------------------|
##Step 5: FEATURE ENGINEERING AND TRANSFORM THE DATA TO A PRODUCT_CUSTOMER PAIR|
#------------------------------------------------------------------------------|

user_product_prior_trainer_only_df <- sqldf("SELECT user_id, product_id, product_name, COUNT(*) as Freq
                                            FROM insta_table_prior_trainers_only
                                            GROUP BY user_id, product_id, product_name")

user_product_train_df <- sqldf("SELECT user_id, product_id, product_name, COUNT(*) as Freq
                          FROM insta_table_train
                          GROUP BY user_id, product_id, product_name")

joined_prior_train <- left_join(user_product_prior_trainer_only_df, user_product_train_df, by = c("user_id", "product_id"))

names(joined_prior_train)[names(joined_prior_train) == "Freq.x"] <- "prior_purchase_count" #number of times the product has already been purchased
joined_prior_train$Freq.y[is.na(joined_prior_train$Freq.y)] <- 0 #Replace NA's with a 0
names(joined_prior_train)[names(joined_prior_train) == "Freq.y"] <- "re_order" #number of times the product has already been purchased
final_df <- subset(joined_prior_train, select = -c(product_name.y))
final_df$re_order <- as.factor(final_df$re_order) 

str(final_df)

final_df
gc()

products_csv2 <- products_csv
products_csv2$product_id <- as.factor(products_csv2$product_id) 
products_csv2$aisle_id <- as.factor(products_csv2$aisle_id)
products_csv2$department_id <- as.factor(products_csv2$department_id)


final_df_aisles <- left_join(final_df, products_csv2, by = c("product_id"))
head(final_df_aisles)
str(final_df_aisles)

#-------------------------------------------------
#----------function to remove rare levels--------

combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }



##Step 6: CLASSIFICATION MODEL 1: XGBOOST
#----------------------------------------


#----------------------------------------------------------------------------#
#--------------------XG BOOST MODEL -----------------------------------------#
#----------------------------------------------------------------------------#

pacman::p_load("caret","ROCR","lift","xgboost") #Check, and if needed install the necessary packages

#Decrease the number of users loaded in the XGBOOST Model
TRAIN_USERS <- final_df_aisles[final_df_aisles$user_id < 5001,]
TEST_USERS <- final_df_aisles  %>%   filter(user_id >5000 &  user_id < 5500)

#Remove product_name.x column from final_df. Product_id is already part of df as factor
TRAIN_USERS_v2 <- subset(TRAIN_USERS, select = -c(product_name.x))
TRAIN_USERS_v3 <- subset(TRAIN_USERS_v2, select = -c(product_id))
TRAIN_USERS_v4 <- subset(TRAIN_USERS_v3, select = -c(user_id))
TRAIN_USERS_v5 <- subset(TRAIN_USERS_v4, select = -c(product_name))

TRAIN_USERS_vF <- TRAIN_USERS_v5[, c(4,3,1,2)]
head(TRAIN_USERS_vF)
str(TRAIN_USERS_vF)

#Remove product_name.x column from final_df. Product_id is already part of df as factor
TEST_USERS_v2 <- subset(TEST_USERS, select = -c(product_name.x))
TEST_USERS_v3 <- subset(TEST_USERS_v2, select = -c(product_id))
TEST_USERS_v4 <- subset(TEST_USERS_v3, select = -c(user_id))
TEST_USERS_v5 <- subset(TEST_USERS_v4, select = -c(product_name))

TEST_USERS_vF <- TEST_USERS_v5[, c(4,3,1,2)]
head(TEST_USERS_vF, 1000)
str(TEST_USERS_vF)

combinerarecategories(TRAIN_USERS_vF, 5)
combinerarecategories(TEST_USERS_vF, 5)


training.XG <-model.matrix(re_order~., data = TRAIN_USERS_vF) ### DO NOT RUN THIS LINE OF CODE, IT WILL BREAK YOUR LAPTOP! :(
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
plotLift(XGboost_prediction, insta_table_prior$XXXXXXXXXX, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


