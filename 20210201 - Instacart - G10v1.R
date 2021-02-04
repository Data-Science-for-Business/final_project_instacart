#------------------------------------------------------------------------------#
####################INSTACART MARKET BASKET ANALYSIS############################
#------------------------------------------------------------------------------#

#-------------------------|
##Step 1: IMPORT LIBRARIES|
#-------------------------|

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071", "tidyverse", "dplyr", "GGally", "data.table", "readr", "sqldf", "data.table")

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
insta_table_prior_trainers_only

nrow(insta_table_prior_trainers_only)
head(insta_table_prior_trainers_only)

#------------------------------------------------------------------------------|
##Step 5:  NEW USER_ID&PRODUCT_ID FEATURES                                     |
#------------------------------------------------------------------------------|

#-------------------------------------------
#####Feature #1: Relative purchase frequency
COPY_insta_table_prior_trainers_only <- insta_table_prior_trainers_only
COPY_insta_table_prior_trainers_only_U1 <- insta_table_prior_trainers_only[insta_table_prior_trainers_only$user_id < 5501,] ### !! ADJUST THIS CAP BASED ON # OF TRAIN AND TEST USERS TO CREATE ###

COPY_insta_table_prior_trainers_only_U1$user_id <- as.factor(COPY_insta_table_prior_trainers_only_U1$user_id) 

str(COPY_insta_table_prior_trainers_only_U1)
COPY_insta_table_prior_trainers_only_U1
detach(package:plyr)

#a)Determine how many purchases the user has made in total
rec1_summary <- COPY_insta_table_prior_trainers_only_U1 %>%
  group_by(user_id) %>%
  summarise(total_of_orders = max(order_number))

#b)Determine how many purchases the user has made on a product level
rec1_users_product_summary <- COPY_insta_table_prior_trainers_only_U1 %>% count(user_id, product_id, sort = TRUE)
names(rec1_users_product_summary)[names(rec1_users_product_summary) == "n"] <- "tot_user_prod_orders"

#create combined table (a+b)
df_users_summary <- left_join(rec1_users_product_summary, rec1_summary, by = c("user_id"))
df_users_summary <- df_users_summary[, c("user_id", "total_of_orders", "product_id", "tot_user_prod_orders")]

#substract 3 from the total number of orders / user (to later filter out orders)
df_users_summary$total_of_orders_THRESHOLD <- df_users_summary$total_of_orders -2

df_users_summary  ##### <---USE THIS ONE TO CREATE RELATIVE PURCHASE FREQUENCY
COPY2_df_users_summary <- df_users_summary

COPY2_df_users_summary$RPF <- COPY2_df_users_summary$tot_user_prod_orders / COPY2_df_users_summary$total_of_orders 

#Drop the THRESHOLD column
COPY2_df_users_summary1 <- subset(COPY2_df_users_summary, select = -c(total_of_orders_THRESHOLD ))

COPY2_df_users_summary1
#-----------------------------------
#####Feature #2: Percentage re_order
COPY2_df_users_summary1$PER_REORDER <- as.numeric((COPY2_df_users_summary1$tot_user_prod_orders - 1) / COPY2_df_users_summary1$tot_user_prod_orders) 

COPY2_df_users_summary1
#-----------------------------------------------------------------------------------------------------
#####Feature #3: RECENCY ==> Did the user purchase the product in its latest 3 orders (prior to train)
#bring back TRIMMED user_id,product_id level table
COPY_insta_table_prior_trainers_only_U1_trimmed <- COPY_insta_table_prior_trainers_only_U1[, c("user_id", "order_id", "order_number", "product_id")]

#join the 2 tables
df_user_product_enriched <- left_join(df_users_summary,COPY_insta_table_prior_trainers_only_U1_trimmed, by = c("user_id", "product_id") )

#keep only last three orders
df_user_product_enriched_ONLY_LAST_THREE_ORDERS <- df_user_product_enriched  %>%   filter(order_number >= total_of_orders_THRESHOLD )
user_product_recency_count <- df_user_product_enriched_ONLY_LAST_THREE_ORDERS %>% count(user_id, product_id, sort = TRUE)
user_product_recency_count$recency_flag <- ifelse(user_product_recency_count$n > 2, 1, 0)
names(user_product_recency_count)[names(user_product_recency_count) == "n"] <- "Count_last_three_orders"

user_product_recency_count

#-----------------------------------------------------|
#combine output feature 2 with newly created feature 3|
#-----------------------------------------------------|

feature_output <- left_join(COPY2_df_users_summary1, user_product_recency_count, by = c("user_id", "product_id")) 
feature_output$Count_last_three_orders[is.na(feature_output$Count_last_three_orders)] <- 0 #Replace NA's as a result of left_join with a 0
feature_output$recency_flag[is.na(feature_output$recency_flag)] <- 0                       #Replace NA's as a result of left_join with a 0
feature_output$recency_flag <- as.factor(feature_output$recency_flag)

str(feature_output)
feature_output_v1 <- feature_output

#-----------------------------------------------------|
#       GET INFORMATION FROM THE TRAIN ORDER :)       \
#-----------------------------------------------------|

COPY_feature_output <- feature_output 
COPY_order_csv <- order_csv

COPY_order_csv_TRAIN_ONLY <- COPY_order_csv[COPY_order_csv$eval_set == 'train',]
COPY_order_csv_TRAIN_ONLY_TRIMMED <- COPY_order_csv_TRAIN_ONLY[, c("user_id", "order_dow", "order_hour_of_day", "days_since_prior_order")]
COPY_order_csv_TRAIN_ONLY_TRIMMED

names(COPY_order_csv_TRAIN_ONLY_TRIMMED)[names(COPY_order_csv_TRAIN_ONLY_TRIMMED) == "order_dow"] <- "TRAIN_order_dow" 
names(COPY_order_csv_TRAIN_ONLY_TRIMMED)[names(COPY_order_csv_TRAIN_ONLY_TRIMMED) == "order_hour_of_day"] <- "TRAIN_order_hour_of_day" 
names(COPY_order_csv_TRAIN_ONLY_TRIMMED)[names(COPY_order_csv_TRAIN_ONLY_TRIMMED) == "days_since_prior_order"] <- "TRAIN_days_since_prior_order" 

glimpse(COPY_order_csv_TRAIN_ONLY_TRIMMED) #DATASET USED TO ENRICH PREVIOUS ORDER WITH CURRENT TRAIN ORDER INFORMATION :)

#----------------------------------------------------------------------------------|
#####Feature #4: Time delta normal purchase pattern vs. time-diff with train order |

#bring back TRIMMED user_id,product_id level table
COPY_insta_table_prior_trainers_only_U1_trimmed_11 <- COPY_insta_table_prior_trainers_only_U1[, c("user_id", "order_id", "order_number", "product_id", "days_since_prior_order")]

#remove the user's first order so that it isnt included in average calculation
COPY_insta_table_prior_trainers_only_U1_trimmed_11_EXCL_FIRST_ORDER <- COPY_insta_table_prior_trainers_only_U1_trimmed_11  %>%   filter(order_number > 1)

#Remove products that have only been purchased once
feature_output_vextra <- feature_output[, c("user_id", "product_id", "tot_user_prod_orders")]
COPY_insta_table_prior_trainers_only_U1_trimmed_11_EXCL_FIRST_PRODUCT_ORDER <- left_join(COPY_insta_table_prior_trainers_only_U1_trimmed_11_EXCL_FIRST_ORDER, feature_output_vextra, by = c("user_id", "product_id") )
COPY_insta_table_prior_trainers_only_U1_trimmed_11_EXCL_FIRST_PRODUCT_ORDER_v2 <- COPY_insta_table_prior_trainers_only_U1_trimmed_11_EXCL_FIRST_PRODUCT_ORDER  %>%   filter(tot_user_prod_orders > 1)

#Calculate average time difference between orders at user_product level
user_product_avg_time_order <- COPY_insta_table_prior_trainers_only_U1_trimmed_11_EXCL_FIRST_PRODUCT_ORDER_v2 %>%
  group_by(user_id, product_id) %>%
  summarise(days_since_prior_order_average = mean(days_since_prior_order))

head(user_product_avg_time_order)

#Bringing it all together
feature_output_v2 <- left_join(feature_output_v1, user_product_avg_time_order, by = c("user_id", "product_id"))
COPY_order_csv_TRAIN_ONLY_TRIMMED$user_id <- as.factor(COPY_order_csv_TRAIN_ONLY_TRIMMED$user_id)
feature_output_v3 <- left_join(feature_output_v2, COPY_order_csv_TRAIN_ONLY_TRIMMED, by = c("user_id"))
feature_output_v3$days_since_prior_order_average[is.na(feature_output_v3$days_since_prior_order_average)] <- 0 #Replace NA's with a 0 ############NOT SURE IF THIS IS CORRECT.... :/
feature_output_v3$Scaled_Delta_FTWR <- abs(feature_output_v3$days_since_prior_order_average - feature_output_v3$TRAIN_days_since_prior_order) 

feature_output_v3

#-------------------------------------------------------------------------------------|
#####Feature #5: Delta from to be expected time during the day that product is bought |

#Group by product_id and get the mean and standard deviation purchase 'order hour of day'
insta_table_prior_trainers_only
str(insta_table_prior_trainers_only)

product_order_hour_summary <- insta_table_prior_trainers_only %>%
  group_by(product_id) %>%
  summarise(avg_order_hour_day = mean(order_hour_of_day), 
            sd_order_hour_day  = sd(order_hour_of_day)
  )

product_order_hour_summary2 <-product_order_hour_summary[order(product_order_hour_summary$sd_order_hour_day),]
View(head(product_order_hour_summary2, 1000)) ##Some orders are indeed purchased during very narrow time windows

#Create a flag if the app is opened at an 'order_hour_day' which is INSIDE the product specific avg 'order_hour_day' confidence interval
feature_output_v4 <- feature_output_v3

feature_output_v5 <- left_join(feature_output_v4, product_order_hour_summary2, by = c("product_id"))
feature_output_v5$Hour_lower_bound <- feature_output_v5$avg_order_hour_day - (1.645* feature_output_v5$sd_order_hour_day) 
feature_output_v5$Hour_upper_bound <- feature_output_v5$avg_order_hour_day + (1.645* feature_output_v5$sd_order_hour_day) 

feature_output_v5$normal_purchase_time <- ifelse(feature_output_v5$TRAIN_order_hour_of_day > feature_output_v5$Hour_lower_bound & feature_output_v5$TRAIN_order_hour_of_day < feature_output_v5$Hour_upper_bound, 1, 0)
feature_output_v5[order(feature_output_v5$user_id),]

feature_output_v5$sd_order_hour_day[is.na(feature_output_v5$sd_order_hour_day)] <- 0
feature_output_v5$Hour_lower_bound[is.na(feature_output_v5$Hour_lower_bound)] <- 0
feature_output_v5$Hour_upper_bound[is.na(feature_output_v5$Hour_upper_bound)] <- 0
feature_output_v5$normal_purchase_time[is.na(feature_output_v5$normal_purchase_time)] <- 0

feature_output_v5
colSums(is.na(feature_output_v5))
str(feature_output_v5)

#-------------------------------------------------------------------------------------|
#####Final step data engineering: Only select columns you actually need               |

feature_output_vF <- feature_output_v5
glimpse(feature_output_v5)

#keep only the variables we will put in the model
feature_output_vF <- feature_output_v5[, c("user_id", "product_id", "total_of_orders", "tot_user_prod_orders", "RPF", "PER_REORDER", "recency_flag", "Scaled_Delta_FTWR", "normal_purchase_time")]

#add back the aisles, department
products_csv33 <- products_csv
products_csv33$product_id <- as.factor(products_csv33$product_id) 
products_csv33$aisle_id <- as.factor(products_csv33$aisle_id)
products_csv33$department_id <- as.factor(products_csv33$department_id)
products_csv33 <- subset(products_csv33, select = -c(product_name))

products_csv33

#Final feature output
feature_output_vFF <- left_join(feature_output_vF, products_csv33, by = c("product_id"))

feature_output_vFF_sorted <- feature_output_vFF[order(user_id)]
View(feature_output_vFF_sorted)

#check the classes of the variables
str(feature_output_vFF_sorted)
feature_output_vFF_sorted$normal_purchase_time <- as.factor(feature_output_vFF_sorted$normal_purchase_time)

#-------------------------------------------------------------------------------------|
#####CLUSTERING OUTPUT: Add User_Id Clusters                                          |

##NOTE! Need to have run Kevin's Clustering.R script to get the variables in

user_id_k_means_df_vF

#Join the user_id clusters
nrow(user_id_k_means_df_vF) #Check if the total unique number of user_ids matches
colSums(is.na(user_id_k_means_df_vF)) #check if there are no missing values in the clustering file
feature_output_vFF_w_cluster1 <- left_join(feature_output_vFF_sorted, user_id_k_means_df_vF, by = c("user_id"))

#Join the product_id clusters
nrow(product_id_k_means_df_vF) #Check if the total unique number of user_ids matches
colSums(is.na(product_id_k_means_df_vF)) #check if there are no missing values in the clustering file
feature_output_vFF_w_cluster2 <- left_join(feature_output_vFF_w_cluster1, product_id_k_means_df_vF, by = c("product_id"))

#Final check bringing in the two clusters
nrow(feature_output_vFF_w_cluster2) #Check if the total unique number of user_ids matches
colSums(is.na(feature_output_vFF_w_cluster2)) #check if there are no missing values in the clustering file

#Clean NA's
feature_output_vFF_w_cluster2$Product_Cluster_Membership_k[is.na(feature_output_vFF_w_cluster2$Product_Cluster_Membership_k)] <- "Other"
feature_output_vFF_w_cluster2$Product_Cluster_Membership_k <- as.factor(feature_output_vFF_w_cluster2$Product_Cluster_Membership_k)

#Final output of clustering join
feature_output_vFF_w_cluster_VF <- feature_output_vFF_w_cluster2
nrow(feature_output_vFF_w_cluster_VF) #Check if the total unique number of user_ids matches
colSums(is.na(feature_output_vFF_w_cluster_VF)) #check if there are no missing values in the clustering file

str(feature_output_vFF_w_cluster_VF)
head(feature_output_vFF_w_cluster_VF)

#------------------------------------------------------------------------------|
##Step 6: CoNNECT THE REORDER EVENT TO THE FEATURE OUTPUT TABLE                |
#------------------------------------------------------------------------------|

user_product_prior_df <- sqldf("SELECT user_id, product_id
                          FROM insta_table_prior
                          GROUP BY user_id, product_id")


user_product_train_df <- sqldf("SELECT user_id, product_id, COUNT(*) as re_order
                          FROM insta_table_train
                          GROUP BY user_id, product_id, product_name")

str(user_product_prior_df)
str(feature_output_vFF_w_cluster_VF)
str(user_product_train_df)

#create a copy of user_id, which is an integer to the core df
user_product_prior_df$user_id_copy <- user_product_prior_df$user_id
user_product_prior_df$user_id <- as.factor(user_product_prior_df$user_id)

user_product_prior_df_VF <- user_product_prior_df
str(user_product_prior_df_VF)

#join this copy to the master feature_ouptut sheet
joined_feature_output_train_reorder <- left_join(feature_output_vFF_w_cluster_VF, user_product_prior_df_VF, by = c("user_id", "product_id"))
str(joined_feature_output_train_reorder)

#Join the train order events to the master feature_output sheet
user_product_train_df$user_id <- as.factor(user_product_train_df$user_id)
joined_feature_output_train_reorder_v2 <- left_join(joined_feature_output_train_reorder, user_product_train_df, by = c("user_id", "product_id") )
str(joined_feature_output_train_reorder_v2)
joined_feature_output_train_reorder_v2$re_order[is.na(joined_feature_output_train_reorder_v2$re_order)] <- 0
str(joined_feature_output_train_reorder_v2)
joined_feature_output_train_reorder_v2$re_order <- as.factor(joined_feature_output_train_reorder_v2$re_order) 

joined_feature_output_train_reorder_vF <- joined_feature_output_train_reorder_v2

joined_feature_output_train_reorder_vF ### THIS IS THE MASTER FILE TO BE INPUTED IN CLASSIFICATION MODELS
View(joined_feature_output_train_reorder_vF)

colSums(is.na(joined_feature_output_train_reorder_vF))


#-------------------------------------------------|
#----------function to remove rare levels---------|

combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }

#------------------------------------------------------------------------------|
##Step 6: BRING IN ENGINEERED FEEATURES
#------------------------------------------------------------------------------|

pacman::p_load("caret","ROCR","lift","xgboost") #Check, and if needed install the necessary packages

#Decrease the number of users loaded in the XGBOOST Model
TRAIN_USERS <- joined_feature_output_train_reorder_vF[joined_feature_output_train_reorder_vF$user_id_copy < 5001,]
TEST_USERS <- joined_feature_output_train_reorder_vF  %>%   filter(user_id_copy >5000 &  user_id_copy < 5500)

#remove user_id copy
TRAIN_USERS <- subset(TRAIN_USERS, select = -c(user_id_copy))
TEST_USERS <- subset(TEST_USERS, select = -c(user_id_copy))

#Remove product_name.x column from final_df. Product_id is already part of df as factor
TRAIN_USERS_v3 <- subset(TRAIN_USERS, select = -c(product_id))
TRAIN_USERS_v4 <- subset(TRAIN_USERS_v3, select = -c(user_id))

TRAIN_USERS_vF <- TRAIN_USERS_v4
head(TRAIN_USERS_vF,40)
str(TRAIN_USERS_vF)

#Remove product_name.x column from final_df. Product_id is already part of df as factor
TEST_USERS_v3 <- subset(TEST_USERS, select = -c(product_id))
TEST_USERS_v4 <- subset(TEST_USERS_v3, select = -c(user_id))

TEST_USERS_vF <- TEST_USERS_v4
head(TEST_USERS_vF, 1000)
str(TEST_USERS_vF)

combinerarecategories(TRAIN_USERS_vF, 5)
combinerarecategories(TEST_USERS_vF, 5)

#------------------------------------------------------------------------------|
##END OF FEATURE ENGINEERING FILE
#------------------------------------------------------------------------------|

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
