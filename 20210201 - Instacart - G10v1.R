#------------------------------------------------------------------------------#
####################INSTACART MARKET BASKET ANALYSIS############################
#------------------------------------------------------------------------------#

##Step 1: IMPORT LIBRARIES
#-------------------------
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071", "tidyverse", "dplyr", "GGally", "data.table", "readr", "plyr", "sqldf")

memory.limit()
memory.limit(size=56000)

##Step 2: LOAD AND MERGE THE CSV FILES
#-------------------------------------
#step a): rbind the orders file
order_products_prior <- fread(file.choose(), showProgress = FALSE)
order_products_train <- fread(file.choose(), showProgress = FALSE) 

joined_order_products <- rbind(order_products_prior, order_products_train)

#Step b): left-join the orders to joined_order_products
order_csv <- fread(file.choose(), showProgress = FALSE)
expanded_orders <- right_join(order_csv, joined_order_products, by = c("order_id"))

nrow(expanded_orders)

#Step c): Add products, aisles, and departments
products_csv <- fread(file.choose(), showProgress = FALSE)
department_csv <- fread(file.choose(), showProgress = FALSE)
aisles_csv <- fread(file.choose(), showProgress = FALSE)

expanded_orders_v2 <- left_join(expanded_orders, products_csv, by = c("product_id"))
expanded_orders_v3 <- left_join(expanded_orders_v2, department_csv, by = c("department_id"))
insta_table <- left_join(expanded_orders_v3, aisles_csv, by = c("aisle_id"))

insta_table$days_since_prior_order[is.na(insta_table$days_since_prior_order)] <- 0 #Replace NA's with a 0

#names(insta_table)[names(insta_table) == "Freq.y"] <- "re_order" #number of times the product has already been purchased

str(insta_table)
colSums(is.na(insta_table))

head(insta_table, 10)
class(insta_table)
str(insta_table)
nrow(insta_table_test)
glimpse(insta_table)

##Step 3: TRANSFORM THE DATATYPES
#-------------------------------
insta_table$user_id <- as.factor(insta_table$user_id)
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

##Step 4: SPLIT THE DATA
#-----------------------
insta_table_prior <- insta_table[insta_table$eval_set == 'prior',] #filter the table to only get the prior orders
insta_table_train <- insta_table[insta_table$eval_set == 'train',] #filter the table to only get the train orders

nrow(insta_table_prior)
nrow(insta_table_train)

##Step 5: FEATURE ENGINEERING AND TRANSFORM THE DATA TO A PRODUCT_CUSTOMER PAIR
#------------------------------------------------------------------------------

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
final_df$re_order <- as.factor(final_df$re_order) 

str(final_df)

final_df

##Step 6: CLASSIFICATION MODEL 1: XGBOOST
#----------------------------------------

