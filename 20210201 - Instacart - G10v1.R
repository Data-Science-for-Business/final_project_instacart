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

glimpse(insta_table)
map(insta_table, class)
colSums(is.na(insta_table))

##Step 4: SPLIT THE DATA
#-----------------------
insta_table_prior <- insta_table[insta_table$eval_set == 'prior',] #filter the table to only get the prior orders
insta_table_train <- insta_table[insta_table$eval_set == 'train',] #filter the table to only get the train orders

nrow(insta_table_prior)
nrow(insta_table_train)

##Step 5: FEATURE ENGINEERING
#----------------------------
insta_table_prior_top_1000 <- (head(insta_table_prior, 100))
View(insta_table_prior_top_1000)

sqldf("SELECT user_id, product_id, product_name, COUNT(*) as Freq
       FROM insta_table_prior_top_1000
       GROUP BY user_id, product_id, product_name")


##Step 6: TRANSFORM THE DATA TO A PRODUCT_CUSTOMER PAIR
#------------------------------------------------------









