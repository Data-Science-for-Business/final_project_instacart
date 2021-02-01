#------------------------------------------------------------------------------#
####################INSTACART MARKET BASKET ANALYSIS############################
#------------------------------------------------------------------------------#

##Step 1: IMPORT LIBRARIES
#-------------------------
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071", "tidyverse", "dplyr", "GGally", "data.table", "readr")

##Step 2: LOAD THE CSV FILE
#--------------------------
insta_table <- fread(file.choose(), showProgress = FALSE)
head(insta_table, 10)
class(insta_table)
str(insta_table)
nrow(insta_table)

##Step 3: CLEAN THE DATA
#-----------------------
insta_table$user_id <- as.factor(insta_table$user_id)
insta_table$day_of_the_week <- as.factor(insta_table$day_of_the_week)
