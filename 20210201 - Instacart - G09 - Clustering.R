#clustering

#Step 1: create data frame for clustering by user ID

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

#Function Mode
Mode2 <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

orders <- fread(file.path("C:/Users/Kevin/Documents/GitHub/final_project_instacart/Data", "orders.csv"))
#orders <- fread(file.choose(), showProgress = FALSE)

orders$user_id <- as.factor(orders$user_id)
orders$eval_set <- as.factor(orders$eval_set)

df_users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  #mutate(last = dplyr::last(days_since_prior_order)) %>%
  summarise(
    number_of_orders = max(order_number),
    # number_of_item_in_basket = .... need to add more data we do not have it with only orders
    order_hour_of_day = mean(order_hour_of_day),
    order_dow = Mode2(order_dow),
    days_since_prior_order_average = (sum(days_since_prior_order, na.rm = T))/(max(order_number)-1)
    #days_since_prior_order_current = last #I have issue with mutate and the grouping, to be solve later or added directly in the featured engineered data
  )

#head(df_users)
#View(df_users)
nrow(df_users)
str(df_users)

#need to reduce the number of ID for HC cluster.
#df_users2 <- df_users %>%
#  slice(1:15000)

#---------------------------------------------------------------
#Step 2: create segments, preparation


# Run below only once, then comment out
# New versions of the networkD3 package may not work properly, so install the following version
packageurl <- "https://cran.r-project.org/src/contrib/Archive/networkD3/networkD3_0.2.13.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

MIN_VALUE = 0.5
max_data_report = 10

segmentation_attributes_used = c(2,3,4,5) 
profile_attributes_used = c(2:5) 
numb_clusters_used = 40 #need to figure out a way to define the number of clusters
profile_with = "hclust"
distance_used = "euclidean"
hclust_method = "ward.D"
kmeans_method = "Lloyd"

ProjectData <- data.matrix(df_users) 
str(ProjectData )

segmentation_attributes_used <- intersect(segmentation_attributes_used, 1:ncol(ProjectData))
profile_attributes_used <- intersect(profile_attributes_used, 1:ncol(ProjectData))

ProjectData_segment <- ProjectData[,segmentation_attributes_used]
ProjectData_profile <- ProjectData[,profile_attributes_used]

ProjectData_scaled <- apply(ProjectData, 2, function(r) if (sd(r)!=0) (r-mean(r))/sd(r) else 0*r)


#------------------------------------------------------------------------------------------------------
#kmean
kmeans_clusters <- kmeans(ProjectData_segment,centers= numb_clusters_used, iter.max=2000, algorithm=kmeans_method)

ProjectData_with_kmeans_membership <- cbind(1:length(kmeans_clusters$cluster),kmeans_clusters$cluster)
colnames(ProjectData_with_kmeans_membership)<-c("Observation Number_k","Cluster_Membership_k")

head(ProjectData_with_kmeans_membership)
write.csv(round(ProjectData_with_kmeans_membership, 2), file = "ProjectData_with_kmeans_membership.csv")


#------------------------------------------------------------------------------------------------------
#hccluster

euclidean_pairwise <- as.matrix(dist(head(ProjectData_segment, max_data_report), method="euclidean"))
euclidean_pairwise <- euclidean_pairwise*lower.tri(euclidean_pairwise) + euclidean_pairwise*diag(euclidean_pairwise) + 10e10*upper.tri(euclidean_pairwise)
euclidean_pairwise[euclidean_pairwise==10e10] <- NA
rownames(euclidean_pairwise) <- colnames(euclidean_pairwise) <- sprintf("Obs.%02d", 1:max_data_report)

Pairwise_Distances <- dist(ProjectData_segment, method = distance_used) 
Hierarchical_Cluster_distances <- dist(ProjectData_segment, method=distance_used)
Hierarchical_Cluster <- hclust(Hierarchical_Cluster_distances, method=hclust_method)

num <- nrow(ProjectData) - 1
df1 <- cbind(as.data.frame(Hierarchical_Cluster$height[length(Hierarchical_Cluster$height):1]), c(1:num))
colnames(df1) <- c("distances","index")

cluster_memberships_hclust <- as.vector(cutree(Hierarchical_Cluster, k=numb_clusters_used)) # cut tree into as many clusters as numb_clusters_used
cluster_ids_hclust=unique(cluster_memberships_hclust)

ProjectData_with_hclust_membership <- cbind(1:length(cluster_memberships_hclust),cluster_memberships_hclust)
colnames(ProjectData_with_hclust_membership)<-c("user_id","Cluster_Membership")

write.csv(round(ProjectData_with_hclust_membership, 2), file = "ProjectData_with_hclust_membership.csv")


#---------------------------------
#clusturing on products

order_product_prior <- fread(file.path("C:/Users/Kevin/Documents/GitHub/final_project_instacart/Data", "order_products__prior.csv"))
#orders <- fread(file.choose(), showProgress = FALSE)

order_product_prior$product_id <- as.factor(order_product_prior$product_id)
order_product_prior$order_id <- as.factor(order_product_prior$order_id)
#str(order_product_prior)

df_products <- order_product_prior %>%
  group_by(product_id) %>%
  summarise(
    number_of_orders = n(),
    percentage_reordered =  (sum(reordered, na.rm = T)/n()),
    when_added_to_cart_average = mean(add_to_cart_order),
    count_added_to_cart_first = sum(add_to_cart_order==1),
    percentage_added_to_cart_first = count_added_to_cart_first/number_of_orders
    #count_added_to_cart_first = sum(order_product_prior[order_product_prior$add_to_cart_order == '1'], na.rm=T) 
  )

head(df_products)
View(df_products)

segmentation_attributes_used_product = c(2,3,4,5,6) 
profile_attributes_used_product = c(2:6) 
numb_clusters_used = 100

ProjectData_product <- data.matrix(df_products) 
str(ProjectData_product )

segmentation_attributes_used_product <- intersect(segmentation_attributes_used_product, 1:ncol(ProjectData_product))
profile_attributes_used_product <- intersect(profile_attributes_used_product, 1:ncol(ProjectData_product))

ProjectData_segment_product <- ProjectData_product[,segmentation_attributes_used_product]
ProjectData_profile_product <- ProjectData_product[,profile_attributes_used_product]

ProjectData_scaled_product <- apply(ProjectData_product, 2, function(r) if (sd(r)!=0) (r-mean(r))/sd(r) else 0*r)


#------------------------------------------------------------------------------------------------------
#kmean products
kmeans_clusters_product <- kmeans(ProjectData_segment_product,centers= numb_clusters_used, iter.max=2000, algorithm=kmeans_method)

ProjectData_with_kmeans_membership_product <- cbind(1:length(kmeans_clusters_product$cluster),kmeans_clusters_product$cluster)
colnames(ProjectData_with_kmeans_membership_product)<-c("Observation Number_k","Cluster_Membership_k")

head(ProjectData_with_kmeans_membership_product)
write.csv(round(ProjectData_with_kmeans_membership_product, 2), file = "ProjectData_with_kmeans_membership_product.csv")

