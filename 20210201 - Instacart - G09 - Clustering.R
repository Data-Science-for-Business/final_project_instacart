#clustering

#Step 1: create data frame for clustering by user ID

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

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
    days_since_prior_order_average = (sum(days_since_prior_order, na.rm = T))/(max(order_number)-1),
    #days_since_prior_order_current = last #I have issue with mutate and the grouping, to be solve later or added directly in the featured enginerred data
  )

#head(df_users)
#View(df_users)
nrow(df_users)
str(df_users)

#proof of concept
df_users <- df_users %>%
  slice(1:15000)

#---------------------------------------------------------------
#Step 2: create segments


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

segmentation_attributes_used <- intersect(segmentation_attributes_used, 1:ncol(ProjectData))
profile_attributes_used <- intersect(profile_attributes_used, 1:ncol(ProjectData))

ProjectData_segment <- ProjectData[,segmentation_attributes_used]
ProjectData_profile <- ProjectData[,profile_attributes_used]

ProjectData_scaled <- apply(ProjectData, 2, function(r) if (sd(r)!=0) (r-mean(r))/sd(r) else 0*r)


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
colnames(ProjectData_with_hclust_membership)<-c("Observation Number","Cluster_Membership")

write.csv(round(ProjectData_with_hclust_membership, 2), file = "ProjectData_with_hclust_membership.csv")



