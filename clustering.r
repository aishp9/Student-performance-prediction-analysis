##setwd(apfiles)
library(cluster)
library(factoextra)
library(fpc)
library(pastecs)
library(dplyr)
K_VALUE = 6  # no. of clusters.
# Function for computing the cluster stats based on distance, cluster type and Number of cluster(k)
# converts the list into a readable comprehensive matrix
cluster_stats <- function(dist, tree, k) 
{
clust.assess = c("cluster.number","n","within.cluster.ss","average.within","average.between","wb.ratio","dunn2","avg.silwidth")
clust.size = c("cluster.size")
stats.names = c()
row.clust = c()
output.stats = matrix(ncol = k, nrow = length(clust.assess))
cluster.sizes = matrix(ncol = k, nrow = k)
for(i in c(1:k)){
  row.clust[i] <- paste("Cluster-", i, " size")
                }

for(i in c(2:k)) {
  stats.names[i] = paste("Test", i-1)
  
  for(j in seq_along(clust.assess)){
    output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
              }
			  
  for(d in 1:k) {
    cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
    dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
    cluster.sizes[d, i]
  }
}

output.stats.df <- data.frame(output.stats)
cluster.sizes <- data.frame(cluster.sizes)
cluster.sizes[is.na(cluster.sizes)] <- 0
rows.all <- c(clust.assess, row.clust)
# rownames(output.stats.df) <- clust.assess
output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
colnames(output) <- stats.names[2:k]
rownames(output) <- rows.all
is.num <- sapply(output, is.numeric)
output[is.num] <- lapply(output[is.num], round, 2)
output
}
#Reading the data
fdataset = read.csv("stud_math.csv",stringsAsFactor = TRUE)
fdataset[fdataset == "?" ] = NA
fdataset = na.omit(fdataset)
# Data description
stat.desc(fdataset)
maths = fdataset
maths$Medu = factor(maths$Medu ,  levels = c(0,1,2,3,4), labels = c('(0)none','(1)primary','(2)middle','(3)secondary','(4)higher'))
maths$Fedu = factor(maths$Fedu ,  levels = c(0,1,2,3,4), labels = c('(0)none','(1)primary','(2)middle','(3)secondary','(4)higher'))
maths$traveltime = factor(maths$traveltime ,  levels = c(1,2,3,4), labels = c('(1)<15m','(2)15-30m','(3)30m-1h','(4)>1h'))
maths$studytime = factor(maths$studytime ,  levels = c(1,2,3,4), labels = c('(1)<15m','(2)15-30m','(3)30m-1h','(4)>1h'))
maths$famrel = factor(maths$famrel ,  levels = c(1,2,3,4,5), labels = c('(1)VeryBad','(2)Bad','(3)Ok','(4)good','(5)Excellent'))
maths$health = factor(maths$health ,  levels = c(1,2,3,4,5), labels = c('(1)VeryBad','(2)Bad','(3)Ok','(4)good','(5)VeryGood'))
maths$freetime = factor(maths$freetime ,  levels = c(1,2,3,4,5), labels = c('(1)VeryLow','(2)Low','(3)Normal','(4)High','(5)VeryHigh'))
maths$goout = factor(maths$goout ,  levels = c(1,2,3,4,5), labels = c('(1)VeryLow','(2)Low','(3)Normal','(4)High','(5)VeryHigh'))
maths$Dalc = factor(maths$Dalc ,  levels = c(1,2,3,4,5), labels = c('(1)VeryLow','(2)Low','(3)Normal','(4)High','(5)VeryHigh'))
maths$Walc = factor(maths$Walc ,  levels = c(1,2,3,4,5), labels = c('(1)VeryLow','(2)Low','(3)Normal','(4)High','(5)VeryHigh'))
maths$G1 = factor(maths$G1, levels = c(0:20),labels = c(rep("F",10),rep("R",3),rep("A",8)))
maths$G2 = factor(maths$G2, levels = c(0:20),labels = c(rep("F",10),rep("R",3),rep("A",8)))
maths$G3 = factor(maths$G3, levels = c(0:20),labels = c(rep("F",10),rep("R",3),rep("A",8)))
#prepare the dataset
dset = maths
## Daisy Fn for numeric + categorical varaibles.
# Calculate Distance matrix
gower_dist <- daisy(dset, metric = "gower")
summary(gower_dist)
gower_mat = as.matrix(gower_dist)
pam_op = pam(gower_mat, K_VALUE)
plot(pam_op)

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist, diss = TRUE,k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
}
# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

### Agglomerative Clustering
agg_clust <- hclust(gower_dist, method = "complete")
plot(agg_clust,main = "Agglomerative, complete linkages")
groups <- cutree(agg_clust , k=K_VALUE) # cut tree into clusters
# draw dendogram with red borders around the clusters 
rect.hclust(agg_clust , k=K_VALUE, border="red")
# maximum amout of clusters by 7 ~ 10
#complete linkages cluster stats looks like the most balanced approach
stats_df_agg <-cluster_stats(gower_dist, agg_clust, 10) 
stats_df_agg
# --------- Choosing the number of clusters ---------#
# Using "Elbow" and "Silhouette" methods to identify the best number of clusters
# to better picture the trend, I will go with 10 clusters.

# Elbow ploting of Agglomerative clustering
t(cluster_stats(gower_dist, agg_clust, 10))[,c(1,3)] %>% 
  plot(type='b' , main = "Agglomerative clustering Elbow plot", xlab = "Num.of clusters", ylab = "Within clusters sum of squares(SS)" )
# Silhouette plot of Agglomerative clustering 
# plot x = Number of Clusters and y = Average Silhouette Width
# pick the maximum sil width as the number of clusters, and it ranges from -1 to +1
asw = data.frame(t(cluster_stats(gower_dist, agg_clust, 10)))[,c(1,8)]
plot(asw$cluster.number, asw$avg.silwidth, type='b' , main = "Agglomerative clustering Silhouette Width plot",  
     xlab = "Num.of clusters", ylab = "Average silhouette width" )
