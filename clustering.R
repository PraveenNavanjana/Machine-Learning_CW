# Loading the necessary packages

library(readxl)
library(dplyr)
library(ggplot2)
library(NbClust)
library(factoextra)
library(cluster)
library(factoextra)




# Dataset loading

datasheet <- read_excel("C:/Users/my pc/Desktop/ML cw/vehicles.xlsx") # Add dataset path
str(datasheet)

# initial 18 columns

data_c <- datasheet[, 2:19]
data_c
# identification of outliers

Data_N_outliers <- data_c
md <- mahalanobis(data_c, colMeans(data_c), cov(data_c))
outliers <- which(md > qchisq(0.95, df = ncol(data_c)))
Data_N_outliers <- Data_N_outliers[-outliers, ]


# data scalability

D_Scaled <- scale(data_c)
D_Scaled

### Utilizing four automated tools to count the clusters


NbClust
set.seed(123)
nb_clusters <- NbClust(D_Scaled, diss = NULL, distance = "euclidean", method = "kmeans", min.nc = 2, max.nc = 10, index = "all")
table(nb_clusters$Best.n[1,])

# Elbow method
wss_ratio <- c()
for (i in 1:10) {
  set.seed(123)
  kmeans_clusters <- kmeans(D_Scaled, centers = i, nstart = 10)
  wss_ratio[i] <- kmeans_clusters$tot.withinss
}
plot(1:10, wss_ratio, type = "b", main = "Elbow Method", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# Gap statistics
set.seed(123)
gap <- clusGap(D_Scaled, FUN = kmeans, nstart = 2, K.max = 10, B = 100)
plot(gap, main = "Gap Statistic", xlab = "Number of Clusters")

# Silhouette method
sil_ratio <- c()
fviz_nbclust(D_Scaled, kmeans, method = 'silhouette')

### K-means clustering using the most clusters possible

set.seed(123)
kmeans_clusters <- kmeans(D_Scaled, centers = 2, nstart = 25)
kmeans_summary <- as.data.frame(kmeans_clusters$centers)
kmeans_summary$cluster <- c("Cluster 1", "Cluster 2")
D_clustered <- datasheet %>% mutate(cluster = as.factor(kmeans_clusters$cluster))

# Evaluation metrics
BSS <- sum(kmeans_clusters$betweenss)
TSS <- sum(kmeans_clusters$totss)
WSS <- sum(kmeans_clusters$tot.withinss)
cat("Ratio of BSS to TSS:", BSS/TSS, "\n")
cat("BSS:", BSS, "\n")
cat("WSS:", WSS, "\n")

# Silhouette plot
silhouette(kmeans_clusters$cluster, dist(D_Scaled))
sil_ratio <- silhouette(kmeans_clusters$cluster, dist(D_Scaled))
fviz_silhouette(sil_ratio)



#PCA

### PCA Dimentionality Reduction


# Perform PCA
D_quality <- Data_N_outliers
D_pca <- prcomp(D_Scaled)
summary(D_pca)

# Extract the first 10 PCA components since they account for 99% of the total.

D_pca.transform <- as.data.frame(-D_pca$x[, 1:10])
D_pca.transform

#k2 cluster
k=2
kmeans_clustered.pca = kmeans(D_pca.transform, centers = k, nstart = 10) 
kmeans_clustered.pca

#CM
wss_ratio = kmeans_clustered.pca$tot.withinss
bss = kmeans_clustered.pca$betweenss
print(paste("Total within-cluster sum of square is", wss_ratio))
print(paste("Between Sum of Squares is", bss))

#plot
par(mar = c(2, 2, 2, 2))
fviz_cluster(kmeans_clustered.pca, data = D_Scaled, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())
