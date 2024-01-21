# learning cluster analysis

# Packages
library(rattle)
library(cluster)
library(flexclust)
library(fMultivar)
library(outliers) # or
library(mvoutlier)

# 1º Escolher variaveis apropriadas
# 2º Padronizar a escala dos dados, por exemplo

  # df1 <- apply(mydata, 2, function(x) {(x - mean(x)) / sd(x)})
  # or use scale(), is similar


# 3º Uma alternativa é remover outliers, cluster podem ser sensiveis a isso

# Calculate distaces


# 4º Selecionar um algoritmo de cluster

  # better is to test them all

# 5º determine the number of cluester

# 6º visualize, interpret and validate the results


# let's start

data(nutrient, package = "flexclust")

head(nutrient)

# 
d <- dist(nutrient) # euclidean distance by default
as.matrix(d)[1:4,1:4]

# Hierarchical cluster analysis
  # the best is Average linkage and Ward methods, but the sigle linkage is less sensitive the outlier

row.names(nutrient) <- tolower(row.names(nutrient))
nutrient.scaled <- scale(nutrient)

d <- dist(nutrient.scaled)

fit.average <- hclust(d, method = "average")

plot(fit.average, hang = 1, cex =.8, 
     main = " Average Linkage Clustering")

#________________________________________________________________________

library(NbClust)
devAskNewPage(ask = F)

nc <- NbClust(nutrient.scaled, distance = "euclidean",
              min.nc = 2, max.nc = 15, method = "average")

table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab = "Numer of Clusters", ylab = " NUmber of Criteria",
        main = "NUmber of clusters Choses by 26 Criteria")

clusters <- cutree(fit.average, k = 5)
table(clusters)

aggregate(nutrient, by = list(cluster = clusters), median)

aggregate(as.data.frame(nutrient.scaled), by = list(cluster = clusters), median)

plot(fit.average, hang = 1, cex =.8,
     main = "Average Linkage Clustering/n5 Cluster Solution")

rect.hclust(fit.average, k = 5)

#_________________________________________________________

# K-means clustering
  # use for a dataset larger

wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
    }
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "within groups sum os squares")
}

data(wine, package = "rattle")
head(wine)

df <- scale(wine[-1])

wssplot(df)

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc = 2, max.nc = 15, method = "kmeans")

table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab = "Numer of Clusters", ylab = " Number of Criteria",
        main = "NUmber of clusters Choses by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(df, 3, nstart = 25)

fit.km$size

fit.km$centers

aggregate(wine[-1], by = list(cluster = fit.km$cluster), mean)


#_________________________________________

# Partitioning Around Medoids

library(cluster)

set.seed(1234)

fit.pam <- pam(wine[-1], k = 3, stand = T)

fit.pam$medoids

clusplot(fit.pam, main = "Bivariate Cluster plot")

ct.pam <- table(wine$Type, fit.pam$clustering)

randIndex(ct.pam)


# Avoiding nonexistent clusters

library(fMultivar)
set.seed(1234)

df <- rnorm2d(1000, rho = .5)

df <- as.data.frame(df)

plot(df, main = "Bivariate Normal Distribuition with rho = 0.5")


wssplot(df)

nc <- NbClust(df, min.nc = 2, max.nc = 15, method = "kmeans")

dev.new()

barplot(table(nc$Best.n[1,]),
        xlab = "Numer of Clusters", ylab = " Number of Criteria",
        main = "NUmber of clusters Choses by 26 Criteria")



#__
library(ggplot2)

fit <- pam(df, k = 2)

df$clustering <- factor(fit$clustering)

ggplot(data = df,  aes(x = V1, y = V2, color = clustering, shape = clustering)) +
    geom_point() +
    ggtitle("Clustering of Bivariate Normal Data")




plot(nc$All.index[,4], type = "o", ylab = "CCC",
     xlab = "NUmber of cluster", col = "blue")

# So, the CCC values are all negative and decreasing for two or more clusters, therefore, doesn't exist cluster


#____________________________________________________________________

# Now, a activity to improve my knowledge

data(iris)

# Select relevant variables
iris_subset <- iris[, c(1:4)]

iris_scaled <- scale(iris_subset)

distances <- dist(iris_scaled)

# Perform hierarchical clustering using Ward's method
fit_ward <- hclust(distances, method = "ward.D2")

# Plot the dendrogram
plot(fit_ward, main = "Hierarchical Clustering - Iris Dataset", sub = "", xlab = "Species",
     ylab = "Ward's Linkage Distance", hang = -1, cex = .6)

clusters_iris <- cutree(fit_ward, k = 3)

pca_result <- prcomp(iris_scaled)
iris_pca <- as.data.frame(pca_result$x[, 1:2])

iris_pca$Cluster <- factor(clusters_iris)

library(ggplot2)
ggplot(iris_pca, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  ggtitle("Hierarchical Clustering of Iris Dataset") +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  theme_minimal()


