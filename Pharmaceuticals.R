install.packages("readxl")
install.packages("graphics")


library(Hmisc) #Contents and Describe
library(leaps) #Variable selection
library(MASS)
library(NbClust)


setwd("C:/Users/hitpr/Desktop/MSBA/1st semester/Business Analytics/Homework")

getwd()

pharmaceutical_data <- read.csv("Pharmaceuticals.csv", header=TRUE)
row.names(pharmaceutical_data) <- pharmaceutical_data[,2]
pharmaceutical_data <- pharmaceutical_data[, -c(1,2,12,13,14)]
pharmaceutical_data.norm <- sapply(pharmaceutical_data, scale)

pharmaceutical_data.norm

nc <- NbClust(pharmaceutical_data.norm, distance="euclidean", min.nc=2, max.nc=10, method="average")

table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")
d <- dist(pharmaceutical_data.norm)

fit.average <- hclust(d, method="average")

plot(fit.average, hang = -1, cex=0.8, main="average linkage clustering")

clusters <- cutree(fit.average, k=6)

aggregate(pharmaceutical_data.norm, by=list(cluster=clusters), median)
rect.hclust(fit.average, k=6)

#Color-coded dendrogram
library(dendextend)
dend <- as.dendrogram(fit.average)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:21)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=6)

dend <- hang.dendrogram(dend,hang_height=0.1)

plot(dend, 
     main = "Clustered Pharma data set", 
     horiz =  TRUE,  nodePar = list(cex = .007))

#Function to find the cluster centroids
clust.means <- function(x, res.clust, groups)
{
  if(!is.matrix(x))
    x <- as.matrix(x)
  means <- tapply(x, list(rep(cutree(res.clust, groups), ncol(x)),
                          col(x)),
                  mean)
  dimnames(means) <- list(NULL, dimnames(x)[[2]])
  return(as.data.frame(means))
}


pharma_centroids<-clust.means(pharmaceutical_data.norm, fit.average, 6)

y<-apply(as.matrix(pharma_centroids),2,as.double )

#Profile plot of Centroids

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = c(min(y), max(y)), xlim = c(0,9))
# label x-axes
axis(1, at = c(1:9), labels = names(pharmaceutical_data))
# plot centroids
for (i in c(1:6))
  lines(y[i,],  lty = i, lwd = 2,
        col = ifelse(i %in% c(1),"blue",
                     (ifelse(i %in% c(2),"green",
                             (ifelse(i %in% c(3),"red",
                                     (ifelse(i %in% c(4,5),"black","dark grey"))))))))

text(x = 0.5, y = pharma_centroids[, 1], labels = paste("Cluster", c(1:6)))


#Better visualization of dendograms based on the number of clusters
#All branches for a cluster are of a single color





#kmeans clustering


library(Hmisc) #Contents and Describe
library(leaps) #Variable selection
library(MASS)
library(NbClust)


setwd("C:/Users/hitpr/Desktop/MSBA/1st semester/Business Analytics/Homework")

getwd()

pharmaceutical_data <- read.csv("Pharmaceuticals.csv", header=TRUE)

pharmaceutical_data
pc<-pharmaceutical_data[,2]
row.names(pharmaceutical_data) <- pharmaceutical_data[,2]
pharmaceutical_data <- pharmaceutical_data[, -c(1,2,12,13,14)]
pharmaceutical_data.norm <- sapply(pharmaceutical_data, scale)
set.seed(42)

devAskNewPage(ask=TRUE)

nc <- NbClust(pharmaceutical_data.norm, min.nc=2, 
              max.nc=10, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

#Use the wss plot to check the number of clusters
wssplot <- function(pharmaceutical_data.norm, nc=10, seed=42) {
  wss <- (nrow(pharmaceutical_data.norm)-1)*sum(apply(pharmaceutical_data.norm, 2, var)) 
  for (i in 2:nc) {
    set.seed(42) 
    wss[i] <- sum(kmeans(pharmaceutical_data.norm, centers=i)$withinss)
  } 
  plot(1:nc, wss, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")
}
wssplot(pharmaceutical_data.norm,nc=10)


# Perform k-means cluster analysis
fit.km <- kmeans(pharmaceutical_data.norm, 5, nstart=10)
fit.km$size
fit.km$centers
fit.km$withinss

# calcualte cluster centroidsfit.km$centers
fit.km$centers


fit.km$withinss

# calcualte cluster centroidsfit.km$centers
fit.km$centers

#check if any cluster is a striking outlier
dist(fit.km$centers)


#Profile plot of Centroids

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = c(min(fit.km$centers), max(fit.km$centers)), xlim = c(0, 9))
# label x-axes
axis(1, at = c(1:9), labels = names(pharmaceutical_data))
# plot centroids
for (i in c(1:5))
  lines(fit.km$centers[i,], lty = i, lwd = 2, 
        col = ifelse(i %in% c(1),"black",
                        (ifelse(i %in% c(2),"blue",
                                (ifelse(i %in% c(3),"green",
                                        (ifelse(i %in% c(4),"red","dark grey"))))))))
                                          
text(x = 0.5, y = fit.km$centers[, 1], labels = paste("Cluster", c(1:5))) 
