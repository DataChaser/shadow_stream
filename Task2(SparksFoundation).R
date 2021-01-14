#Task 2: Prediction using Unsupervised ML (Predicting the Optimum Number of Clusters)

#Setting Working Directory
setwd('C:/Users/DELL/Desktop/Internships/Sparks_Foundation')

#Importing the dataset
dataset = read.csv('Iris.csv')

#Viewing and visualizing the data
head(dataset)
library(ggplot2)
ggplot() + 
  geom_point(aes(x = SepalLengthCm, y = SepalWidthCm, col = Species), data = dataset) +
  xlab('Sepal Length') +
  ylab('Sepal Width') +
  ggtitle('Sepal Dimensions Based on Species') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() + 
  geom_point(aes(x = PetalLengthCm, y = PetalWidthCm, col = Species), data = dataset) +
  xlab('Petal Length') +
  ylab('Petal Width') +
  ggtitle('Petal Dimensions Based on Species') + 
  theme(plot.title = element_text(hjust = 0.5))

#Removing non-essential columns
x = dataset[,2:5]
View(x)

#Finding the optimum number of clusters using the Elbow Method
w = vector()
for (i in 1:10) 
  w[i] = sum(kmeans(x, i)$withinss)
plot(1:10, w, type = 'b')

#Using the KMeans Clustering to divide the data into clusters
kmeans = kmeans(x, 3, iter.max = 300, nstart = 10)

#Visualizing the clusters
library(cluster)
clusplot(x = dataset, clus = kmeans$cluster, 
         lines = 0, shade = TRUE, labels = 4, plotchar = TRUE, color = TRUE, span = TRUE)

table(dataset$Species, kmeans$cluster)
