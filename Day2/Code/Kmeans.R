#Demonstration of clustering
#Read data
setwd("E:\\Analytics\\Training\\thoughtworks\\data\\")
data <- read.csv("Telco.csv")
myData1 <- data

#Explore data
summary(myData1)

#k-means clustering
c2 <- kmeans(myData1,2)
c3 <- kmeans(myData1,3)
c4 <- kmeans(myData1, 4)
c5 <- kmeans(myData1,5)
c6 <- kmeans(myData1,6)
#Visualizing clusters
df1 <- data.frame(myData1, c2$cluster)
plot(df1)
df2 <- data.frame(myData1, c3$cluster)
plot(df2)
df3 <- data.frame(myData1, c4$cluster)
plot(df3)
df4 <- data.frame(myData1, c5$cluster)
plot(df4)
df5 <- data.frame(myData1, c5$cluster)
plot(df1)
#Quality of clusters
c2
c3
c4
c5
c6
#Number of clusters
#Export model
require(pmml)
xm <-pmml(c2)

