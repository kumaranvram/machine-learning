#Demonstration of clustering
#Read data
setwd("E:\\Analytics\\Training\\thoughtworks\\data\\")
telco <- read.csv("Telco.csv")
attach(telco)
#telco$ARPUStatus <- ifelse(ARPU<=25, "A1", ifelse(ARPU>25 & ARPU<=50, "A2", ifelse(ARPU>50 & ARPU <=75, "A3", ifelse(ARPU>75 & ARPU <=100, "A4", "A5"))))
myData1 <- telco

newdata <- sample(nrow(myData1), nrow(myData1)*0.1)
newdata1 <- myData1[newdata,]

#Explore data
#summary(myData1)
View(newdata1)
#k-means clustering
#myData1 <- newdata1
c2 <- kmeans(newdata1,2)
c3 <- kmeans(newdata1,3)
c4 <- kmeans(newdata1, 4)
c5 <- kmeans(newdata1,5)
c6 <- kmeans(newdata1,6)
c7 <- kmeans(newdata1, 7)
c8 <- kmeans(newdata1, 8)
#Visualizing clusters
newdata1$ARPUStatus <- ifelse(newdata1$ARPU<=25, "A1", ifelse(newdata1$ARPU>25 & newdata1$ARPU<=50, "A2", ifelse(newdata1$ARPU>50 & newdata1$ARPU <=75, "A3", ifelse(newdata1$ARPU>75 & ARPU <=100, "A4", "A5"))))
View(newdata1)
df2 <- data.frame(newdata1, c2$cluster)
newdata1$c2cluster = c2$cluster
plot(df2)
View(newdata1)
table(newdata1$ARPUStatus, newdata1$c2cluster)
df3 <- data.frame(newdata1, c3$cluster)
plot(df3)
newdata1$c3cluster = c3$cluster
table(newdata1$ARPUStatus, newdata1$c3cluster)
df4 <- data.frame(newdata1, c4$cluster)
newdata1$c4cluster = c4$cluster
plot(df4)
df5 <- data.frame(newdata1, c5$cluster)
newdata1$c5cluster = c5$cluster
table(newdata1$ARPUStatus, newdata1$c4cluster)
table(newdata1$ARPUStatus, newdata1$c5cluster)
plot(df5)
df6 <- data.frame(newdata1, c6$cluster)
newdata1$c6cluster = c6$cluster
plot(df6)
df7 <- data.frame(newdata1, c7$cluster)
newdata1$c7cluster = c7$cluster
plot(df7)
df8 <- data.frame(newdata1, c8$cluster)
newdata1$c8cluster = c8$cluster
plot(df8)


table(newdata1$ARPUStatus, newdata1$c6cluster)
table(newdata1$ARPUStatus, newdata1$c7cluster)
table(newdata1$ARPUStatus, newdata1$c8cluster)

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

table(newdata$ARPUStatus, c2)
table(newdata$ARPUStatus, c3)

