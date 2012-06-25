#Read data
setwd("E:\\Analytics\\Training\\Thoughtworks\\Data\\")
data <- read.csv("CreditApproval.csv")
#data <- read.csv("Risk.csv")
require(vcd)
myData <- data
summary(myData)
str(myData)

require(rpart)
m1 <- rpart(Risk ~., data = myData, method = "class")
m1a <- rpart(Class~., data = myData, method = "clas")
printcp(m1) # display the results
plotcp(m1) # visualize cross-validation results
summary(m1) # detailed summary of splits

# plot tree
``

# prune the tree
m1p<- prune(m1, cp= m1$cptable[which.min(m1$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(m1p, uniform=TRUE,
     main="Pruned Classification Tree for credit approval")
text(m1p, use.n=TRUE, all=TRUE, cex=.8)

#Confusion matrix
#unpruned tree
pred <- predict(m1, newdata=myData, type="class")
cm1 <- table(myData$Risk, pred)
cm1

#Pruned tree
pred2 <- predict(m1p, newdata=myData, type="class")
cm2 <- table(myData$Risk, pred)
cm2

