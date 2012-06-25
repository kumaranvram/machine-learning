#Read data
setwd("E:\\Analytics\\Training\\Thoughtworks\\Data\\")
#data <- read.csv("CreditApproval.csv")
data <- read.csv("RiskLR.csv")
require(vcd)
myData <- data
summary(myData)
str(myData)
attach(myData)

#Model with 2 variables of low strength
m1 <- glm(newClass ~Ratio1+Ratio2, data = myData, family=binomial())
summary(m1) 


#Confusion matrix
predP <- predict(m1, type="response")
pred <- ifelse(predP<0.5, 1, 2)
cm1 <- table(myData$newClass, pred)
cm1

#Model with 1 variable of high strength
m2 <- glm(newClass~Ratio1+Ratio2+Ratio3+Ratio4, data = myData, family=binomial())
summary(m2) 


#Confusion matrix
predP <- predict(m2, type="response")
pred <- ifelse(predP<0.5, 1, 2)
cm2 <- table(myData$newClass, pred)
cm2


