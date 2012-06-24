# R Version 2.14.0 
### EXPLORATARY DATA ANALYSIS #################################################

library(Hmisc, warn.conflicts = T)
library(arm, warn.conflicts = T)
library(scatterplot3d, warn.conflicts = T)
library(lattice, warn.conflicts = T)

setwd("D:/Karma/_Compegence/EDA-VDA")

### GOLD RATES DATA -----------------------------------------

gold <- read.csv("Gold.csv", header = T, stringsAsFactors = F)
gold$Date <- as.Date(gold$Date, format = "%d-%b-%y")
gold$rCol = ifelse(gold$GLDr1>0,"blue","red")
str(gold)
summary(gold)
describe(gold)
dim(gold)

# density plot
plot(density(gold$GLDr1), col = "blue")
abline(v = mean(gold$GLDr1), lwd = 1, col = "red")

# Histogram with normal curve
h <- hist(gold$GLDr1, breaks=10, xlab="Gold Return", main="Data Distribution", ylim = c(0, 200))
abline(v = mean(gold$GLDr1), lwd = 1, col = "red")
xfit <- seq(min(gold$GLDr1),max(gold$GLDr1), length = length(gold$GLDr1))
yfit <- dnorm(xfit, mean = mean(gold$GLDr1), sd = sd(gold$GLDr1))
yfit <- yfit*200/max(yfit)
lines(xfit, yfit, col="blue", lwd=2) 

# Normal Quantile-Quantile plot
qqnorm(gold$GLDr1)
qqline(gold$GLDr1, col = 2)

# boxplot
boxplot(gold$GLDr1, col = "orange", main = "GLDr1")
boxplot.stats(gold$GLDr1)

#timeseries plot
ts.gold <- ts(gold$GLDr1, start = c(2001, 01), freq = 52)
str(ts.gold)

plot(ts.gold)
abline(h=mean(ts.gold)-sd(ts.gold)*3.3,col='blue')
abline(h=mean(ts.gold),col='red')
abline(h=mean(ts.gold)+sd(ts.gold)*3.3,col='green')

plot(stl(ts.gold, s.window = 52), main = "GLDr1", col = "blue")
stl.gold <- stl(ts.gold, s.window = 52)
plot(stl.gold, main = "GLDr1", col = "blue")

### CREDIT APPROVAL DATA -----------------------------------------

credit <- read.csv("CreditApproval.csv", header = T, stringsAsFactors = F)
credit$RiskCol = ifelse(credit$Risk==" good.","blue","red")
num.credit <- credit[,c(2,5,8,11,13)]
str(credit)
summary(credit)
describe(credit)

# bi-variate plots -----------------------

# scatter plot
plot(credit$credit_amt, credit$age, col=credit$RiskCol)
cor(credit$credit_amt, credit$age)

# box plot
boxplot(credit$credit_amt, credit$age)

# bar plot
Risk.House <- table(credit$Risk, credit$housing)
barplot(Risk.House, col=c('red','blue'))
barplot(Risk.House, beside=TRUE, col=c('red','blue'))
legend(x="topright",legend=c("Good","Bad"),fill=c('red','blue'))
plot(Risk.House, col=c(2:7),xlab="Risk",ylab="Housing",main="Frequency by Risk & Housing")

# mosaic plot
Cr.Risk <- xtabs(credit_amt ~ job, data = credit)
Cr.Risk
mosaicplot(Cr.Risk, col = hcl(c(20,120)), main="Credit Amt by Risk")
#
Cr.Job <- xtabs(credit_amt ~ job, data = credit)
Cr.Job
mosaicplot(Cr.Job, col = hcl(c(20,120)), main="Credit Amt by Job")
#
Cr.Job.Risk <- xtabs(credit_amt ~ Risk+job, data = credit)
Cr.Job.Risk
mosaicplot(Cr.Job.Risk, col = hcl(c(20,120)), main="Credit Amt by Risk & Job")

## Multivariate plots -------------

#timeseries plot - Gold
num.gold <- gold[,3:(ncol(gold)-1)]
palette(rainbow(ncol(num.gold)))
ts.plot(num.gold,type='l',col=1:ncol(num.gold))
legend(x="topright",legend=colnames(num.gold),col=1:ncol(num.gold),lty=1,cex=.5)

#boxplot - Gold
boxplot(num.gold,col=1:ncol(num.gold))
num.gold$FCXVolNorm <- ifelse(num.gold$FCXVolNorm>.75,.75,num.gold$FCXVolNorm)
boxplot(num.gold,col=1:ncol(num.gold))

#scatter plot - Gold
plot(num.gold)
plot(num.gold,col=gold$rCol)

#correlation plot - Gold
cor(num.gold)
corrplot(num.gold,color=T)

#scatter plot - Credit
pairs(num.credit,col=credit$RiskCol)
pairs(num.credit[,c(1,2,5)],col=credit$RiskCol)

#correlation plot - Credit
cor(num.credit)
corrplot(num.credit, color = T);

#mosaic plot
mVar <- xtabs(credit_amt ~ gender_stat+phone+Risk+job, data = credit)
mVar <- xtabs(credit_amt ~ property+job, data = credit)
mVar
palette(rainbow(12))
mosaicplot(mVar, col = c(1:12), main="Credit Amt by Risk & Job")

#parallel coord plot
parcoord(num.credit[,c(1,2,5)],col=credit$RiskCol)

palette(rainbow(nrow(gold)))
parcoord(num.gold,col=c(1:nrow(gold)))

#conditional plot
coplot(age~duration_mths|credit_amt, data = num.credit)

# 3d scatter plot
cloud(GLDr1~gold[,3]*gold[,8], groups = rCol, data = gold)
cloud(GLDr1~FCXr1*SPYr1, groups = rCol, data = gold)
cloud(age~duration_mths*resi_since, groups = credit_amt, data = num.credit)

# 3d surface plot
CrRisk=" good."
xCr <- quantile(credit[which(credit$Risk==CrRisk),]$duration_mths,prob=seq(0,1,.2))
yCr <- quantile(credit[which(credit$Risk==CrRisk),]$resi_since,prob=seq(0,1,.2))
g <- expand.grid(x = xCr, y = yCr, gr=CrRisk)
g$z <- quantile(credit[which(credit$Risk==CrRisk),]$age,prob=seq(0,1,.2))
wireframe(z~x*y, data = g, scales = list(arrows = FALSE), drape = TRUE, colorkey = TRUE)

g <- expand.grid(x = 1:10, y = 5:15, gr = 1:3)
g$z <- log((g$x^g$g + g$y^2) * g$gr)
wireframe(z ~ x * y, data = g, groups = gr, scales = list(arrows = FALSE), drape = TRUE, colorkey = TRUE, screen = list(z = 30, x = -60))


