credit <- read.csv("CreditApproval.csv", header = T, stringsAsFactors = F)
str(credit)
summary(credit)
plot(credit$credit_amt, credit$age)

gold <- read.csv("Gold.csv", header = T, stringsAsFactors = F)
gold$Date <- as.Date(gold$Date, format = "%d-%b-%y")
str(gold)

#testing random samples
credit <- read.csv("CreditApproval.csv", header = T, stringsAsFactor = T)
# mean(credit$age)
n1 <- sample(nrow(credit), nrow(credit)*0.1)
n1sample <- credit[n1,]
t.test(n1sample$age, mu = 35)

# if we want to know mean age is around 27
t.test(credit$age, mu = 27)
mean(credit$age)
t.test(credit$age, mu = 35)
# two-sample test - if we want to know two groups are same.
t.test(age ~ Risk, data = credit) # where y is numeric and x is a binary factor
boxplot(credit$age~credit$Risk)
t.test(duration_mths ~ Risk, data = credit)

# f-test
var.test(gold[,2], gold[,3], ratio = 1, alternative = "two.sided")
# analysis of variance
aov(gold[,2] ~ gold[,3] + gold[,4])

#testing counts/proportion for categorical
testchi <- table(credit$Risk, credit$job)
chisq.test(testchi)

# simple linear model test
names(credit)
testass <- lm(credit_amt ~ age, data = credit)
summary(testass)
testass1  <-  lm(credit_amt ~ duration_mths, data = credit)
summary(testass1)

plot(ratio)
testa2 <- lm(Ratio.1 ~ Ratio.2, data = ratio)
summary(testa2)
testa3 <- lm(Ratio.1 ~ Ratio.3, data = ratio)
summary(testa3)
testa4 <- lm(Ratio.3 ~ Ratio.4, data = ratio)
summary(testa4)