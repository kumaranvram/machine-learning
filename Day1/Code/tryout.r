abline(v = mean(gold$GLDr1, lwd=1, col="red"))
xfit <- seq(min(gold$GLDr1), max(gold$GLDr1), length=length(gold$GLDr1))
yfit <- dnorm (xfit, mean= mean(gold$GLDr1), sd=sd(gold$GLDr1))
yfit <- yfit*200/max(yfit)
lines(xfit, yfit, col="red", lwd=2)

qqnorm(gold$GLDr1)
qqline(gold$GLDr1, col=2)
boxplot(gold$GLDr1, col="orange", main="GLDr1")
boxplot.stats(gold$GLDr1)


