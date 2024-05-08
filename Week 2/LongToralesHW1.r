# question 2.1
football <- read.table("TableB1.txt", header=TRUE, sep=",")

# part a
plot(y ~ x8, data=football)
football.model <- lm(y ~ x8, data=football)
abline(football.model)
summary(football.model)

# part b
anova(football.model)

# part c
confint(football.model, level=0.95)

# part d
summary(football.model)

# part e
part.2.1.e <- data.frame(x8=2000)
predict(football.model, part.e, interval="confidence", level=0.95)


# question 2.3
solar <- read.table("TableB2.txt", header=TRUE, sep=",")

# part a
plot(y ~ x4, data = solar)
solar.model <- lm(y ~ x4, data = solar)
abline(solar.model)
summary(solar.model)

# part b
anova(solar.model)

# part c
confint(solar.model, level=0.99)

# part d
summary(solar.model)

# part e
part.2.3.e <- data.frame(x4=16.5)
predict(solar.model, part.2.3.e, interval="confidence", level=0.95)


# question 2.7
distillation <- read.table("DistillationTable.txt", header=TRUE, sep=",")

# part a
plot(Purity.... ~ Hydrocarbon...., data=distillation)
distillation.model <- lm(Purity.... ~ Hydrocarbon...., data=distillation)
abline(distillation.model)
summary(distillation.model)

# part b
anova(distillation.model)

# part c
summary(distillation.model)

# part d
confint(distillation.model, level=0.95)

# part e
part.2.7.e <- data.frame(Hydrocarbon....=1.00)
predict(distillation.model, part.2.7.e, interval="confidence", level=0.95)


# question 2.13
ozone <- read.table("OzoneTable.txt", header=TRUE, sep=",")

# part a
plot(Days ~ Index, data=ozone)

# part b
viscosity.model <- lm(Days ~ Index, data=ozone)
summary(viscosity.model)

# part c
anova(viscosity.model)

# part d
plot(Days ~ Index, data=ozone, ylim=c(-30, 130))
CI = predict(viscosity.model, interval = "confidence", level=0.95)
PI = predict(viscosity.model, interval = "prediction", level=0.95)
abline(viscosity.model, col="red")
lines(sort(ozone$Index), CI[order(ozone$Index), 2], lty = 2, col = "blue")
lines(sort(ozone$Index), CI[order(ozone$Index), 3], lty = 2, col = "blue")
lines(sort(ozone$Index), PI[order(ozone$Index), 2], lty = 2, col = "green")
lines(sort(ozone$Index), PI[order(ozone$Index), 3], lty = 2, col = "green")
