# question 1
SSR <- 5550.8166
SST <- 5784.5426

SSE <- SST - SSR
SSE

DFR <- 2
DFE <- 22
DFT <- 24

MSR <- SSR / DFR
MSR
MSE <- SSE / DFE
MSE

F <- MSR / MSE
F

pf(F, DFR, DFE, lower.tail=FALSE)


# question 2
tires <- data.frame(
    x=c(30, 31, 32, 33, 34, 35, 36),
    y=c(c(29.5, 32.1, 36.3, 38.2, 37.7, 33.6, 26.8), 
        c(30.2, 34.5, 35.0, 37.6, 36.1, 34.2, 27.4)))

# part a
plot(y~x, data=tires)

# part b
tires.model <- lm(y~x, data=tires)
tires.model

# part c
library(EnvStats)
anovaPE(tires_model)

# part d
summary(tires_model)


# question 3
solar <- read.table("TableB2.txt", header=TRUE, sep=",")
solar.model <- lm(y ~ x4, data = solar)

# part a
solar.studres <- rstandard(solar.model) / sqrt((1-lm.influence(solar.model)$hat))
qqnorm(solar.studres,
    ylab="Studentized Residuals",
    xlab="Normal Scores")
qqline(solar.studres)

# part b
plot(solar.studres~solar.model$fit, 
    data=solar,
    ylab="Studentized Residuals",
    xlab="Predicted y Values")
abline(0, 0)


# question 4
housing <- read.table("TableB4.txt", header=TRUE, sep=",")
housing.model = lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9, data=housing)

# part a
housing.studres <- rstandard(housing.model) / sqrt((1-lm.influence(housing.model)$hat))
qqnorm(housing.studres,
    ylab="Studentized Residuals",
    xlab="Normal Scores")
qqline(housing.studres)

# part b
plot(housing.studres~housing.model$fit, 
    data=housing,
    ylab="Studentized Residuals",
    xlab="Predicted y Values")
abline(0, 0)

# part c
library(car)
avPlots(housing.model)

# part d
housing.Rstudres <- rstudent(housing.model)
housing.residuals <- cbind(data.frame(housing), housing.studres, housing.Rstudres)
housing.residuals
