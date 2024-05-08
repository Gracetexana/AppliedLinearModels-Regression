# question 1
# part 3
tortoise <- data.frame(endemics = c(23, 21, 3, 9, 1, 11, 0, 7),
                    species = c(58, 31, 3, 25, 2, 18, 24, 10))
tortoise.model <- lm(species ~ endemics, data = tortoise)
summary(tortoise.model)

# part 4
confint(tortoise.model, level = 0.95)

# part 6
y.hat.23 <- predict(tortoise.model, newdata = data.frame(endemics = 23))

library(dplyr)
y.23 <- tortoise %>% filter(endemics == 23) %>% pull(species)

res.23 <- y.hat.23 - y.23
res.23

# part 7
3.30**2

# part 8
library(gmodels)
glh.test(tortoise.model, c(0, 1), c(2))


# question 2
# part 2
DFR.2 <- 3
DFT.2 <- 23
DFE.2 <- DFT.2 - DFR.2
DFE.2

SSE.2 <- 61.44300
SST.2 <- 689.26000
SSR.2 <- SST.2 - SSE.2
SSR.2

MSR.2 <- SSR.2 / DFR.2
MSR.2

MSE.2 <- SSE.2 / DFE.2
MSE.2

MST.2 <- SST.2 / DFT.2

F.2 <- MSR.2 / MSE.2
F.2

p.2 <- pf(F.2, DFR.2, DFE.2, lower.tail = FALSE)
p.2

# part 4
r2.2 <- SSR.2 / SST.2
r2.2

# part 5
r2adj.2 <- 1- (MSE.2 / MST.2)
r2adj.2

# part 6
sigma.hat.2 <- sqrt(MSE.2)
sigma.hat.2


# question 3
# part b
k.3 <- 2
n.3 <- 25
r2.3 <- 0.90

F.3 <- (r2.3 * (n.3 - (k.3 +1))) / (k.3 * (1 - r2.3))
F.3

DFR.3 <- k.3
DFT.3 <- n.3 - 1
DFE.3 <- DFT.3 - DFR.3

p.3 <- pf(F.3, DFR.3, DFE.3, lower.tail = FALSE)
p.3


# question 4
soap <- data.frame(grams.product = c(4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0),
                    suds.height = c(32, 43, 45, 51, 53, 61, 62))

# part a
soap.model <- lm(suds.height ~ grams.product, data = soap)
soap.model

# part b
summary(soap.model)

# part c
soap.res <- resid(soap.model)
soap.modified <- cbind(soap, soap.model$fit, soap.res)
soap.modified

soap.studres <- rstandard(soap.model) / sqrt((1 - lm.influence(soap.model)$hat))
qqnorm(soap.studres,
    ylab="Studentized Residuals",
    xlab="Normal Scores")
qqline(soap.studres)

plot(soap.studres ~ soap.model$fit, 
    data = soap,
    ylab = "Studentized Residuals",
    xlab = "Predicted y Values")
abline(0, 0)


# question 5
automobile <- read.table("AutomobileTable.txt", header = TRUE, sep = ",")

# part a
automobile.model <- lm(Y.Miles.gal. ~ X1.Displacement..in3. + X2.Weight..lbs., data = automobile)
automobile.model

# part b and c
anova(automobile.model)
summary(automobile.model)

# part d
confint(automobile.model)

# part e
predict(automobile.model, data.frame(X1.Displacement..in3. = 275, X2.Weight..lbs. = 3000), interval = "confidence", level = 0.95)

# part f
predict(automobile.model, data.frame(X1.Displacement..in3. = 275, X2.Weight..lbs. = 3000), interval = "prediction", level = 0.95)

# part h
automobile.studres <- rstandard(automobile.model) / sqrt((1 - lm.influence(automobile.model)$hat))
qqnorm(automobile.studres,
    ylab="Studentized Residuals",
    xlab="Normal Scores")
qqline(automobile.studres)

plot(automobile.studres ~ automobile.model$fit, 
    data = automobile,
    ylab = "Studentized Residuals",
    xlab = "Predicted y Values")
abline(0, 0)

plot(automobile.studres ~ X1.Displacement..in3., 
    data = automobile,
    ylab = "Studentized Residuals",
    xlab = "Displacement")
abline(0, 0)

plot(automobile.studres ~ X2.Weight..lbs., 
    data = automobile,
    ylab = "Studentized Residuals",
    xlab = "Weight")
abline(0, 0)
