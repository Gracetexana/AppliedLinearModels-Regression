# question 1
softdrink <- read.table(
    "SoftDrinkTable.txt",
    header = TRUE,
    sep = ","
)

# part a
pairs(
    Delivery.Time.y..min. ~ Number.of.Cases.x1 + Distance.x2..ft.,
    data = softdrink
)

# part b
cor(softdrink[, 2:4])

# part c
softdrink_model <- lm(
    Delivery.Time.y..min. ~ Number.of.Cases.x1 + Distance.x2..ft.,
    data = softdrink
)

library(car)
vif(softdrink_model)

softdrink_X <- as.matrix(softdrink[, 3:4])
eigen(t(softdrink_X) %*% softdrink_X)

kappa(softdrink_model)

summary(softdrink_model)

# part d
library(MASS)

softdrink_scaled <- scale(softdrink)
softdrink_model_scaled <- lm(
    Delivery.Time.y..min. ~ Number.of.Cases.x1 + Distance.x2..ft.,
    data = data.frame(softdrink_scaled)
)

plot(lm.ridge(
    Delivery.Time.y..min. ~ Number.of.Cases.x1 + Distance.x2..ft.,
    data = data.frame(softdrink_scaled),
    lambda = seq(0, 1, 0.1)
))
select(lm.ridge(
    Delivery.Time.y..min. ~ Number.of.Cases.x1 + Distance.x2..ft.,
    data = data.frame(softdrink_scaled),
    lambda = seq(0, 1, 0.01)
))
softdrink_model_ridge <- lm.ridge(
    Delivery.Time.y..min. ~ Number.of.Cases.x1 + Distance.x2..ft.,
    data = data.frame(softdrink_scaled),
    lambda = 0.36
)

coef(softdrink_model_ridge)
softdrink_model_scaled

# question 2
wine <- read.table(
    "TableB19.txt",
    header = TRUE,
    sep = ","
)

pairs(
    y ~ .,
    data = wine
)

cor(wine)

wine_model <- lm(
    y ~ .,
    data = wine
)
vif(wine_model)
wine_model <- lm(
    y ~ x1 + x2 + x3 + x4 + x5 + x6 + x8 + x9 + x10,
    data = wine
)
vif(wine_model)

wine_X <- as.matrix(wine[, 2:10])
eigen(t(wine_X) %*% wine_X)

kappa(wine)

summary(wine_model)
