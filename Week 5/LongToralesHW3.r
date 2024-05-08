# question 3.1
football <- read.table("TableB1.txt", header=TRUE, sep=",")

# part a
football.model <- lm(y ~ x2+x7+x8, data=football)
football.model

# part b and e
anova(football.model)

# part c and d
summary(football.model)


# question 3.10
wine <- read.table("TableB11.txt", header=TRUE, sep=",")

# part a
wine.model <- lm(y ~ x1+x2+x3+x4+x5, data=wine)
wine.model

# part b and c
summary(wine.model)

# part d
wine.alternate <- lm(y ~ x2+x4, data=wine)
summary(wine.alternate)

# part e
confint(wine.model)
confint(wine.alternate)