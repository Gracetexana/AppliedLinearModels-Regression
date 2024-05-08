# question 8.5
gas <- read.table("TableB3.txt", header = TRUE, sep = ",")

# part a
gas.lm <- lm(y ~ x10 + x11, data = gas)
anova(gas.lm)

# part b
alt.gas.lm <- lm(y ~ x10 + x11 + x10*x11, data = gas)
anova(alt.gas.lm)
alt.gas.lm
