# problem 5.1
hydrocarbons <- data.frame(
    temperature = c(24.9, 35.0, 44.9, 55.1, 65.2, 75.2, 85.2, 95.2),
    viscosity = c(1.133, 0.9772, 0.8532, 0.7550, 0.6723, 0.6021, 0.5420, 0.5074)
)

# part a
plot(viscosity ~ temperature, data = hydrocarbons)

# part b
model <- lm(viscosity ~ temperature, data = hydrocarbons)

summary((model))

hydrocarbons.linear.studres <- rstandard(model) / sqrt((1-lm.influence(model)$hat))

qqnorm(hydrocarbons.linear.studres,
    ylab="Studentized Residuals",
    xlab="Normal Scores")
qqline(hydrocarbons.linear.studres)

plot(hydrocarbons.linear.studres ~ model$fit, 
    data=hydrocarbons,
    ylab="Studentized Residuals",
    xlab="Predicted y Values")
abline(0, 0)

plot(hydrocarbons.linear.studres ~ temperature, 
    data=hydrocarbons,
    ylab="Studentized Residuals",
    xlab="X Values")
abline(0, 0)

# part c
hydrocarbons.exponential <- lm(log(viscosity) ~ temperature, data = hydrocarbons)

summary((hydrocarbons.exponential))

hydrocarbons.exponential.studres <- rstandard(hydrocarbons.exponential) / sqrt((1-lm.influence(hydrocarbons.exponential)$hat))

qqnorm(hydrocarbons.exponential.studres,
    ylab="Studentized Residuals",
    xlab="Normal Scores")
qqline(hydrocarbons.exponential.studres)

plot(hydrocarbons.exponential.studres ~ hydrocarbons.exponential$fit, 
    data=hydrocarbons,
    ylab="Studentized Residuals",
    xlab="Predicted y Values")
abline(0, 0)

plot(hydrocarbons.exponential.studres ~ temperature, 
    data=hydrocarbons,
    ylab="Studentized Residuals",
    xlab="X Values")
abline(0, 0)


# problem 6.1
solar <- read.table(
    "TableB2.txt",
    header = TRUE,
    sep = ","
)

solar.model <- lm(
    y ~ x1 + x2 + x3 + x4 + x5,
    data = solar
)

solar.p <- length(solar.model$coefficients)
solar.n <- nrow(solar)
solar.hat.cutoff <- 2 * solar.p / solar.n
solar.cooks.d.cutoff <- 1
solar.dfbetas.cutoff <- 2 / sqrt(solar.n)
solar.dffits.cutoff <- 2 * sqrt(solar.p / solar.n)

summary(solar.model)

influence.measures(solar.model)

solar.hat.cutoff
solar.cooks.d.cutoff
solar.dfbetas.cutoff
solar.dffits.cutoff

alt.solar.model <- lm(
    y ~ x1 + x3 + x4,
    data = solar
)

model_p <- length(alt.solar.model$coefficients)
model_n <- nrow(solar)
alt.solar.hat.cutoff <- 2 * model_p / model_n
alt.solar.cooks.d.cutoff <- 1
alt.solar.dfbetas.cutoff <- 2 / sqrt(model_n)
alt.solar.dffits.cutoff <- 2 * sqrt(model_p / model_n)

summary(alt.solar.model)

influence.measures(alt.solar.model)

alt.solar.hat.cutoff
alt.solar.cooks.d.cutoff
alt.solar.dfbetas.cutoff
alt.solar.dffits.cutoff
