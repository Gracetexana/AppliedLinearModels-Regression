# question 5
lakes <- read.table(
    "Q5.txt",
    header = TRUE,
    sep = ","
)
lakes_model <- lm(
    y ~ X1 + X2,
    data = lakes
)

# part 1
confint(
    lakes_model,
    level = 0.90
)

# part 2
library(gmodels)
T_matrix <- rbind(c(0, 1, 0), c(0, 0, 1))
glh.test(lakes_model, T_matrix, d = c(0, 0))

# part 3
T_matrix <- c(0, 0, 1)
glh.test(lakes_model, T_matrix, d = c(0))


# question 6
y <- c(16.0, 15.8, 15.6, 15.5, 14.8, 14.0, 13.5, 13.0, 12.0, 11.0)
x <- c(1700, 1720, 1730, 1740, 1750, 1760, 1770, 1780, 1790, 1795)
x2 <- x^2

# part 1
model <- lm(y ~ x + x2)

# part 2
summary(model)

# part 3
anova(model)

# part 4
model_studres <- rstandard(model) / sqrt((1-lm.influence(model)$hat))

qqnorm(model_studres,
    ylab="Studentized Residuals",
    xlab="Normal Scores")
qqline(model_studres)

plot(model_studres ~ model$fit, 
    ylab="Studentized Residuals",
    xlab="Predicted y Values")
abline(0, 0)

plot(model_studres ~ x, 
    ylab="Studentized Residuals",
    xlab="Linear Term")
abline(0, 0)

plot(model_studres ~ x2, 
    ylab="Studentized Residuals",
    xlab="Quadratic Term")
abline(0, 0)

# question 7
condo <- read.table("condo.txt", header = FALSE, sep = "\t")
condo_model <- lm(
    V1 ~ V2 + V3 + V4 + V5 + V6,
    data = condo
)

# part 1
condo_model

# part 2
pairs(
    V1 ~ V2 + V3 + V4 + V5 + V6,
    data = condo
)
cor(condo)

# part 3 and 4
summary(condo_model)

# part 5
library(car)
vif(condo_model)

condo_X <- as.matrix(condo[,2:6])
eigen(t(condo_X) %*% condo_X)

kappa(condo_model)

# part 6
library(MASS)
condo_step_model <- stepAIC(
    condo_model,
    direction = "both",
    trace = FALSE
)
summary(condo_step_model)

# part 7
vif(condo_step_model)

condo_step_X <- as.matrix(condo[,2:5])
eigen(t(condo_step_X) %*% condo_step_X)

kappa(condo_step_model)

# part 8
which(condo$V1 %in% c(boxplot.stats(condo$V1)$out))

influence.measures(condo_step_model)

model_p <- length(condo_step_model$coefficients)
model_n <- nrow(condo)
hat_cutoff <- 2 * model_p / model_n
cooks_d_cutoff <- 1
dfbetas_cutoff <- 2 / sqrt(model_n)
dffits_cutoff <- 2 * sqrt(model_p / model_n)
hat_cutoff
cooks_d_cutoff
dfbetas_cutoff
dffits_cutoff

# part 9
step_model_studres <- rstandard(condo_step_model) / sqrt((1-lm.influence(condo_step_model)$hat))

qqnorm(step_model_studres,
    ylab="Studentized Residuals",
    xlab="Normal Scores")
qqline(step_model_studres)

plot(step_model_studres ~ condo_step_model$fit, 
    data=condo,
    ylab="Studentized Residuals",
    xlab="Predicted y Values")
abline(0, 0)

plot(step_model_studres ~ V2, 
    data=condo,
    ylab="Studentized Residuals",
    xlab="Floor")
abline(0, 0)

plot(step_model_studres ~ V3, 
    data=condo,
    ylab="Studentized Residuals",
    xlab="Distance from Elevator")
abline(0, 0)

plot(step_model_studres ~ V4, 
    data=condo,
    ylab="Studentized Residuals",
    xlab="Ocean View")
abline(0, 0)

plot(step_model_studres ~ V5, 
    data=condo,
    ylab="Studentized Residuals",
    xlab="End Unit")
abline(0, 0)