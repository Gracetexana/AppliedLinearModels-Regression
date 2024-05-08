library("xlsx")
library("dplyr")
library("car")
library(ggplot2)
library(MASS)

# bringing data into R
path <- "C:/Users/Grace/My Drive/RIT/Applied Linear Models - Regression/Project/CyanoMaxCD_environmental_vars_FINAL.xlsx"

table <- read.xlsx(path, sheetName = "Data")
data <- table[, c(4, 8:13, 22:25)] %>% filter_all(all_vars(. != "na"))
data <- as.data.frame(sapply(data, as.numeric))


# part a
# summary statistics
summary(data)
lapply(data, sd)

# graphs
for (c in 1:length(data)){
    plot(
        data[,c],
        data$Cyanobacteria_Max_cells.ml,
        xlab = colnames(data)[c]
    )
    hist(
        data[, c],
        main = colnames(data)[c]
    )
    boxplot(
        data[, c],
        main = colnames(data)[c]
    )
}


# part b
# overall significance of the model
data_scaled <- scale(data)
model <- lm(
    Cyanobacteria_Max_cells.ml ~ .,
    data = data.frame(data_scaled)
)
summary(model) # the p-value of the F-statistic is <0.05 so we reject the null hypothesis; at least one of the regressors contributes significantly to the model

# confidence intervals
confint(model)


# part c
# lack of fit
full_model <- lm(
    Cyanobacteria_Max_cells.ml ~ as.factor(TP_ppb) + as.factor(P_dissolved_ppb) + as.factor(TKN_ppm) + as.factor(NH3_ppm) + as.factor(NOx_ppm) + as.factor(TOC_ppm) + as.factor(mayST_Celsius) + as.factor(junST_Celsius) + as.factor(julST_Celsius) + as.factor(augST_Celsius),
    data = data
)
anova(model, full_model) # this results in an F statistic of NaN and a p-value of NaN. I think it is possibly because there are no replicate data samples.

# residual plots
# all of the residual plots look pretty good, suggesting that the regression model fits well to the data
model_studres <- rstandard(model) / sqrt((1-lm.influence(model)$hat))

qqnorm(model_studres,
    ylab="Studentized Residuals",
    xlab="Normal Scores")
qqline(model_studres)

plot(model_studres ~ model$fit, 
    data=data,
    ylab="Studentized Residuals",
    xlab="Predicted y Values")
abline(0, 0)

plot(model_studres ~ Cyanobacteria_Max_cells.ml, 
    data=data,
    ylab="Studentized Residuals",
    xlab="X Values")
abline(0, 0)

# multicollinearity, outliers, influential observations
pairs(data) # several of the regressors appear to be linearly related
cor(data) # further suggests multicollinearity
vif(model) # none of the VIFs are greater than 10, but a few are greater than 5; TP_ppb has a VIF of 7.403, P_dissolved_ppb has a VIF of 5.555, and augST_Celsius has a VIF of 5.292
model_X <- as.matrix(data[,2:10])
eigen(t(model_X) %*% model_X) # there is one small eigenvalue
kappa(model) # kappa is <100 so there is no serious multicollinearity issue by this measure
summary(model) # the p-value of the F-statistic is <0.05 but most of the regressors are not significant according to their individual t tests; this may indicate multicollinearity

influence.measures(model) # 10 influential points: 8, 10, 16, 19, 23, 24, 27, 29, 39, 43 
# 8: hat, dfbetas
# 10: dffits, dfbetas
# 16: dfbetas
# 19: hat, cook's d, dffits
# 23: dfbetas
# 24: hat
# 27: dfbetas
# 29: dfbetas
# 39: hat, cook's d, dffits
# 43: hat, dffits
# as far as I know, these are valid observations
model_p <- length(model$coefficients)
model_n <- nrow(data)
hat_cutoff <- 2 * model_p / model_n
cooks_d_cutoff <- 1
dfbetas_cutoff <- 2 / sqrt(model_n)
dffits_cutoff <- 2 * sqrt(model_p / model_n)
hat_cutoff
cooks_d_cutoff
dfbetas_cutoff
dffits_cutoff

# remedial measures
# addressing multicollinearity because some of the measures used to detect multicollinearity suggested that it may be present
plot(lm.ridge( # the parameters look pretty stable already, I think ridge regression is unnecessary
    Cyanobacteria_Max_cells.ml ~ .,
    data = data.frame(data_scaled),
    lambda = seq(0, 1, 0.1)
))
