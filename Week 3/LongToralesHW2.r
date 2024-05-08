# problem 1
# part a
storage.temp = c(-8, -4, 0, 4, 8)
time = c(11.7, 11, 10.2, 9, 7.8)
n = length(storage.temp)

Y = as.vector(time)
X = as.matrix(cbind(rep(1, n), storage.temp))

b = solve(t(X) %*% X) %*% t(X) %*% Y

# part b
flavor.model = lm(time ~ storage.temp)
sse = sum((fitted(flavor.model) - time)^2)

sigma2.hat = sse / (n-2)

var.b = sigma2.hat * solve(t(X) %*% X)
vcov(flavor.model)

# part c
Xh = t(as.matrix(c(1, -6)))
Yh = Xh %*% b

# part d answered in part b

# part e
part.1.e <- data.frame(storage.temp = -6)
predict(flavor.model, part.1.e, interval = "prediction", level = 0.95)


# problem 2.15
# part a
temperature = c(24.9, 35.0, 44.9, 55.1, 65.2, 75.2, 85.2, 95.2)
viscosity = c(1.1330, 0.9772, 0.8532, 0.7550, 0.6723, 0.6021, 0.5420, 0.5074)
n = length(temperature)

Y = as.vector(viscosity)
X = as.matrix(cbind(rep(1, n), temperature))

b = solve(t(X) %*% X) %*% t(X) %*% Y

# part b
viscosity.model <- lm(viscosity ~ temperature)
summary(viscosity.model)
anova(viscosity.model)

# part c
plot(viscosity ~ temperature)
abline(viscosity.model, col = "red")
CI = predict(viscosity.model, interval = "confidence", level=0.95)
PI = predict(viscosity.model, interval = "prediction", level=0.95)
lines(sort(temperature), CI[order(temperature), 2], lty = 2, col = "blue")
lines(sort(temperature), CI[order(temperature), 3], lty = 2, col = "blue")
lines(sort(temperature), PI[order(temperature), 2], lty = 2, col = "green")
lines(sort(temperature), PI[order(temperature), 3], lty = 2, col = "green")


# problem 2.18
# part a
commercials <- read.table("commercialsTable.txt", header = TRUE, sep = ",")

Y = as.vector(commercials$Returned.Impressions.per.week..millions.)
n = length(Y)
X = as.matrix(cbind(rep(1, n), commercials$Amount.Spent..millions.))

b = solve(t(X) %*% X) %*% t(X) %*% Y

# part b
commercials.model <- lm(Returned.Impressions.per.week..millions. ~ Amount.Spent..millions., data = commercials)
summary(commercials.model)

# part c
plot(Returned.Impressions.per.week..millions. ~ Amount.Spent..millions., data = commercials)
abline(commercials.model, col = "red")
CI = predict(commercials.model, interval = "confidence", level=0.95)
PI = predict(commercials.model, interval = "prediction", level=0.95)
lines(sort(commercials$Amount.Spent..millions.), CI[order(commercials$Amount.Spent..millions.), 2], lty = 2, col = "blue")
lines(sort(commercials$Amount.Spent..millions.), CI[order(commercials$Amount.Spent..millions.), 3], lty = 2, col = "blue")
lines(sort(commercials$Amount.Spent..millions.), PI[order(commercials$Amount.Spent..millions.), 2], lty = 2, col = "green")
lines(sort(commercials$Amount.Spent..millions.), PI[order(commercials$Amount.Spent..millions.), 3], lty = 2, col = "green")

# part d
Xh = data.frame(Amount.Spent..millions. = commercials["MCI", "Amount.Spent..millions."])
predict(commercials.model, Xh, interval="confidence", level=0.95)
predict(commercials.model, Xh, interval="prediction", level=0.95)
