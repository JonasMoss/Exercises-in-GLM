
# STK 3100, fall 2017
# Additional Exercise 3

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# (a)
# Read data directly from the web.
Beetle = read.table("http://www.stat.ufl.edu/~aa/glm/data/Beetles2.dat", header = T)


# Fit the glm model.
Beetle.model.1 = glm(cbind(dead, n - dead) ~ logdose, family = binomial(link = "logit"), data = Beetle)
# Fit the glm model with an extra suqare term.
Beetle.model.2 = glm(cbind(dead, n - dead) ~ logdose + I(logdose^2), family = binomial(link = "logit"), data = Beetle)

# Show the summary of the model.
summary(Beetle.model.1)
summary(Beetle.model.2)

# Obtain predictions from model 1.
X.grid = seq(1.65, 1.95, 0.01)
# For educative purpose, I demonstrate different ways of obtaining predictions from a fitted glm model.
# Method 1: Do it manually.
logistic.func = function (x) {1/(1 + exp(-x))}
eta.hat.model.1 = Beetle.model.1$coef[1] + Beetle.model.1$coef[2]*X.grid
y.hat.model.1 = logistic.func(eta.hat.model.1)
head(y.hat.model.1)

# Method 2: Obtain eta.hat automatically and transform it with inverse link function.
eta.hat.model.1 = predict(Beetle.model.1, newdata = data.frame(logdose = X.grid), type = "link")
y.hat.model.1 = logistic.func(eta.hat.model.1)
head(y.hat.model.1)

# Method 3: Obtain y.hat automatically.
y.hat.model.1 = predict(Beetle.model.1, newdata = data.frame(logdose = X.grid), type = "response")
head(y.hat.model.1)

# Obtain predictions from model 2.
y.hat.model.2 = predict(Beetle.model.2, newdata = data.frame(logdose = X.grid), type = "response")


# Plot the models
# Plot raw data
plot(x = Beetle[,"logdose"], y = Beetle[,"dead"]/Beetle[,"n"], pch = 16, col = 'black',
     ylim = c(0,1), xlab = "log(Dose)", ylab = "Mortality rate")
# Plot the models
matlines(x = X.grid, y = cbind(y.hat.model.1, y.hat.model.2), lty = c(2,2), lwd = c(2,2), col = c("red","blue"))
# Legend
legend("bottomright", c("Model 1", "Model 2"), lty = c(2,2), col = c("red","blue"))


# (b)
cov.mat.model.2 = summary(Beetle.model.2)$cov.scaled
cor.mat.model.2 = cov2cor(cov.mat.model.2)
show(cor.mat.model.2)


# (c)
# Fit a glm model with probit link function.
Beetle.model.3 = glm(cbind(dead, n - dead) ~ logdose, family = binomial(link = "probit"), data = Beetle)
summary(Beetle.model.3)

# Obtain predictions from model 3.
y.hat.model.3 = predict(Beetle.model.3, newdata = data.frame(logdose = X.grid), type = "response")
head(y.hat.model.3)
# Alternative: Obtain eta.hat automatically and transform it with inverse probit link (i.e. Normal cdf).
eta.hat.model.3 = predict(Beetle.model.3, newdata = data.frame(logdose = X.grid), type = "link")
y.hat.model.3 = pnorm(eta.hat.model.3)
head(y.hat.model.3)

# Plot the models
# Plot raw data
plot(x = Beetle[,"logdose"], y = Beetle[,"dead"]/Beetle[,"n"], pch = 16, col = 'black',
     ylim = c(0,1), xlab = "log(Dose)", ylab = "Mortality rate")
# Plot the models
matlines(x = X.grid, y = cbind(y.hat.model.1, y.hat.model.2, y.hat.model.3), lty = c(2,2,2), lwd = c(2,2,2), col = c("red","blue","green"))
# Legend
legend("bottomright", c("Model 1", "Model 2", "Model 3"), lty = c(2,2,2), col = c("red","blue","green"))

# Compare log-likelihood values
logLik(Beetle.model.1)
logLik(Beetle.model.2)
logLik(Beetle.model.3)

