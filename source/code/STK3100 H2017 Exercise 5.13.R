
# STK 3100, fall 2017
# Exercise 5.13

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Simulate data
set.seed(1)
x = runif(n = 100, min = 0, max = 100)
eta = -2 + 0.04*x
pi.vec = exp(eta)/(1+exp(eta))
y = rbinom(n = 100, size = 1, prob = pi.vec)

# Fit a logistic regression
model.1 = glm(y~x, family = binomial(link = "logit"))
summary(model.1)
# Fitted values from the model
pi.hat = predict(model.1, type = "response")
# Compute residuals
residual.values = residuals(model.1, type = "deviance")
residual.values = residuals(model.1, type = "pearson")

# Plot: residuals against x
plot(x = x, y = residual.values, xlab = "x", ylab = "residuals")

# Plot: residuals against fitted values
plot(x = pi.hat, y = residual.values, xlab = "pi.hat", ylab = "residuals")


# Experiment
# Plot: residuals against x
plot(x = x, y = abs(residual.values), xlab = "x", ylab = "residuals")
# Plot: residuals against fitted values
plot(x = pi.hat, y = abs(residual.values), xlab = "pi.hat", ylab = "residuals")
