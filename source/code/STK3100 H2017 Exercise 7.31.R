
# STK 3100, fall 2017
# Exercise 7.31

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Load packages
library(MASS)

# Read data
homicide.data = read.table("http://www.stat.ufl.edu/~aa/glm/data/Homicides.dat", header = T)
homicide.data[,"race"] = as.factor(homicide.data[,"race"])
head(homicide.data)
table(homicide.data[,"count"], homicide.data[,"race"])

# a)
# Fit Poisson model
Poisson.model = glm(count ~ race, family = poisson, data = homicide.data)
summary(Poisson.model)


# b)
# Fit negative binomial model
negbin.model = glm.nb(count ~ race, data = homicide.data)
summary(negbin.model)

# Test overdispersion
overdisp.test.statistic = -2*(logLik(Poisson.model) - logLik(negbin.model))
1 - pchisq(as.numeric(overdisp.test.statistic), df = 1)
# We reject the null hypothesis with alpha = 0.05.
# So, we conclude that there is overdispersion and choose for the negative binomial model.


# c)
# Wald 95% confidence interval
exp(confint.default(Poisson.model))
exp(confint.default(negbin.model))


# Extra: reconstruct table 7.5
n.white = nrow(homicide.data[(homicide.data[,"race"] == 0),])
n.black = nrow(homicide.data[(homicide.data[,"race"] == 1),])
response.range = 0:6

# Estimated number of reponses from Poisson model
Pois.mu.hat.white = predict(Poisson.model, newdata = data.frame(race = as.factor(0)), type = "response")
Pois.mu.hat.black = predict(Poisson.model, newdata = data.frame(race = as.factor(1)), type = "response")
Poisson.estimation = data.frame(
  black = n.black*dpois(x = response.range, lambda = Pois.mu.hat.black),
  white = n.white*dpois(x = response.range, lambda = Pois.mu.hat.white)
  )
Poisson.estimation = round(Poisson.estimation, 1)
Poisson.estimation

# Estimated number of reponses from negative binomial model
negbin.mu.hat.white = predict(negbin.model, newdata = data.frame(race = as.factor(0)), type = "response")
negbin.mu.hat.black = predict(negbin.model, newdata = data.frame(race = as.factor(1)), type = "response")
negbin.estimation = data.frame(
  black = n.black*dnbinom(x = response.range, size = negbin.model$theta, mu = negbin.mu.hat.black),
  white = n.white*dnbinom(x = response.range, size = negbin.model$theta, mu = negbin.mu.hat.white)
)
negbin.estimation = round(negbin.estimation, 1)
negbin.estimation
