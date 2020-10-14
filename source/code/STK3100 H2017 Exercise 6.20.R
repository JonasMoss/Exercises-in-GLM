
# STK 3100, fall 2017
# Exercise 6.20

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Load package VGAM
library(VGAM)

# Data
heaven = data.frame(
  race = c(1,1,0,0),
  gender = c(1,0,1,0),
  y1 = c(88,54,397,235),
  y2 = c(16,7,141,189),
  y3 = c(2,5,24,39)
)
heaven[,"race"] = as.factor(heaven[,"race"])
heaven[,"gender"] = as.factor(heaven[,"gender"])
heaven

# Fit cumulative logit model
model.1 = vglm(cbind(y1,y2,y3) ~ gender + race, family = cumulative(parallel = T), data = heaven)
summary(model.1)

# Deviance goodness-of-fit test
1 - pchisq(9.2542, df = 4)
# The test suggests that ther is no evidance for the model.

# Wald 95% confidence interavl of parameters
alpha = 0.05
# $coeff doesn't work for vglm class.
summary(model.1)$coeff
# We compute confidence interval manually
CI.beta.gender = c(0.76956 -qnorm(1-alpha/2)*0.12253, 0.76956 +qnorm(1-alpha/2)*0.12253)
CI.beta.gender

CI.beta.race = c(1.01645 -qnorm(1-alpha/2)*0.21059, 1.01645 +qnorm(1-alpha/2)*0.21059)
CI.beta.race
