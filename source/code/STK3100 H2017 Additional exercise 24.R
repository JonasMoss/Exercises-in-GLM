
# STK 3100, fall 2017
# Additional Exercise 24

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

library(nlme)

# Load data
data(Orthodont)
# Transform Subject to the values 1-27
Orthodont[,"ID"] = as.factor(as.numeric(Orthodont[,"Subject"]))
head(Orthodont)
help(Orthodont)


# (a)
# Exploratory analysis
plot(Orthodont) #Plot Age againts Distance for each Subject
distance.by.age = split(Orthodont[,"distance"], Orthodont[,"age"])
boxplot(distance.by.age)


# (b)
# Plot distance against age. For every group, add a regression line
plot(Orthodont[,"age"], Orthodont[,"distance"], col = Orthodont[,"ID"])
for(i in 1:length(unique(Orthodont[,"ID"]))){
  Orthodont.per.ID = Orthodont[Orthodont[,"ID"] == i, ]
  abline(lm(distance ~ age, data = Orthodont.per.ID), col = i)
}


# (c)
# Fit linear mixed effects model with a random intercept and no constraints on the structure of covariance matrix.
fit1 = lme(distance ~ age, data = Orthodont, random = ~1|ID)
# Plot the data and the results from the LME
plot(Orthodont[,"age"], Orthodont[,"distance"], col = Orthodont[,"ID"])
beta.0.hat = fit1$coef$fixed["(Intercept)"]
beta.1.hat = fit1$coef$fixed["age"]
abline(a = beta.0.hat, b = beta.1.hat, lwd = 4) # y.hat no conditioning on random effect
for (i in 1:length(unique(Orthodont[,"ID"]))){
  b.hat.i = fit1$coef$random$ID[i,]
  abline(a = beta.0.hat + b.hat.i, b = beta.1.hat, col = i) # y.hat|b_i conditioned on random effect
}


# (d)
summary(fit1)
mean(as.matrix(ranef(fit1)))
# Estimate of \sigma_{\mu}^{2}
2.114772^2 #??? How to extract this number from model summary with a syntax? 

# Estimate of \sigma_{\epsilon}^{2}
1.431592^2
(summary(fit1)$sigma)^2


# (e)
# Add fixed covariate "sex".
fit2 = lme(distance ~ age + Sex,data = Orthodont, random = ~1|ID)
summary(fit2)
# According to the Wald test, "sex" is a significant variable.
# Interpretaion
# Interpretaion of fixed effects is same as in normal linear model.
# Interpretaion of the variance of random effect:
# Variance between individuals after it's corrected the difference caused by fixed effects.
