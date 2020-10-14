
# STK 3100, fall 2017
# Exercise 7.30

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Load packages
library(MASS)

# Data
shark.data = data.frame(year = 2001:2013, attacks = c(33,29,29,12,17,21,31,28,19,14,11,26,23))

# i) Poisson model vs negative binomial model
# Fir Poisson model
Poisson.model.null = glm(attacks ~ 1, family = poisson, data = shark.data)
summary(Poisson.model.null)
logLik(Poisson.model.null)

# Fit negative binomial model 
negbin.model.null = glm.nb(attacks ~ 1, data = shark.data)
summary(negbin.model.null)
logLik(negbin.model.null)
# Both log-likelihood and AIC value indicates that negative binomial model is the winner.

# Test overdispersion
overdisp.test.statistic = -2*(logLik(Poisson.model.null) - logLik(negbin.model.null))
1 - pchisq(as.numeric(overdisp.test.statistic), df = 1)
# We reject the null hypothesis with alpha = 0.05.
# So, we conclude that there is overdispersion and choose for the negative binomial model.


# ii) Test a positive linear trend over time.
# We use the winning model from i) and add year as a linear predictor.
# Fit negative binomial model
negbin.model.year = glm.nb(attacks ~ I(year-2000), data = shark.data)
summary(negbin.model.year)
# Wald test indicates that beta_year = 0, so there is no positive linear trend over time.

# Likelihood ratio test (using anova())
anova(negbin.model.null, negbin.model.year, test = "Chisq")
# Likelihood ratio test (manually)
LR.test = function(model.1, model.2) {
  LR.test.stat = -2*as.numeric(logLik(model.1) - logLik(model.2))
  df.diff = df.residual(model.1) - df.residual(model.2)
  p.value = 1 - pchisq(LR.test.stat, df.diff)
  return(list(test.stat = LR.test.stat, df.diff = df.diff, p.value = p.value))
}
LR.test(negbin.model.null, negbin.model.year)
# LR test indicates that beta_year = 0, so there is no positive linear trend over time.

# Since we test:
# H_0: beta_year = 0
# H_1: beta_year > 1,
# we perform one-sided test.
# The p-value is then 0.09428785 = (0.1885757/2).
# So, we keep H_0 and conclude that there is no positive linear trend over time.

