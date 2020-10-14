
# STK 3100, fall 2017
# Oblig 1

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Exercise 1
# (a)
# Read data.
sore.throat = read.table("http://www.uio.no/studier/emner/matnat/math/STK3100/data/sore-throat.txt", header = T)
sore.throat[,"sore"] = as.factor(sore.throat[,"sore"])
sore.throat[,"type"] = as.factor(sore.throat[,"type"])
head(sore.throat)

# Fit logistic regression.
sore.throat.model.1 = glm(sore~duration, family = binomial(link = "logit"), data = sore.throat)
summary(sore.throat.model.1)


# (b)
# Extract beta.1.hat from the model
beta.1.hat = as.numeric(sore.throat.model.1$coeff[2])
# Estimate the odds ratio
odds.ratio.hat = exp(10*beta.1.hat)
show(odds.ratio.hat)


# 95% confidence interval of beta.1
# Method 1: invert Wald-test manually (Use asymptotic normality of beta.1)
se.beta = as.numeric(sqrt(diag(vcov(sore.throat.model.1))))
beta.1.hat.CI95 = c(
  beta.1.hat + qnorm(0.025)*se.beta[2],
  beta.1.hat + qnorm(0.975)*se.beta[2]
)
show(beta.1.hat.CI95)

# Method 2: invert Wald-test automatically (Use asymptotic normality of beta.1)
beta.1.hat.CI95 = confint.default(sore.throat.model.1)[2,]
show(beta.1.hat.CI95)

# Method 3: We don't know the true variance. So we use t-distribution based confidence interval
beta.1.hat.CI95 = c(
  beta.1.hat + qt(0.025, df = summary(sore.throat.model.1)$df[2])*se.beta[2],
  beta.1.hat + qt(0.975, df = summary(sore.throat.model.1)$df[2])*se.beta[2]
)
show(beta.1.hat.CI95)

# Method 4: Profile likelihood based method (invert likelihood-ratio test)
# See: http://people.upei.ca/hstryhn/stryhn208.pdf
beta.1.hat.CI95 = confint(sore.throat.model.1)[2,]
show(beta.1.hat.CI95)

# 95% confidence interval of odds ratio
# The naive way: We simply transform the confidence interval by inserting the boundary values in the transforming function.
# It's okay to do it like this since exp(10*x) is a monotonically increasing function.
# If we transform beta.1.hat with a non-monotonic function, we need to use Delta method.
odds.ratio.CI95 = exp(10*beta.1.hat.CI95)
show(odds.ratio.CI95)


# (c)
# Fit logistic regression.
sore.throat.model.2 = glm(sore~duration+type, family = binomial(link = "logit"), data = sore.throat)
summary(sore.throat.model.2)

# Extract beta.2.hat from the model
beta.2.hat = as.numeric(sore.throat.model.2$coeff[3])
# Estimate the odds ratio
odds.ratio.2.hat = exp(beta.2.hat)
show(odds.ratio.2.hat)

# 95% confidence interval of beta.2
beta.2.hat.CI95 = confint.default(sore.throat.model.2)[3,]
show(beta.2.hat.CI95)
# 95% confidence interval of odds ratio
odds.ratio.2.CI95 = exp(beta.2.hat.CI95)
show(odds.ratio.2.CI95)


# (d)
# Wald test
# Method 1: Do it manually.
beta.2.under.null = 0
se.beta.2.hat = as.numeric(sqrt(diag(vcov(sore.throat.model.2))))[3]
Wald.test.statistic = (beta.2.hat - beta.2.under.null)/se.beta.2.hat
Wald.test.p.value = 2*pnorm(q = Wald.test.statistic)
show(Wald.test.p.value)

# Method 2: Automatically.
summary(sore.throat.model.2)

# Likelihood ratio test
# Method 1: Do it manually.
LR.test.statistic = -2*(as.numeric(logLik(sore.throat.model.1)) - as.numeric(logLik(sore.throat.model.2)))
LR.test.p.value = 1 - pchisq(
  q = LR.test.statistic,
  df = length(sore.throat.model.2$coeff) - length(sore.throat.model.1$coeff)
  )
show(LR.test.statistic)
show(LR.test.p.value)

# Method 2: Automatically.
anova(sore.throat.model.1, sore.throat.model.2, test = "LRT")

# Score test
anova(sore.throat.model.1, sore.throat.model.2, test = "Rao")


