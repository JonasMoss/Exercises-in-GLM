
# STK 3100, fall 2017
# Additional Exercise 22

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# (a)
# Enter the data
lung.cancer.data = data.frame(
  city = rep(1:4, each = 5),
  age = rep(1:5, times = 4),
  cases = c(11,11,11,10,11,13,6,15,10,12,4,8,7,11,9,5,7,10,14,8),
  number = c(3059,800,710,581,509,2879,1083,923,834,634,3142,
         1050,895,702,535,2520,878,839,631,539)
  )
lung.cancer.data[,"age"] = as.factor(lung.cancer.data[,"age"])
lung.cancer.data[,"city"] = as.factor(lung.cancer.data[,"city"])
head(lung.cancer.data)
# Fit Poisson GLM
Poisson.model.1 = glm(cases ~ offset(log(number)) + age + city, family = poisson, data = lung.cancer.data)
summary(Poisson.model.1)


# (c)
lung.cancer.data[,"Fredericia"] = as.factor(as.numeric(lung.cancer.data[,"city"] == 1))
head(lung.cancer.data)
# Fit Poisson GLM
Poisson.model.2 = glm(cases ~ offset(log(number)) + age + Fredericia, family = poisson, data = lung.cancer.data)
summary(Poisson.model.2)

# Likelihood ratio test.
anova(Poisson.model.2, Poisson.model.1)
1 - pchisq(anova(Poisson.model.2, Poisson.model.1)$Deviance[2], df = 1)


# (d)
# Estimated rate ratio
exp(summary(Poisson.model.2)$coefficients["Fredericia1","Estimate"])
# 95% confidence interval of rate ratio
exp(confint.default(Poisson.model.2))["Fredericia1",]


# (e)
# Numeric version of variable age
lung.cancer.data[,"age.numeric"] = rep(
  c(mean(40,55), mean(55,60), mean(60,65), mean(65,70), mean(70,75)),
  times = 4
  )
# Fit Poisson GLM
Poisson.model.3 = glm(cases ~ offset(log(number)) + I(age.numeric-40) + Fredericia, family = poisson, data = lung.cancer.data)
summary(Poisson.model.3)

# Likelihood ratio test.
anova(Poisson.model.3, Poisson.model.2)
1 - pchisq(anova(Poisson.model.3, Poisson.model.2)$Deviance[2], df = 1)

