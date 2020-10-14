
# STK 3100, fall 2017
# Additional Exercise 19

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# (d)
# Enter the data
diabetes = as.data.frame(matrix(c(377, 17864, 336, 20099), 2, 2))
rownames(diabetes) = c("diseased","healthy")
colnames(diabetes) = c("male","female")
show(diabetes)

# Perform chi-square test
chisq.test(diabetes)

# (e)
# Estimated odds ratio
OR.hat = diabetes[1,1]*diabetes[2,2]/(diabetes[1,2]*diabetes[2,1])
show(OR.hat)
# Standard error of log of odds ratio
std.error = sqrt(sum(1/diabetes))

# 95% confidence interval of odds ratio
low.CI = OR.hat*exp(-qnorm(0.975)*std.error)
upp.CI = OR.hat*exp(qnorm(0.975)*std.error)
c(low.CI, upp.CI)


# (g)
# Modify data such that it's suitable for logistic regression
diabetes.glm.form = data.frame(
  y = as.numeric(diabetes["diseased",]),
  n = as.numeric(colSums(diabetes)),
  sex = c(1,0))
diabetes.glm.form[,"sex"] = as.factor(diabetes.glm.form[,"sex"])
head(diabetes.glm.form)

# Fit logistic regerssion
diabetes.model.1 = glm(cbind(y, n - y) ~ sex, family = binomial(link = "logit"), data = diabetes.glm.form)
summary(diabetes.model.1)


# Confidence interval based on logistic regression
beta.1.hat = summary(diabetes.model.1)$coefficients["sex1","Estimate"]
std.error.beta.1 = summary(diabetes.model.1)$coefficients["sex1","Std. Error"]
low.CI = exp(beta.1.hat - qnorm(0.975)*std.error.beta.1)
upp.CI = exp(beta.1.hat + qnorm(0.975)*std.error.beta.1)
c(low.CI, upp.CI)
