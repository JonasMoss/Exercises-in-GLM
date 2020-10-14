
# STK 3100, fall 2017
# Exercise 8.16

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Read data
rats.data = read.table("http://www.stat.ufl.edu/~aa/glm/data/Rats.dat", header = T)
rats.data[,"group"] = as.factor(rats.data[,"group"])
# Consider two groups: placebo or treated
rats.data[,"placebo"] = ifelse(rats.data[,"group"] == 1, 1, 0)
head(rats.data)

# Fit quasi likelihood model with "group" as only covariate.
QL.model.1 = glm(s/n ~ group, weights = n, family = quasi(link = "logit", variance = "mu(1-mu)"), data = rats.data)
summary(QL.model.1)

# Fit quasi likelihood model with "placebo" as only covariate.
QL.model.2 = glm(s/n ~ placebo, weights = n, family = quasi(link = "logit", variance = "mu(1-mu)"), data = rats.data)
summary(QL.model.2)

# F-test
anova(QL.model.2, QL.model.1, test = "F")
# We keep the null hypothesis and conclude that the covariate "group" can be replaced by the simplified variable "placebo".
