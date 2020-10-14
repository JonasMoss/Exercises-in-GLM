
# STK 3100, fall 2017
# Exercise 8.17

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Read data
basketball.data = read.table("http://www.stat.ufl.edu/~aa/glm/data/Basketball.dat", header = T)
head(basketball.data)


# a)
# Fit logistic regression (binomial)
binomial.model = glm(cbind(made, attempts - made) ~ 1, family = binomial(link = "logit"), data = basketball.data)
summary(binomial.model)


# b)
# Possible cause of overdispersion: The player's performance can vary from game to game.
# Fit quasi likelihood model.
QL.model = glm(made/attempts ~ 1, weights = attempts, family = quasi(link = "logit", variance = "mu(1-mu)"), data = basketball.data)
summary(QL.model)

# Compare confidence intervals
confint.default(binomial.model)
confint.default(QL.model)
