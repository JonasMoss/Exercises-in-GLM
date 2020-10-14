
# STK 3100, fall 2017
# Exercise 6.18

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Load package VGAM
library(VGAM)

# Read aligator data from the web:
alligators = read.table("http://www.stat.ufl.edu/~aa/glm/data/Alligators2.dat", header = T)
alligators[,"lake"] = as.factor(alligators[,"lake"])
alligators[,"gender"] = as.factor(alligators[,"gender"])
alligators[,"size"] = as.factor(alligators[,"size"])
head(alligators)

# Multinomial logit model with fish (y1) as the reference category
# With 3 explanatory variables: size, gender, lake
model.size.gender.lake = vglm(cbind(y2,y3,y4,y5,y1) ~ size + gender + lake,
                              family = multinomial, data = alligators)
summary(model.size.gender.lake)


# Built-in solution for likelihood ratio test doesn't work for vglm().
anova.vgam(model.gender.lake, model.size.gender.lake, test = "LRT")

# Likelihood ratio test
LR.test = function(model.1, model.2) {
  LR.test.stat = -2*(logLik(model.1) - logLik(model.2))
  df.diff = df.residual(model.1) - df.residual(model.2)
  p.value = 1 - pchisq(LR.test.stat, df.diff)
  return(list(test.stat = LR.test.stat, df.diff = df.diff, p.value = p.value))
}


# Variable selection
# Round 1)
# Test whether size has a significant effect
model.gender.lake = vglm(cbind(y2,y3,y4,y5,y1) ~ gender + lake,
                         family = multinomial, data = alligators)
LR.test(model.gender.lake, model.size.gender.lake)
# We reject the null hypothesis and keep size in the model.

# Test whether gender has a significant effect
model.size.lake = vglm(cbind(y2,y3,y4,y5,y1) ~ size + lake,
                       family = multinomial, data = alligators)
LR.test(model.size.lake, model.size.gender.lake)
# We keep the null hypothesis and remove gender from the model.

# Test whether lake has a significant effect
model.size.gender = vglm(cbind(y2,y3,y4,y5,y1) ~ size + gender,
                         family = multinomial, data = alligators)
LR.test(model.size.gender, model.size.gender.lake)
# We reject the null hypothesis and keep lake in the model.


# Round 2)
# Test whether size has a significant effect
model.lake = vglm(cbind(y2,y3,y4,y5,y1) ~ lake,
                         family = multinomial, data = alligators)
LR.test(model.lake, model.size.lake)
# We reject the null hypothesis and keep size in the model.

# Test whether lake has a significant effect
model.size = vglm(cbind(y2,y3,y4,y5,y1) ~ size,
                  family = multinomial, data = alligators)
LR.test(model.size, model.size.lake)
# We reject the null hypothesis and keep lake in the model.

# Final model
final.model = model.size.lake
summary(final.model)
