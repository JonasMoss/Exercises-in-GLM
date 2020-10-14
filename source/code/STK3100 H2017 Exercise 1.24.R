
# STK 3100, fall 2017
# Exercise 1.24

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Read data directly from the web.
Anorexia = read.table("http://www.stat.ufl.edu/~aa/glm/data/Anorexia.dat", header = T, skip = 3)
# A sneak peak of the data.
head(Anorexia)
# Check for missing data and so on.
summary(Anorexia)
# Check whether the variable "therapy" is properly recognized as categorical variable.
is.factor(Anorexia$therapy)

# Set "control group" as reference.
Anorexia[,"therapy"] = relevel(Anorexia[,"therapy"], ref = "c")

# Fit linear model.
lm.fit = lm(after ~ before + therapy, data = Anorexia)
# Summary of the fitted linear model.
summary(lm.fit)
