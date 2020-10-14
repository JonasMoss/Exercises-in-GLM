
# STK 3100, fall 2017
# Exercise 1.21

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Read data directly from the web.
FEV = read.table("http://www.stat.ufl.edu/~aa/glm/data/FEV.dat", header = T)
# A sneak peak of the data.
head(FEV)
# Check for missing data and so on.
summary(FEV)
# Check whether the variable "drug" is properly recognized as categorical variable.
is.factor(FEV$drug)

# Fit linear model.
lm.fit = lm(fev1 ~ base + drug + base*drug, data = FEV)
# Summary of the fitted linear model.
summary(lm.fit)
