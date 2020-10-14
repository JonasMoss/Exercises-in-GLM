
# STK 3100, fall 2017
# Exercise 8.14

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Load packages
library(MASS)

# Read data
homicide.data = read.table("http://www.stat.ufl.edu/~aa/glm/data/Homicides.dat", header = T)
homicide.data[,"race"] = as.factor(homicide.data[,"race"])
head(homicide.data)
table(homicide.data[,"count"], homicide.data[,"race"])

# Fit Poisson model
Poisson.model = glm(count ~ race, family = poisson, data = homicide.data)
summary(Poisson.model)

# Fit negative binomial model
negbin.model = glm.nb(count ~ race, data = homicide.data)
summary(negbin.model)

# Quasi likelihood approach.
QL.model = glm(count ~ race, family = quasi(link = "log", variance = "mu"), data = homicide.data)
summary(QL.model)
