
# STK 3100, fall 2017
# Additional Exercise 18

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# (a)
# Read data.
Car = read.table("http://www.uio.no/studier/emner/matnat/math/STK3100/data/car.txt", header = T, sep = ",")
Car = Car[Car[,"claimcst0"]>0,]
Car[,"agecat"] = as.factor(Car[,"agecat"])
Car[,"gender"] = as.factor(Car[,"gender"])
Car[,"area"] = as.factor(Car[,"area"])
rownames(Car) = NULL
head(Car)

# Fit GLM
Car.model.1 = glm(claimcst0~agecat+gender+area, family = inverse.gaussian(link = "log"), data = Car)
summary(Car.model.1)


# (b)
# Wald test
summary(Car.model.1)
# Likelihood ratio test
Car.model.2 = glm(claimcst0~agecat+area, family = inverse.gaussian(link = "log"), data = Car)
anova(Car.model.2, Car.model.1, test = "LRT")
# Alternatively, you can use drop1().
drop1(Car.model.1, test = "LRT")


# (c)
# Wald test
summary(Car.model.1)
# Likelihood ratio test
Car.model.3 = glm(claimcst0~gender+area, family = inverse.gaussian(link = "log"), data = Car)
anova(Car.model.3, Car.model.1, test = "LRT")
# Alternatively, you can use drop1().
drop1(Car.model.1, test = "LRT")


