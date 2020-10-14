
# STK 3100, fall 2017
# Additional Exercise 15

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# (a)
# Read data.
Beetle = read.table("http://www.stat.ufl.edu/~aa/glm/data/Beetles2.dat", header = T)
head(Beetle)

# Fit logistic regression
Beetle.model.1 = glm(cbind(dead,n-dead) ~ logdose, family = binomial, data = Beetle)
summary(Beetle.model.1)

# Estimation of LD50
LD50 = -as.numeric(Beetle.model.1$coef[1])/as.numeric(Beetle.model.1$coef[2])
show(LD50)

# 95% confidence interval of LD50
alpha = 0.05
z.value = qnorm(1 - alpha/2)

beta.hat = as.numeric(Beetle.model.1$coeff)
beta.hat.cov.mat = vcov(Beetle.model.1) 

a.val = beta.hat[2]^2 - (z.value^2)*beta.hat.cov.mat[2,2]
b.val = 2*beta.hat[1]*beta.hat[2] - 2*(z.value^2)*beta.hat.cov.mat[1,2]
c.val = beta.hat[1]^2 - (z.value^2)*beta.hat.cov.mat[1,1]

LD50.CI95 = c(
  (-b.val -sqrt(b.val^2 -4*a.val*c.val))/(2*a.val),
  (-b.val +sqrt(b.val^2 -4*a.val*c.val))/(2*a.val)
)
show(LD50.CI95)

