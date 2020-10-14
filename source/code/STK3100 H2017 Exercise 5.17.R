
# STK 3100, fall 2017
# Exercise 5.17

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# (a)
# Create data
n.i = 4
data.1 = data.frame(
  x = c(rep(0, n.i), rep(1, n.i), rep(2, n.i)),
  n = 1,
  y = c(0,0,0,1,0,0,1,1,1,1,1,1)
)

data.2 = data.frame(
  x = c(0,1,2),
  n = 4,
  y = c(1,2,4)
)

show(data.1)
show(data.2)

# Fit M.0 with 2 different data forms.
M.0.data.1 = glm(y ~ 1, family = binomial(link = "logit"), data = data.1)
M.0.data.2 = glm(cbind(y, n-y) ~ 1, family = binomial(link = "logit"), data = data.2)
summary(M.0.data.1)
summary(M.0.data.2)

# Fit M.1 with 2 different data forms.
M.1.data.1 = glm(y ~ x, family = binomial(link = "logit"), data = data.1)
M.1.data.2 = glm(cbind(y, n-y) ~ x, family = binomial(link = "logit"), data = data.2)
summary(M.1.data.1)
summary(M.1.data.2)

deviance.table = as.data.frame(matrix(NA, nrow = 2, ncol = 2))
rownames(deviance.table) = c("M.0", "M.1")
colnames(deviance.table) = c("data.1", "data.2")
deviance.table[1,1] = M.0.data.1$deviance
deviance.table[1,2] = M.0.data.2$deviance
deviance.table[2,1] = M.1.data.1$deviance
deviance.table[2,2] = M.1.data.2$deviance
show(deviance.table)

logLik.table = as.data.frame(matrix(NA, nrow = 2, ncol = 2))
rownames(logLik.table) = c("M.0", "M.1")
colnames(logLik.table) = c("data.1", "data.2")
logLik.table[1,1] = logLik(M.0.data.1)
logLik.table[1,2] = logLik(M.0.data.2)
logLik.table[2,1] = logLik(M.1.data.1) 
logLik.table[2,2] = logLik(M.1.data.2)
show(logLik.table)

kernel.logLik.table = as.data.frame(matrix(NA, nrow = 2, ncol = 2))
rownames(kernel.logLik.table) = c("M.0", "M.1")
colnames(kernel.logLik.table) = c("data.1", "data.2")
kernel.logLik.table[1,1] = logLik(M.0.data.1)
kernel.logLik.table[1,2] = logLik(M.0.data.2) - (lchoose(4, 1) + lchoose(4, 2) + lchoose(4, 4))
kernel.logLik.table[2,1] = logLik(M.1.data.1)
kernel.logLik.table[2,2] = logLik(M.1.data.2) - (lchoose(4, 1) + lchoose(4, 2) + lchoose(4, 4))
show(kernel.logLik.table)


# (b)
deviance.table["M.0","data.1"] - deviance.table["M.1","data.1"]
deviance.table["M.0","data.2"] - deviance.table["M.1","data.2"]

