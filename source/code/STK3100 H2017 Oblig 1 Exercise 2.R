
# STK 3100, fall 2017
# Oblig 1
setwd("C:/Users/Vinnie/Dropbox/RCode/STK3100/Oblig 1")
# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Exercise 2
# (e)
# Significance level
alpha = 0.05
n = 100
y = 30
pi.null = 0.5
pi.hat = y/n

# Frame to write down the result.
Null.hypothesis.test.result = as.data.frame(matrix(NA, nrow = 3, ncol = 6))
rownames(Null.hypothesis.test.result) = c("Wald", "Score", "LR")
colnames(Null.hypothesis.test.result) = c("Z","Z^2","p.value.Z","p.value.Z^2","nh.Z.rejected","nh.Z^2.rejected")

# i) Wald test
# Wald test statistic
Null.hypothesis.test.result["Wald","Z"] = (pi.hat - pi.null)/(sqrt(pi.hat*(1-pi.hat)/n))
Null.hypothesis.test.result["Wald","Z^2"] = (Null.hypothesis.test.result["Wald","Z"])^2
# Compute p-value (standard Normal)
Null.hypothesis.test.result["Wald","p.value.Z"] = 2*(1 - pnorm(abs(Null.hypothesis.test.result["Wald","Z"])))
# Compute p-value (Chi-squared)
Null.hypothesis.test.result["Wald","p.value.Z^2"] = 1 - pchisq(Null.hypothesis.test.result["Wald","Z^2"], df = 1)
# Null hypothesis testing (standard Normal)
Null.hypothesis.test.result["Wald","nh.Z.rejected"] = as.numeric(Null.hypothesis.test.result["Wald","p.value.Z"] < 1 - alpha)
# Null hypothesis testing (Chi-squared)
Null.hypothesis.test.result["Wald","nh.Z^2.rejected"] = as.numeric(Null.hypothesis.test.result["Wald","p.value.Z^2"] < 1 - alpha)
# Alternative (standard normal)
#critical.value = qnorm(1-alpha)
#Null.hypothesis.test.result["Wald","nh.Z.rejected"] = as.numeric(abs(Null.hypothesis.test.result["Wald","Z"]) > critical.value)

# ii) Score test
# Score test statistic
Null.hypothesis.test.result["Score","Z"] = (pi.hat - pi.null)/sqrt(pi.null*(1 - pi.null)/n)
Null.hypothesis.test.result["Score","Z^2"] = (Null.hypothesis.test.result["Score","Z"])^2
# Compute p-value (standard Normal)
Null.hypothesis.test.result["Score","p.value.Z"] = 2*(1 - pnorm(abs(Null.hypothesis.test.result["Score","Z"])))
# Compute p-value (Chi-squared)
Null.hypothesis.test.result["Score","p.value.Z^2"] = 1 - pchisq(Null.hypothesis.test.result["Score","Z^2"], df = 1)
# Null hypothesis testing (standard Normal)
Null.hypothesis.test.result["Score","nh.Z.rejected"] = as.numeric(Null.hypothesis.test.result["Score","p.value.Z"] < 1 - alpha)
# Null hypothesis testing (Chi-squared)
Null.hypothesis.test.result["Score","nh.Z^2.rejected"] = as.numeric(Null.hypothesis.test.result["Score","p.value.Z^2"] < 1 - alpha)
# Alternative (standard normal)
#critical.value = qnorm(1-alpha)
#Null.hypothesis.test.result["Score","nh.Z.rejected"] = as.numeric(abs(Null.hypothesis.test.result["Score","Z"]) > critical.value)


# iii) Likelihood ratio test
# Likelihood ratio test statistic
Null.hypothesis.test.result["LR","Z^2"] = 2*(y*log(pi.hat/pi.null) + (n-y)*log((1-pi.hat)/(1-pi.null)))
Null.hypothesis.test.result["LR","Z"] = sqrt(Null.hypothesis.test.result["LR","Z^2"])
# Compute p-value (standard Normal)
Null.hypothesis.test.result["LR","p.value.Z"] = 2*(1 - pnorm(abs(Null.hypothesis.test.result["LR","Z"])))
# Compute p-value (Chi-squared)
Null.hypothesis.test.result["LR","p.value.Z^2"] = 1 - pchisq(Null.hypothesis.test.result["LR","Z^2"], df = 1)
# Null hypothesis testing (standard Normal)
Null.hypothesis.test.result["LR","nh.Z.rejected"] = as.numeric(Null.hypothesis.test.result["LR","p.value.Z"] < 1 - alpha)
# Null hypothesis testing (Chi-squared)
Null.hypothesis.test.result["LR","nh.Z^2.rejected"] = as.numeric(Null.hypothesis.test.result["LR","p.value.Z^2"] < 1 - alpha)
# Alternative (Chi-squared)
#critical.value = qchisq(1-alpha, df = 1)
#Null.hypothesis.test.result["LR","nh.Z^2.rejected"] = as.numeric(Null.hypothesis.test.result["LR","Z^2"] > critical.value)

# Report the result.
Null.hypothesis.test.result



# (f)
# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Significance level
alpha = 0.05
n = 100
y = 5
pi.null = 0.15
pi.hat = y/n

# Frame to write down the result.
Null.hypothesis.test.result = as.data.frame(matrix(NA, nrow = 3, ncol = 6))
rownames(Null.hypothesis.test.result) = c("Wald", "Score", "LR")
colnames(Null.hypothesis.test.result) = c("Z","Z^2","p.value.Z","p.value.Z^2","nh.Z.rejected","nh.Z^2.rejected")

# i) Wald test
# Wald test statistic
Null.hypothesis.test.result["Wald","Z"] = (pi.hat - pi.null)/(sqrt(pi.hat*(1-pi.hat)/n))
Null.hypothesis.test.result["Wald","Z^2"] = (Null.hypothesis.test.result["Wald","Z"])^2
# Compute p-value (standard Normal)
Null.hypothesis.test.result["Wald","p.value.Z"] = 2*(1 - pnorm(abs(Null.hypothesis.test.result["Wald","Z"])))
# Compute p-value (Chi-squared)
Null.hypothesis.test.result["Wald","p.value.Z^2"] = 1 - pchisq(Null.hypothesis.test.result["Wald","Z^2"], df = 1)
# Null hypothesis testing (standard Normal)
Null.hypothesis.test.result["Wald","nh.Z.rejected"] = as.numeric(Null.hypothesis.test.result["Wald","p.value.Z"] < 1 - alpha)
# Null hypothesis testing (Chi-squared)
Null.hypothesis.test.result["Wald","nh.Z^2.rejected"] = as.numeric(Null.hypothesis.test.result["Wald","p.value.Z^2"] < 1 - alpha)
# Alternative (standard normal)
#critical.value = qnorm(1-alpha)
#Null.hypothesis.test.result["Wald","nh.Z.rejected"] = as.numeric(abs(Null.hypothesis.test.result["Wald","Z"]) > critical.value)

# ii) Score test
# Score test statistic
Null.hypothesis.test.result["Score","Z"] = (pi.hat - pi.null)/sqrt(pi.null*(1 - pi.null)/n)
Null.hypothesis.test.result["Score","Z^2"] = (Null.hypothesis.test.result["Score","Z"])^2
# Compute p-value (standard Normal)
Null.hypothesis.test.result["Score","p.value.Z"] = 2*(1 - pnorm(abs(Null.hypothesis.test.result["Score","Z"])))
# Compute p-value (Chi-squared)
Null.hypothesis.test.result["Score","p.value.Z^2"] = 1 - pchisq(Null.hypothesis.test.result["Score","Z^2"], df = 1)
# Null hypothesis testing (standard Normal)
Null.hypothesis.test.result["Score","nh.Z.rejected"] = as.numeric(Null.hypothesis.test.result["Score","p.value.Z"] < 1 - alpha)
# Null hypothesis testing (Chi-squared)
Null.hypothesis.test.result["Score","nh.Z^2.rejected"] = as.numeric(Null.hypothesis.test.result["Score","p.value.Z^2"] < 1 - alpha)
# Alternative (standard normal)
#critical.value = qnorm(1-alpha)
#Null.hypothesis.test.result["Score","nh.Z.rejected"] = as.numeric(abs(Null.hypothesis.test.result["Score","Z"]) > critical.value)


# iii) Likelihood ratio test
# Likelihood ratio test statistic
Null.hypothesis.test.result["LR","Z^2"] = 2*(y*log(pi.hat/pi.null) + (n-y)*log((1-pi.hat)/(1-pi.null)))
Null.hypothesis.test.result["LR","Z"] = sqrt(Null.hypothesis.test.result["LR","Z^2"])
# Compute p-value (standard Normal)
Null.hypothesis.test.result["LR","p.value.Z"] = 2*(1 - pnorm(abs(Null.hypothesis.test.result["LR","Z"])))
# Compute p-value (Chi-squared)
Null.hypothesis.test.result["LR","p.value.Z^2"] = 1 - pchisq(Null.hypothesis.test.result["LR","Z^2"], df = 1)
# Null hypothesis testing (standard Normal)
Null.hypothesis.test.result["LR","nh.Z.rejected"] = as.numeric(Null.hypothesis.test.result["LR","p.value.Z"] < 1 - alpha)
# Null hypothesis testing (Chi-squared)
Null.hypothesis.test.result["LR","nh.Z^2.rejected"] = as.numeric(Null.hypothesis.test.result["LR","p.value.Z^2"] < 1 - alpha)
# Alternative (Chi-squared)
#critical.value = qchisq(1-alpha, df = 1)
#Null.hypothesis.test.result["LR","nh.Z^2.rejected"] = as.numeric(Null.hypothesis.test.result["LR","Z^2"] > critical.value)

# Report the result.
Null.hypothesis.test.result



# (j)
y = 30
n = 100
pi.hat = y/n

# i) Wald test based confidence interval
term.1 = pi.hat
term.2 = 1.96*sqrt(pi.hat*(1-pi.hat)/n)
show(term.1)
show(term.2)
low.bound = term.1 - term.2
upp.bound = term.1 + term.2
show(low.bound)
show(upp.bound)

# ii) Score test based confidence interval
z.value = qnorm(0.975)
chi.value = qchisq(0.95, df = 1)
term.1 = (y+chi.value/2)/(n+chi.value)
term.2 = z.value*sqrt(y*(1-pi.hat)+chi.value/4)/(100+chi.value)
show(term.1)
show(term.2)
low.bound = term.1 - term.2
upp.bound = term.1 + term.2
show(low.bound)
show(upp.bound)

# iii) Likelihood ratio test based confidence interval
# Critical value
critical.value = qchisq(0.95, df = 1)
# Likelihood ratio statistic
Z.square.LR.func = function(pi.null)
  {
  n = 100
  y = 30
  pi.hat = y/n
  2*(y*log(pi.hat/pi.null) + (n-y)*log((1-pi.hat)/(1-pi.null)))
}
# pi.null values
pi.null = seq(0, 1, by = 0.0001)
# Z.square values
pi.null.Z.square.mat = data.frame(
  pi.null = pi.null,
  Z.square = Z.square.LR.func(pi.null)
)

# Plot
pdf("./Exercise_2_j.pdf", width = 10, height = 5)
par(mfrow=c(1,2))
# Prevent that ylab is cut out
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 0.3, 0, 0))
plot(x = pi.null.Z.square.mat[,"pi.null"], y = pi.null.Z.square.mat[,"Z.square"],
     type = "l",
     xlab = expression(paste(pi[0])),
     ylab = expression(paste({Z[LR]^{2}}(pi[0])))
     )
abline(h = critical.value, lty = 2, col = c("red"))


# It's not clearly visible. So, we zoom in.
# pi.null values
pi.null = seq(0.15, 0.45, by = 0.0001)
# Z.square values
pi.null.Z.square.mat = data.frame(
  pi.null = pi.null,
  Z.square = Z.square.LR.func(pi.null)
)

# Find the exact value of lower and upper bound
# Find lower bound
low.bound.ind = which.min(abs(pi.null.Z.square.mat[pi.null.Z.square.mat[,"pi.null"] < 0.3,"Z.square"] - critical.value))
low.bound.pi.null = pi.null.Z.square.mat[low.bound.ind,"pi.null"]
low.bound.Z.square = pi.null.Z.square.mat[low.bound.ind,"Z.square"]

# Find upper bound
upp.bound.ind = which.min(abs(pi.null.Z.square.mat[pi.null.Z.square.mat[,"pi.null"] > 0.3,"Z.square"] - critical.value)) +
  nrow(pi.null.Z.square.mat[pi.null.Z.square.mat[,"pi.null"] <= 0.3,])
upp.bound.pi.null = pi.null.Z.square.mat[upp.bound.ind,"pi.null"]
upp.bound.Z.square = pi.null.Z.square.mat[upp.bound.ind,"Z.square"]

# Plot
plot(x = pi.null.Z.square.mat[,"pi.null"], y = pi.null.Z.square.mat[,"Z.square"],
     type = "l",
     xlab = expression(paste(pi[0])),
     ylab = expression(paste({Z[LR]^{2}}(pi[0])))
)
abline(h = critical.value, lty = 2, col = c("red"))
# Mark it on the plot.
abline(v = low.bound.pi.null, lty = 2, col = c("blue"))
abline(v = upp.bound.pi.null, lty = 2, col = c("blue"))
dev.off()

# 95% confidence interval of pi.null
pi.null.CI95 = c(low.bound.pi.null, upp.bound.pi.null)
show(pi.null.CI95)



# (k)
y = 5
n = 100
pi.hat = y/n

# i) Wald test based confidence interval
term.1 = pi.hat
term.2 = 1.96*sqrt(pi.hat*(1-pi.hat)/n)
show(term.1)
show(term.2)
low.bound = term.1 - term.2
upp.bound = term.1 + term.2
show(low.bound)
show(upp.bound)

# ii) Score test based confidence interval
z.value = qnorm(0.975)
chi.value = qchisq(0.95, df = 1)
term.1 = (y+chi.value/2)/(n+chi.value)
term.2 = z.value*sqrt(y*(1-pi.hat)+chi.value/4)/(100+chi.value)
show(term.1)
show(term.2)
low.bound = term.1 - term.2
upp.bound = term.1 + term.2
show(low.bound)
show(upp.bound)

# iii) Likelihood ratio test based confidence interval
# Critical value
critical.value = qchisq(0.95, df = 1)
# Likelihood ratio statistic
Z.square.LR.func = function(pi.null)
{
  n = 100
  y = 5
  pi.hat = y/n
  2*(y*log(pi.hat/pi.null) + (n-y)*log((1-pi.hat)/(1-pi.null)))
}
# pi.null values
pi.null = seq(0, 1, by = 0.0001)
# Z.square values
pi.null.Z.square.mat = data.frame(
  pi.null = pi.null,
  Z.square = Z.square.LR.func(pi.null)
)

# Plot
pdf("./Exercise_2_k.pdf", width = 10, height = 5)
par(mfrow=c(1,2))
# Prevent that ylab is cut out
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 0.3, 0, 0))
plot(x = pi.null.Z.square.mat[,"pi.null"], y = pi.null.Z.square.mat[,"Z.square"],
     type = "l",
     xlab = expression(paste(pi[0])),
     ylab = expression(paste({Z[LR]^{2}}(pi[0])))
)
abline(h = critical.value, lty = 2, col = c("red"))


# It's not clearly visible. So, we zoom in.
# pi.null values
pi.null = seq(0.0, 0.15, by = 0.0001)
# Z.square values
pi.null.Z.square.mat = data.frame(
  pi.null = pi.null,
  Z.square = Z.square.LR.func(pi.null)
)

# Find the exact value of lower and upper bound
# Find lower bound
low.bound.ind = which.min(abs(pi.null.Z.square.mat[pi.null.Z.square.mat[,"pi.null"] < 0.05,"Z.square"] - critical.value))
low.bound.pi.null = pi.null.Z.square.mat[low.bound.ind,"pi.null"]
low.bound.Z.square = pi.null.Z.square.mat[low.bound.ind,"Z.square"]

# Find upper bound
upp.bound.ind = which.min(abs(pi.null.Z.square.mat[pi.null.Z.square.mat[,"pi.null"] > 0.05,"Z.square"] - critical.value)) +
  nrow(pi.null.Z.square.mat[pi.null.Z.square.mat[,"pi.null"] <= 0.05,])
upp.bound.pi.null = pi.null.Z.square.mat[upp.bound.ind,"pi.null"]
upp.bound.Z.square = pi.null.Z.square.mat[upp.bound.ind,"Z.square"]

# Plot
plot(x = pi.null.Z.square.mat[,"pi.null"], y = pi.null.Z.square.mat[,"Z.square"],
     type = "l",
     xlab = expression(paste(pi[0])),
     ylab = expression(paste({Z[LR]^{2}}(pi[0])))
)
abline(h = critical.value, lty = 2, col = c("red"))
# Mark it on the plot.
abline(v = low.bound.pi.null, lty = 2, col = c("blue"))
abline(v = upp.bound.pi.null, lty = 2, col = c("blue"))
dev.off()

# 95% confidence interval of pi.null
pi.null.CI95 = c(low.bound.pi.null, upp.bound.pi.null)
show(pi.null.CI95)



# (n)
n = 100
y = 5
critical.value = qnorm(0.975)
# 95% confidence interval of theta
theta.low.bound = log(y/(n-y)) - critical.value*sqrt(1/y + 1/(n-y))
theta.upp.bound = log(y/(n-y)) + critical.value*sqrt(1/y + 1/(n-y))
# 95% confidence interval of pi
pi.low.bound = exp(theta.low.bound)/(1 + exp(theta.low.bound))
pi.upp.bound = exp(theta.upp.bound)/(1 + exp(theta.upp.bound))
c(pi.low.bound, pi.upp.bound)



