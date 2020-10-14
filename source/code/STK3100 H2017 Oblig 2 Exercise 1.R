
setwd("C:/Users/Vinnie/Dropbox/RCode/STK3100/Oblig 2")

# (c)
claims = read.table("http://www.uio.no/studier/emner/matnat/math/STK3100/data/claims.txt", header = T)
head(claims)
claims[,"alder"] = as.factor(claims[,"alder"])
claims[,"motorvolum"] = as.factor(claims[,"motorvolum"])
claims[,"distrikt"] = as.factor(claims[,"distrikt"])
# How to work with ordered factor?
#claims[,"alder"] = factor(claims[,"alder"], ordered = TRUE)

Poisson.model = Poisson.model = glm(antskader ~ offset(log(antforsikret)) + alder + motorvolum + distrikt, family = poisson, data = claims)
summary(Poisson.model)
# Model with all interactions
#Poisson.model = Poisson.model = glm(antskader ~ offset(log(antforsikret)) + (alder + motorvolum + distrikt)^3, family = poisson, data = claims)


# d)
# # Plot
# pdf("./Exercise_1.pdf", width = 10, height = 10)
# par(mfrow=c(2,2))
# plot(Poisson.model)
# dev.off()


# e)
par.fitted.val = summary(Poisson.model)$coefficients
par.CI = confint.default(Poisson.model)
par.fitted.val
par.CI
exp(par.CI)

# f)
x.new = data.frame(alder = as.factor(2), motorvolum = as.factor(3), distrikt = as.factor(4), antforsikret = 1)
eta.hat.fit = predict(Poisson.model, newdata = x.new, type = c("link"), se.fit = TRUE)
eta.hat.SE = eta.hat.fit$se.fit
eta.hat.var = (eta.hat.SE)^2
eta.hat = c(eta.hat.fit$fit-qnorm(0.975)*eta.hat.fit$se.fit, eta.hat.fit$fit, eta.hat.fit$fit+qnorm(0.975)*eta.hat.fit$se.fit)
names(eta.hat) = c("2.5%", "eta.hat", "97.5%")

y.hat.fit = predict(Poisson.model, newdata = x.new, type = c("response"), se.fit = TRUE)
y.hat.SE = y.hat.fit$se.fit
y.hat.var = (y.hat.SE)^2
y.hat = c(y.hat.fit$fit-qnorm(0.975)*y.hat.fit$se.fit, y.hat.fit$fit, y.hat.fit$fit+qnorm(0.975)*y.hat.fit$se.fit)
names(y.hat) = c("2.5%", "y.hat", "97.5%")

eta.hat
eta.hat.var
eta.hat.SE

y.hat
y.hat.var
y.hat.SE


# Manually compute variance of eta and corresponding confidence interval
cov.mat = summary(Poisson.model)$cov.scaled

eta.hat.var.manual = 
  cov.mat["(Intercept)","(Intercept)"] + 
  cov.mat["alder2","alder2"] +
  cov.mat["motorvolum3","motorvolum3"] +
  cov.mat["distrikt4","distrikt4"] +
  2*(
    cov.mat["(Intercept)","alder2"] +
      cov.mat["(Intercept)","motorvolum3"] +
      cov.mat["(Intercept)","distrikt4"] +
      cov.mat["alder2","motorvolum3"] +
      cov.mat["alder2","distrikt4"] +
      cov.mat["motorvolum3","distrikt4"]
  )
eta.hat.SE.manual = sqrt(eta.hat.var.manual)

eta.hat.var.manual
eta.hat.SE.manual

