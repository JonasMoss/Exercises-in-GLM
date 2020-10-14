
# STK 3100, fall 2017
# Additional Exercise 11

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# (a)
# Read data directly from the web.
LBW = read.table("http://www.uio.no/studier/emner/matnat/math/STK3100/data/lowbirthweight.txt", header = T)
LBW[,"bwt"] = NULL
head(LBW)
summary(LBW)

# Categorical variables should be factors.
LBW[,"race"] = as.factor(LBW[,"race"])
LBW[,"low"] = as.factor(LBW[,"low"])
LBW[,"smoke"] = as.factor(LBW[,"smoke"])
LBW[,"ht"] = as.factor(LBW[,"ht"])
LBW[,"ui"] = as.factor(LBW[,"ui"])

# Fit logistic regression:
LBW.model.1 = glm(low~age+lwt+race+smoke+ht+ui+ftv+ptl, family = binomial(link = "logit"), data = LBW)
summary(LBW.model.1)


# (b)
# Significance level
alpha = 0.05

# Starting value
i = 1
LRT.result = as.data.frame(drop1(LBW.model.1, test = "LRT"))
LRT.result = LRT.result [-1,]
new.model = LBW.model.1

# Remove non-significant variables, one by one.
while(max(LRT.result[,"Pr(>Chi)"]) > alpha){

  # Inspect which variable has the highest p-value.
  most.insig.row = which.max(LRT.result[,"Pr(>Chi)"])
  most.insig.var = rownames(LRT.result)[most.insig.row]
  
  # Remove the concerning variable from the model.
  old.formula = new.model$formula
  new.formula = update(old.formula, paste(".~. -", most.insig.var))
  new.model = glm(formula = new.formula, family = binomial(link = "logit"), data = LBW)
  # Print message
  cat("itr = ", i, "\n", sep = "")
  cat("'", most.insig.var,"' with p = ", max(LRT.result[,"Pr(>Chi)"]), " is removed from the model.", "\n", sep = "")
  
  # Check whether there is still a non-significant variable
  LRT.result = as.data.frame(drop1(new.model, test = "LRT"))
  LRT.result = LRT.result [-1,]
  
  # Update iteration
  i = i + 1
}

LBW.model.2 = new.model


# (c)
summary(LBW.model.2)

