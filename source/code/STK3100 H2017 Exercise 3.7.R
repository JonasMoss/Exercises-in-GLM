
# STK 3100, fall 2017
# Exercise 3.7

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# (b)
alpha = 0.05
c.val = 3
n = c(10, 30, 50)
lambda = n/2

df.1 = c.val - 1
df.2 = c.val*n - c.val

critic.val = qf(1 - alpha, df.1 , df.2) # 0.95 quantile of F dist
power.val = 1 - pf(critic.val, df.1, df.2, lambda) # right-tail prob. for noncentral F

result.mat = data.frame(n = n, critic.val = critic.val, power.val = power.val)
show(result.mat)


# (c)
# Clean up the memory before we start.
rm(list=ls(all=TRUE))

alpha = 0.05
c.val = 3
n = 10
Delta = c(0, 0.5, 1)
lambda = 2*n*Delta^2

df.1 = c.val - 1
df.2 = c.val*n - c.val

critic.val = qf(1 - alpha, df.1 , df.2) # 0.95 quantile of F dist
power.val = 1 - pf(critic.val, df.1, df.2, lambda) # right-tail prob. for noncentral F

result.mat = data.frame(Delta = Delta, critic.val = critic.val, power.val = power.val)
show(result.mat)