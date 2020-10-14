
# STK 3100, fall 2017
# Exercise 1.18.b

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

# Number of categories in a categorical variable.
n.cat = 4

# Model matrix when the first category is the reference category.
X = diag(n.cat)
X.1 = cbind(1, rbind(0, X))
# Model matrix when the last category is the reference category.
X.c = cbind(1, rbind(X, 0))

# Transformation matrix for the front side.
G.front = X.1%*%solve(t(X.c)%*%X.c)%*%t(X.c)
G.front = round(G.front, digits = 10)
# Transformation matrix for the back side
G.back = t(X.c)%*%solve(X.c%*%t(X.c))%*%X.1
G.back = round(G.back, digits = 10)

# Display the obtained matrices.
show(G.front)
show(G.back)

# Check whether the created matrices do their job.
identical(X.1, G.front%*%X.c)
identical(X.1, X.c%*%G.back)
