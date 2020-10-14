
# STK 3100, fall 2017
# Exercise 2.22

# Clean up the memory before we start.
rm(list=ls(all=TRUE))

y = c(5,10)
X = matrix(1, nrow = 2, ncol = 1)

hat.matrix = X%*%solve(t(X)%*%X)%*%t(X)
beta.vec = solve(t(X)%*%X)%*%t(X)%*%y
mu.vec = X%*%solve(t(X)%*%X)%*%t(X)%*%y

sum.of.squares.term.1 = sum(y^2)
sum.of.squares.term.2 = length(y)*mean(y)^2
sum.of.squares.term.3 = sum((y - mean(y))^2)

s.y = sd(y)
