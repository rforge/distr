require(distrEx)

### Why distrEx is useful ---  a convincing demonstration

N <- Norm(mean = 2, sd = 1.3)
P <- Pois(lambda = 1.2)
Z <- 2 * N + 3 + P # exact transformation

### examining what N, P, Z are:
plot(Z)
p(Z)(0.4)
q(Z)(0.3)
r(Z)(10)

## something weird
Znew <- sin(abs(Z)) # by simulations
plot(Znew)
p(Znew)(0.2)


####################################################################################
# example expectation operator
####################################################################################
require("distrEx")

D1 <- Norm(mean=2)
m1 <- E(D1)  # = 2
E(D1, function(x){ x^2 }) # E(D1^2)
# integrand with additional argument:
E(D1, function(x, m1){(x - m1)^2}, m1 = m1) # '$\Var$'
# same way
sd(D1);median(D1);mad(D1);IQR(D1)

## now same code but for Poisson:

D1 <- Pois(lambda=3)
m1 <- E(D1) # = 3
E(D1, function(x){ x^2 })
E(D1, function(x, m1){(x - m1)^2}, m1 = m1)
sd(D1);median(D1);mad(D1);IQR(D1)
