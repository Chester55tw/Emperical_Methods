##Q3 1
N=10000
betahat = rep(0,N)
a = rep(0,N)
for (i in 1:N){
  r1= rnorm(600,0.006,0.05)
  r2 = rnorm(600,0.006,0.05) 
  betahat[i] = coefficients(lm(r1~r2))[2]
  a[i] = coefficients(lm(r1~r2))[1]
}
u = mean(betahat)
std = sd(betahat)
rhat <- r2 * betahat+a # fitted values
ehat <- r1 - rhat
n <- length(r1)
k <- 1
xxi <- solve(crossprod(r2))
s2 <- (1/(n-k)) * t(ehat) %*% ehat
s2 <- as.vector(s2)
V0 <- s2*xxi
s_beta <- sqrt(diag(V0))
t_stat = (betahat - 0)/s_beta
hist(t_stat)
hist(betahat)

##Q3 2
N=10000
betahat = rep(0,N)
a = rep(0,N)
for (i in 1:N){
  p1 = rep(0,600)
  p2 = rep(0,600)
  r1 = rnorm(599,0.006,0.05)
  r2 = rnorm(599,0.006,0.05)
  for (t in 1:599){
    p1[t+1] = p1[t]+r1[t]
    p2[t+1] = p2[t] +r2[t]
  }
  betahat[i] = coefficients(lm(r1~r2))[2]
  a[i] = coefficients(lm(r1~r2))[1]
}
u = mean(betahat)
std = sd(betahat)
phat <- p2* betahat+a # fitted values
ehat <- p1 - phat
n <- length(p1)
k <- 1
xxi <- solve(crossprod(r2))
s2 <- (1/(n-k)) * t(ehat) %*% ehat
s2 <- as.vector(s2)
V0 <- s2*xxi
s_beta <- sqrt(diag(V0))
t_stat = (betahat - 0)/s_beta
hist(t_stat)
hist(betahat)
