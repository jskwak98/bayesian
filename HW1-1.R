## 3.1 - (a) ##

rm(list=ls())

theta_likelihood <- function(observation, theta){
  likelihood = choose(100, observation) * theta^observation * (1-theta)^(100-observation)
  return(likelihood)
}


## 3.1 - (b) ##
theta <- seq(0, 1, 0.01)
probs <- rep(0, length(theta))
for(i in 1:length(theta)){
  probs[i] <- theta_likelihood(57, theta[i])
}
plot(theta, probs, main="Probability sum(Y)=57 given theta", xlab="theta", ylab="probability",
     type='o')


## 3.1 - (c) ##
posterior <- rep(0, length(theta))
for(i in 1:length(theta)){
  posterior[i] <- probs[i]/sum(probs)
}
plot(theta, posterior, main="Posterior given theta", xlab="theta", ylab="posterior",
     type='o')


## 3.1 - (d) ##
uniform_theta <- runif(30)
uniform_theta <- sort(uniform_theta)
plot(uniform_theta, 1 * theta_likelihood(57, uniform_theta), type='o')


## 3.1 - (e) ##
plot(theta, dbeta(theta, 58, 44), type='o')



## 3.2 ##
rm(list=ls())
library(reshape2)

posterior_dist <- function(prior_n, prior_theta){
  a = prior_n * prior_theta
  b = (1-prior_theta) * prior_n
  return(1-pbeta(0.5, a+57, b+100-57))
}

theta_0 = seq(0.1, 0.9, 0.1)
n_0 = c(1, 2, 8, 16, 32)
data = outer(n_0, theta_0,FUN = posterior_dist)
rownames(data) = n_0
colnames(data) = theta_0
melt_data = melt(data, varnames=c("n_0", "theta_0"))

persp(n_0, theta_0, data)
contour(n_0, theta_0, data)
points(melt_data[,1], melt_data[,2], pch='*')



## 3.4 - (a) ##
rm(list=ls())


n <- 43; x <- 15; a <- 2; b <- 8
grid <- seq(0,1,.01)
likelihood <- dbinom(x,n,grid)
likelihood <- likelihood/sum(likelihood) #scaled
prior_a <- dbeta(grid,a,b)
prior_a <- prior_a/sum(prior_a)
post <- likelihood*prior_a
post <- post/sum(post)

plot(grid,likelihood,type="l",lty=2,col=1,xlab="theta",ylab="Density", ylim=c(0.00, 0.07))
lines(grid,prior_a,col="blue")
lines(grid,post,lwd=2,col="red")
legend("topright",c("Likelihood","Prior","Posterior"),
       lwd=c(1,1,2),lty=c(2,1,1),col=c(1,"blue","red"))

#posterior가 beta(a+x, b+n-x) 나옴을 설명할것, beta(17, 36)

posterior_mean = (a+x)/(a+b+n)
posterior_std = sqrt((a+x)*(b+n-x)/(((a+b+n)^2)*(a+b+n+1)))
posterior_mode = (x+a-1)/(n+a+b-2)

posterior_mean
posterior_std
posterior_mode


## 3.4-(b) ##

n <- 43; x <- 15; a <- 8; b <- 2
grid <- seq(0,1,.01)
likelihood <- dbinom(x,n,grid)
likelihood <- likelihood/sum(likelihood) #scaled
prior_b <- dbeta(grid,a,b)
prior_b <- prior_b/sum(prior_b)
post <- likelihood*prior_b
post <- post/sum(post)

plot(grid,likelihood,type="l",lty=2,col=1,xlab="theta",ylab="Density", ylim=c(0.00, 0.07))
lines(grid,prior_b,col="blue")
lines(grid,post,lwd=2,col="red")
legend("topright",c("Likelihood","Prior","Posterior"),
       lwd=c(1,1,2),lty=c(2,1,1),col=c(1,"blue","red"))

#posterior가 beta(a+x, b+n-x) 나옴을 설명할것, beta(17, 36)

posterior_mean = (a+x)/(a+b+n)
posterior_std = sqrt((a+x)*(b+n-x)/(((a+b+n)^2)*(a+b+n+1)))
posterior_mode = (x+a-1)/(n+a+b-2)

posterior_mean
posterior_std
posterior_mode

## 3.4-(c) ##

prior_mixture = 0.75*prior_a + 0.25*prior_b
plot(grid,prior_mixture, type="l", lwd=2.5, col="purple",xlab="theta",ylab="Density", ylim=c(0.00, 0.07))
lines(grid,prior_a, lwd=1, col="blue")
lines(grid,prior_b,lwd=1, col="red")
legend("topright",c("Mixture","Prior Beta(2,8)","Prior Beta(8,2)"),
       lwd=c(2.5,1,1),lty=c(1,1,1),col=c("purple","blue","red"))

# 75%는 낮은 재범률, 25%는 높은 재범률


## 3-4-(d) ##


posterior_mixture = prior_mixture * likelihood
posterior_mixture = posterior_mixture/sum(posterior_mixture)
plot(grid,posterior_mixture, type="l", lwd=2.5, col=1, xlab="theta", ylab="Density", ylim=c(0.00, 0.07))

grid[which.max(posterior_mixture)]
# 0.3, 0.43. 0.31

## e -> 손으로 prior * likelihood 해주고, Beta 분포꼴로 만들어주기 위한 상수를
## 위 아래로 곱해서 맞춰준다. 이후 weight를 구한다.


## 3-10 (a) ##


p_psi <- function(a, b, psi){
  return((1/beta(a,b)) * (exp(psi)/(exp(psi)+1))^a * (1/(exp(psi)+1))^b)
}

psi <- seq(-17, 17, 0.1)
dist_p_psi <- p_psi(1, 1, psi)
plot(psi, dist_p_psi, type='l', xlab="Psi", ylab="Density", lwd=2)









