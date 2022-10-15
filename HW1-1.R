## 3.1 - (a) ##


## 3.1 - (b) ##
theta <- seq(0, 1, 0.01)
probs <- rep(0, length(theta))
for(i in 1:length(theta)){
  probs[i] <- dbinom(57, 100, theta[i])
}
plot(theta, probs, main="Probability sum(Y)=57 given theta", xlab="theta", ylab="probability",
     pch="@")


## 3.1 - (c) ##
posterior <- rep(0, length(theta))
for(i in 1:length(theta)){
  posterior[i] <- probs[i]/sum(probs)
}
plot(theta, posterior, main="Posterior given theta", xlab="theta", ylab="posterior",
     pch="@")


## 3.1 - (d) ##
uniform_theta <- runif(30)
plot(uniform_theta, 1 * dbinom(57, 100, uniform_theta))




## 3.1 - (e) ##
plot(theta, dbeta(theta, 58, 44))
