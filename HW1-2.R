## 2-(a) ##

rm(list=ls())

xy_joint <- matrix(c(0.15, 0.15, 0.15, 0.15, 0.2, 0.2), 3, 2, byrow=F)
dimnames(xy_joint)[[1]]<-0:2
dimnames(xy_joint)[[2]]<-0:1
y_pmf <- apply(xy_joint, 2, sum)

rejection_sampling <- function(pmf){
  repeat{
    y <- rbinom(1, 1, 0.5)
    v <- runif(1)
    M <- max(pmf)/0.5
    if (v < pmf[y + 1] / (M * dbinom(y, 1, 0.5)))
      break
  }
  return(y)
}

nsim <- 10^4
set.seed(17)
rejection_sampled_Y <- replicate(nsim, rejection_sampling(y_pmf))
table(rejection_sampled_Y)/nsim
y_pmf


## 2-(b) ##

rm(list=ls())


xy_joint <- matrix(c(0.15, 0.15, 0.15, 0.15, 0.2, 0.2), 3, 2, byrow=F)
dimnames(xy_joint)[[1]]<-0:2
dimnames(xy_joint)[[2]]<-0:1

moc_discrete_sampling <- function(xy_joint, nsim){
  xy_sample_pairs <- matrix(0, nsim, 2)
  y_pmf <- apply(xy_joint, 2, sum)
  for (i in 1:nsim){
    y <- sample(0:1, 1, prob=y_pmf)
    x <- sample(0:2, 1, prob=xy_joint[,y+1]/y_pmf[y+1])
    xy_sample_pairs[i,] <- c(x, y)
  }
  return(xy_sample_pairs)
}

set.seed(17)
nsim <- 10^4
xy_sample <- data.frame(moc_discrete_sampling(xy_joint, nsim))
sample_joint <- matrix(table(xy_sample)/nsim, 3, 2)
sample_x_marginal <- matrix(apply(sample_joint, 1, sum),1,3)
x_marginal <- matrix(apply(xy_joint, 1, sum), 1, 3)
dimnames(sample_x_marginal)[[2]] <- 0:2
dimnames(sample_x_marginal)[[1]] <- "probs"
dimnames(x_marginal)[[2]] <- 0:2
dimnames(x_marginal)[[1]] <- "probs"
sample_x_marginal
x_marginal


## 2-(c) ##

rm(list=ls())

transition_matrix <- function(joint){
  n <- length(joint[,1])
  t_mat <- joint
  for (i in 1:n){
    t_mat[i,] <- joint[i,]/sum(joint[i,]) 
  }
  return(t_mat)
}
gibbs_sampling <- function(XtoY, YtoX, y = 1, iter){
  gibbs_sampled <- matrix(0, iter, 2)
  for(k in 1:iter){
    x <- sample(0:2, 1, prob = XtoY[y+1,])
    y <- sample(0:1, 1, prob = YtoX[x+1,])
    gibbs_sampled[k,] <- c(x, y)
  }
  return(gibbs_sampled)
}

xy_joint <- matrix(c(0.15, 0.15, 0.15, 0.15, 0.2, 0.2), 3, 2, byrow=F)
dimnames(xy_joint)[[1]]<-0:2
dimnames(xy_joint)[[2]]<-0:1

XtoY <- transition_matrix(t(xy_joint))
YtoX <- transition_matrix(xy_joint)
nburnin <- 3000
nsample <- 10^4
iter <- nburnin + nsample
set.seed(17)
gibbs_sample_data <- data.frame(gibbs_sampling(XtoY, YtoX, 1, iter))
gibbs_sample_data
names(gibbs_sample_data)<-c("Y","X")
sample_joint = table(gibbs_sample_data[(nburnin+1):(iter),])/nsample

sample_x_marginal <- matrix(apply(sample_joint, 1, sum),1,3)
x_marginal <- matrix(apply(xy_joint, 1, sum), 1, 3)
dimnames(sample_x_marginal)[[2]] <- 0:2
dimnames(sample_x_marginal)[[1]] <- "probs"
dimnames(x_marginal)[[2]] <- 0:2
dimnames(x_marginal)[[1]] <- "probs"

sample_y_marginal <- matrix(apply(sample_joint, 2, sum),1,2)
y_marginal <- matrix(apply(xy_joint, 2, sum), 1, 2)
dimnames(sample_y_marginal)[[2]] <- 0:1
dimnames(sample_y_marginal)[[1]] <- "probs"
dimnames(y_marginal)[[2]] <- 0:1
dimnames(y_marginal)[[1]] <- "probs"

sample_joint
xy_joint

sample_x_marginal
x_marginal

sample_y_marginal
y_marginal