pi <- c(3/4, 1/4)
mu <- c(0, 8)
sd <- c(1, 1)


accept_prob <- function(x, x1, mu, sd, pi){
  px <- pi[1]*dnorm(x, mu[1], 1) + pi[2]*dnorm(x, mu[2], 1)
  px1 <- pi[1]*dnorm(x1, mu[1], 1) + pi[2]*dnorm(x1, mu[2], 1)
  qx <- dnorm(x, mean = x1, sd)
  qx1 <- dnorm(x1, mean = x, sd)
  
  return(min(1, (px1*qx)/(px*qx1)))
}

#### Question 2a ####

run_mcmc1 <- function(iterations){
  x <- 0
  sample <- numeric()
  for(i in 1:3000){
    x1 <- rnorm(1, mean = x, sd = 1)
    p <- min(1, accept_prob(x, x1, mu, 1, pi))
    propose <- rbinom(1, 1, p)
    if(propose == 1){
      sample <- c(sample, x1)
      x <- x1
    }
  }
  sample
}


set.seed(98)
mcmcsample <- run_mcmc1(3000)
mean(mcmcsample)  # estimator of E(w)
length(mcmcsample)  # number of successesful samples
hist(mcmcsample)


### question 2b ###

run_mcmc2 <- function(iter){
  x <- 0
  sample <- numeric()
  flips <- rbinom(iter, 1, 1/2)
  for(i in 1:iter){
    x1 <- rnorm(1, mean = x, sd = 1)
    p <- ifelse(flips[i] == 1, min(1, accept_prob(x, x1, mu, 1, pi)), min(1, accept_prob(x, x1, mu, 10, pi)))
    propose <- rbinom(1, 1, p)
    if(propose == 1){
      sample <- c(sample, x1)
      x <- x1
    }
  }
  sample
}


set.seed(0604)
mcmcsample2 <- run_mcmc2(3000)

mean(mcmcsample2)  # estimator of E(w)
length(mcmcsample2)  # number of successesful samples
hist(mcmcsample2)




  