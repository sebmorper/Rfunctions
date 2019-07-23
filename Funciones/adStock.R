###########################################################################
## AD STOCK
###########################################################################


###########################################################################
##### Description #####
###########################################################################
# This script contains functions to adjust a variable along the time
# it gaves the variable an effect (lag / decay) to convert a discrete
# variable into a continous variable.
# e.g., the post effect of marketing activities in sells, visits,
# leads, etc.

###########################################################################
##### lag #####
###########################################################################
# The lag function takes a numeric vector (x) and moves it a delta
# time (days, weeks, months, years,...) it depends on the terms of time
# that x is.

lag <- function(x, delta) {
  y <- rep(0, length(x))
  for(i in seq_along(x)) {
    y[i+delta] <- x[i]
  }
  return(y[1:length(x)])
}

###########################################################################
##### simple decay model #####
###########################################################################
# The decay function takes a numeric vector (x) and applys an post effect
# that converts a discrete vector to continous:
# y[i+1] = x[i+1] + x[i]*lambda

decay <- function(x, lambda) {
  y <- as.numeric(x)
  for(i in seq_along(x)) {
    y[i+1] <- x[i+1] + (y[i] * lambda)
  }
  return(y[1:length(x)])
}


###########################################################################
##### log decay model #####
###########################################################################
# The Log Decay model applies a straightforward logarithmic distribution
# to the advertising variable

log_decay <- function(x, lambda) {
  y <- as.numeric(x)
  for(i in seq_along(x)) {
    if(x[i] == 0) {
      y[i] <-  0
    } else {
      y[i] <- log(x[i])
    }
  }
  return(decay(y, lambda = lambda))
}


###########################################################################
##### negative exponential decay model #####
###########################################################################
# Applies a negative exponential distribution to the basic
# Adstock formula, using two parameters, the 'decay' weight
# parameter lambda and the learning rate or saturation parameter v.

neg_exp_decay <- function(x, v, lambda) {
  y <- as.numeric(x)
  for(i in seq_along(x)) {
    y[i] <- 1- exp(-v*x[i])
  }
  return(decay(y, lambda = lambda))
}


###########################################################################
##### logistic decay model #####
###########################################################################
# Using a logistic distribution instead of negative exponential will
# impart an S-shape to the Adstock variable, implying an inflexion
# point or 'threshold' level of GRPs before diminishing returns set in.
# Below this threshold, the logistic function imparts exponential returns.

logistic_decay <- function(x, v, lambda) {
  y <- as.numeric(x)
  for(i in seq_along(x)) {
    y[i] <- 1/(1+exp(-v*x[i]))
  }
  return(decay(y, lambda = lambda))
}

###########################################################################
##### lag & simple decay #####
###########################################################################
# The lag_decay function combines a lag vector and a simple decay vector
# and apply them in a single x numeric vector. The function return a 
# data frame.

lag_decay <- function(x, l, d, name){

  new.data <- data.frame(x)
  new.names <- rep(0, length(l)*length(d))
  c <- 1
  for(i in seq_along(l)){
    for(j in seq_along(d)) {
      new.data[,c] <- lag(decay(x,d[j]), l[i])
      new.names[c] <- paste(name, ".l", l[i], ".d", d[j], sep = "")
      
      c <- c+1
    }
  }
  names(new.data) <- new.names
  return(new.data)
}


###########################################################################
##### lag & logarithmic decay #####
###########################################################################
# The lag_decay function combines a lag vector and a log decay vector
# and apply them in a single x numeric vector. The function return a 
# data frame.

lag_log_decay <- function(x, l, d, name){
  
  new.data <- data.frame(x)
  new.names <- rep(0, length(l)*length(d))
  c <- 1
  for(i in seq_along(l)){
    for(j in seq_along(d)) {
      new.data[,c] <- lag(log_decay(x,d[j]), l[i])
      new.names[c] <- paste("log.", name, ".l", l[i], ".d", d[j], sep = "")
      
      c <- c+1
    }
  }
  names(new.data) <- new.names
  return(new.data)
}


###########################################################################
##### lag & negative exponential decay #####
###########################################################################
# The lag_decay function combines a lag vector and a neg exp decay vector
# and apply them in a single x numeric vector. The function return a 
# data frame.

lag_neg_exp_decay <- function(x, l, d, V, name){
  
  new.data <- data.frame(x)
  new.names <- rep(0, length(l)*length(V),length(d))
  c <- 1
  for(i in seq_along(l)){
    for(k in seq_along(V)) {
      for(j in seq_along(d)) {
        new.data[,c] <- lag(neg_exp_decay(x, V[k], d[j]), l[i])
        new.names[c] <- paste("neg.exp." ,name, ".l", l[i],
                              ".d", d[j], ".v", V[k], sep = "")
        
        c <- c+1
      }
    }
  }
  names(new.data) <- new.names
  return(new.data)
}


###########################################################################
##### lag & logistic decay #####
###########################################################################
# The lag_logistic_decay function combines a lag vector and a logistic
# decay vector and apply them in a single x numeric vector.
# The function return a data frame.

lag_logistic_decay <- function(x, l, d, V, name){
  
  new.data <- data.frame(x)
  new.names <- rep(0, length(l)*length(V),length(d))
  c <- 1
  for(i in seq_along(l)){
    for(k in seq_along(V)) {
      for(j in seq_along(d)) {
        new.data[,c] <- lag(logistic_decay(x, V[k], d[j]), l[i])
        new.names[c] <- paste("logit." ,name, ".l", l[i],
                              ".d", d[j], ".v", V[k], sep = "")
        
        c <- c+1
      }
    }
  }
  names(new.data) <- new.names
  return(new.data)
}


###########################################################################
##### simple ad stock #####
###########################################################################
# The function applys the lag_decay function to a whole data frame. 
# Takes a data frame and returns a data frame.

simple.ad.stock <- function(the.data, the.lags, the.decays) {
  new.data <- the.data
  for(i in seq_along(the.data)) {
    new.data <- cbind(new.data, lag_decay(x = the.data[,i],
                                          l = the.lags,
                                          d = the.decays,
                                          name = names(the.data)[i]))
  }
  return(new.data)
}


###########################################################################
##### logarithmic ad stock #####
###########################################################################
# The function applys the lag_log_decay function to a whole data frame. 
# Takes a data frame and returns a data frame.

log.ad.stock <- function(the.data, the.lags, the.decays) {
  new.data <- the.data
  for(i in seq_along(the.data)) {
    new.data <- cbind(new.data, lag_log_decay(x = the.data[,i],
                                              l = the.lags,
                                              d = the.decays,
                                              name = names(the.data)[i]))
  }
  return(new.data)
}


###########################################################################
##### negative exponential ad stock #####
###########################################################################
# The function applys the lag_neg_exp_decay function to a whole data frame. 
# Takes a data frame and returns a data frame.

neg.exp.ad.stock <- function(the.data, the.lags, the.decays, the.vs) {
  new.data <- the.data
  for(i in seq_along(the.data)) {
    new.data <- cbind(new.data, lag_neg_exp_decay(x = the.data[,i],
                                                  l = the.lags,
                                                  d = the.decays,
                                                  V = the.vs,
                                                  name = names(the.data)[i]))
  }
  return(new.data)
}


###########################################################################
##### logistic ad stock #####
###########################################################################
# The function applys the lag_logistic_decay function to a whole data frame. 
# Takes a data frame and returns a data frame.

logit.ad.stock <- function(the.data, the.lags, the.decays, the.vs) {
  new.data <- the.data
  for(i in seq_along(the.data)) {
    new.data <- cbind(new.data, lag_logistic_decay(x = the.data[,i],
                                                   l = the.lags,
                                                   d = the.decays,
                                                   V = the.vs,
                                                   name = names(the.data)[i]))
  }
  return(new.data)
}

###########################################################################
##### Examples #####
###########################################################################
a <- c(0,1,2,3,5,8,0,0)
b <- c(1,4,2,3,6,7,2,0)

a
lag(a, 2)

b
decay(b, 0.60)
log_decay(b, 0.6)
neg_exp_decay(b, 0.60, 0.4)
logistic_decay(b, 0.60, 0.4)

dta <- data.frame(a,b)
lg <- c(1,2,4)
dcy <- c(.60,.80,.90)
sat <- c(.5,1,2)
simple.ad.stock(dta,lg,dcy)
log.ad.stock(dta, lg, dcy)
neg.exp.ad.stock(dta, lg, dcy, sat)
logit.ad.stock(dta, lg, dcy, sat)


###########################################################################
##### Author: Sebastián Morales Peralta #####
###########################################################################