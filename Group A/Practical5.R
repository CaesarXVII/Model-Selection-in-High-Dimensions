require(mvtnorm)

require(MASS)

require(selectiveInference)
############################################## EXERCISE 2  ###

##  1)
set.seed(11)

n = 1000

p = 10

rho = 0

mu = rep(1,p)

sigma = rep(0,p^2)

sigma = matrix(data = sigma, ncol = p,nrow = p)

# Autoregressive structure

for (i in 1:p) {
  
  for (j in 1:p) {
    
    
    sigma[i,j] = rho^(abs(i-j))
    
    
  }
  
  
}

X = mvrnorm(n,mu,sigma)

dim(X)

## 2)
# new beta
beta = c(3,1.5,0,2,rep(0,6))
#### 3)
y_hat = X%*%beta + rnorm(n,0,1)

##### a)

help(fs)
help(selectiveInference)
require(selectiveInference)
Step_model<-fs(X, y_hat)

#### alpha=0.9 . So, for small values of alpha, less number of steps 
Step_out <- fsInf(Step_model,  type="active",alpha = 0.05)
forwardStop(Step_out$pv, alpha = 0.5)

#### b)
Step_AIC <- fsInf(Step_model,  type="aic",alpha = 0.05)

#### c)

install.packages("tictoc")

help("Sys.time")

require(tictoc)

tic()

Step_model
Sys.sleep(1)
toc()

#### So, the time was 1.17 seconds

############################################ exercice 3


str(data_zambia)
summary(data_zambia)

#data withou factors

zambia<-data_zambia[,c(1:7,21,22,24)]
zambia<-as.matrix(zambia)

beta = c(3,1.5,0,2,rep(0,6))

y_hat_z = zambia%*%beta + rnorm(1927,0,1)

Step_model_z<-fs(zambia, y_hat_z)
Step_model_z

Step_out_z <- fsInf(Step_model_z,  type="active",alpha = 0.05)
forwardStop(Step_out_z$pv, alpha = 0.4)

#### b)
Step_AIC_z <- fsInf(Step_model_z,  type="aic",alpha = 0.4)
Step_AIC_z 

# With AIC we take 5 steps while in the ForwardStop rule we take 4, having the same alpha