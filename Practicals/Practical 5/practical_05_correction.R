################################################
###                                          ###
###        PRACTICAL 5 - CORRECTION          ###
###                                          ###
################################################

rm(list=ls())

# install.packages("selectiveInference")

require(selectiveInference)

require(MASS)

### Exercise 2 ###

# Create the simulation setting

set.seed(11)

n = 1000

p = 10

# change rho as you please to inspect other correlations among the predictors

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


beta = c(3,1.5,0,2,rep(0,6))


y = X%*%beta + rnorm(n,0,1) #for us sigma is 1


## Part a ##


fsfit = fs(X,y) #partial correlations like OMP or stagewise

fsfit$action #order of regressors

fsfit$beta #the beta estimation at each step


# compute sequential p-values and confidence intervals  (sigma estimated from full model) 

out = fsInf(fsfit,alpha = 0.4) #default

out # the value of forward stop is the last regressor which is active

# estimate optimal stopping point 

last = forwardStop(out$pv, alpha=0.4) # you want to reject more often fixing higher alfa

stop_point = forwardStop(out$pv,alpha = 0.85)

fsfit$action[1:last] # to catch right components

# Conclusions: fixing a higher alfa will let you include on average more regressors. 


## Part b ##

# We use the same fit, the k step is decided by AIC penalty

out_2 = fsInf(fsfit,type = "aic",alpha = 0.99) #akaike case, the fixed value of alfa is 0.1

out_2 #be careful check with classic AIC evaluation (sometimes is not correct)

# AIC check

m_1 = lm(y ~ X[,1] + X[,4] + X[,2]) # check if it is true in AIC case

AIC(m_1)

m_2 = lm(y ~ X[,1] + X[,4] + X[,2] + X[,10] + X[,9] + X[,3])

AIC(m_2)

m_3 = lm(y ~ X[,1] + X[,4] + X[,2] + X[,3] + X[,5]) # check if it is true in AIC case

AIC(m_3) 


# We have already an idea that AIC tends to overfit


### YOU CAN PROGRAM AIC USING ORDER IN fsfit$action ###


# We use the same fit, the k step is decided by BIC penalty


out_3 = fsInf(fsfit, type = "aic", mult = log(n)) #bic case

out_3

# Another nice evidence, BIC has harsh penalty than AIC.

# Other possibilities of fsInf() function

out_4 = fsInf(fsfit,type = "all",k = 4,mult = log(n)) # this is just to stop at a fixed number of steps

out_4

## Part c ##


# Look at Practical 3, see your exhaustive search time for p=10. 

# In my case around 17 minutes for 1023 models (2^10 -1) since I used 1sec per model

require(tictoc)

tic()

fsfit = fs(X,y)

out = fsInf(fsfit) #default

last = forwardStop(out$pv, alpha=0.05) # you want to reject more often fixing higher alfa

Sys.sleep(1)
toc()

# 1.81 against 17 minutes... Exhaustive vs stepwise, a huge gain!


## Part d ##

# Create the simulation setting

set.seed(11)

n = 100

p = 100  

#p = 150

# change rho as you please to inspect other correlations among the predictors

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


beta = c(3,1.5,0,2,rep(0,96))


y = X%*%beta + rnorm(n,0,1) #for us sigma is 1


fsfit = fs(X,y) #partial correlations like OMP or stagewise


# You need to have p < n to work with this kind of package. Or you can do a first screening based on

# marginal correlations (like in SIS): order them and take the first n. 


sigma_hat = estimateSigma(X,y) 


# You need to estimate sigma in this big models. If not there is bad estimation by default since sigma

# is estimated on the full model.


# compute sequential p-values and confidence intervals

out = fsInf(fsfit,sigma = 0.977) 


out # the value of forward stop is the last regressor which is active

# estimate optimal stopping point 


last = forwardStop(out$pv, alpha=0.05) # you want to reject more often fixing higher alfa

fsfit$action[1:last] # to catch right components


# Evaluation with model selection criteria

order = list()

for (z in 1:100) {
  
  y = X%*%beta + rnorm(n,0,1) 
  
  
  fsfit = fs(X,y) 
  
  
  sigma_hat = estimateSigma(X,y) 
  
  a = sigma_hat$sigmahat
  
  out = fsInf(fsfit,sigma = a) 
  
  last = forwardStop(out$pv, alpha=0.05) # you want to reject more often fixing higher alfa
  
  order[[z]] = fsfit$action[1:last] 
  
}

### Exact models ###

exact = rep(0,100)

for (i in 1:100) {
  
  if (identical(order[[i]],c(1,4,2)) == TRUE){
    
    exact[i] = 1
    
    
  }
  
  
}

sum(exact)/100  

### Correct (related to consistency in model selection) ###

prop_cor = rep(0,100)

for (i in 1:100) {
  
  if (length(order[[i]]) >= 3) {
    
    
    prop_cor[i] = 1
    
  } 
}

sum(prop_cor)/100  #Due to the low number of observations n=100

  
### Average number of regressors ###


# Find the number of elements in each order[[z]]

num.el = sapply(order, length)

sum(num.el)/100

# The role of alfa is the same as before, as you increase it you get overfitting while if you lower it

# you will reject more often thus leading to a sparser model. 


### EXERCISE 3 ###

getwd()

load("malnutrion_zambia_cleaned.Rda")


data_zambia = data_zambia[,c(1:7,21,22,24)] #exclude the factors from the analysis

### selective inference package ###

## ForwardStop rule ##

X = as.matrix(data_zambia[,2:10])

y = data_zambia[,1]

fsfit = fs(X,y) 

fsfit$action #order by partial correlations

# compute sequential p-values and confidence intervals # (sigma estimated from full model)

out = fsInf(fsfit) 

# The above function is using forward stop and pvalue evaluated at each step with forwardstop 

# It is a formula to control FDR.

out 

forwardStop(out$pv, alpha=.10) #only breastfeeding and height of the mother matters


## AIC and BIC penalty to decide step k ##

out_2 = fsInf(fsfit,type = "aic") #akaike case, the fixed value of alfa is 0.1

out_2 #estimated stopping point is correct. 

# You notice that AIC tends to overfit in this framework. Remember that we were doing 

# exhaustive search and not stepwise with AIC in the other Practicals.

out_3 = fsInf(fsfit, type = "aic", mult = log(n)) #bic case

out_3 # BIC has a harsh penalty that correct AIC tendency to overfit (in this applied context)

# Here we are in a reality case so AIC is not optimal anymore as in our simulation setting, we expect

# something different while forward stop control the FRD. BIC has a harsh penalty that is why it 

# selects less variables. Moreover we know that, from previous practicals, our residuals inspection

# told us that we had a right tail with respect to the Normal QQ plot (see ebook corrections). 


# Check Alternatives (see practical 3)

# For example best subsets: is increasingly difficult as p increases in terms of computing time

# This is interesting to check AIC results

help("regsubsets")

require(leaps)


regsubsets.out <- regsubsets(data_zambia$`Height for age sd` ~ .,data = data_zambia,nbest = 1,
                             nvmax = NULL,    # NULL for no limit on number of variables
                             force.in = NULL, force.out = NULL,method = "exhaustive")

summary(regsubsets.out)


plot(regsubsets.out) # BIC is default, equal to result in selective inference


plot(regsubsets.out,scale = "Cp") #C_p case which is equal to AIC with a linear model



