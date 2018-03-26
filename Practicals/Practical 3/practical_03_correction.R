################################################
###                                          ###
###        PRACTICAL 3 - CORRECTION          ###
###                                          ###
################################################

rm(list = ls())

require(mvtnorm)

require(MASS)

### EXERCISE 1 - Simulations and CV ###

## Part a

n = 1000

p = 5

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

## Part b

beta = c(3,1.5,0,2,0)

# Signal to noise ratio: 

# Statistical literature: reciprocal of CV --> u / sigma

s_n_ratio = mean(X%*%beta) # same result taking the mean out of X directly

# sigma in our case is set to one


# Engineering literature: Var(f(x)) / Var(epsilon)

signal_to_noise_ratio = 6.52

data = X%*%beta

k = sqrt(var(data)/(signal_to_noise_ratio*var(rnorm(n,0,1))))


y_hat_eng = data + k*noise # how you generate data to have your fixed signal to noise ratio

## Part c

y_hat = X%*%beta + rnorm(n,0,1) # you can also use previous formula (i.e. y_hat_eng = data + k*noise )

## Part d

# set.seed()

help("sample")

data = data.frame(cbind(y_hat,X))

colnames(data) = c("y","x1","x2","x3","x4","x5")

View(data)

ind = 1:1000

index = sample(x = ind,size = 500,replace = F)

length(index)

train_set = data[index,]

test_set = data[-index,]


# Possible models: 2^p - 1 = 2^5 - 1 = 31 models (use the Newton's binomial theorem)

dim(X)


cv_error = rep(0,31)

aic_values = rep(0,31)


# One regressor only models

for(i in 1:5) {
  
  
  m_1 = lm(train_set$y ~ train_set[,i+1], data = train_set)
  
  cv_error[i] = mean((test_set$y - (cbind(rep(1,500),test_set[,i+1])%*% m_1$coefficients))^2)
  
  aic_values[i] = -2*logLik(m_1) + 2*3

}


# Two regressors models

help("combn")

x = c(1,2,3,4,5)

M = combn(x,2)

dim(M)

for(i in 1:10) {
  
  
  m_2 = lm(train_set$y ~ train_set[,M[1,i]+1] + train_set[,M[2,i]+1], data = train_set)
  
  cv_error[i+5] = mean((test_set$y - cbind(rep(1,500),test_set[,M[1,i]+1], test_set[, M[2,i] +1])%*%m_2$coefficients)^2)
  
  aic_values[i+5] = -2*logLik(m_2) + 2*4
}


# Three regressors models

M = combn(x,3)

dim(M)

for(i in 1:10) {
  
  
  m_3 = lm(train_set$y ~ train_set[,M[1,i]+1] + train_set[,M[2,i]+1] + train_set[,M[3,i]+1], data = train_set)
  
  cv_error[i+ 15] = mean((test_set$y - cbind(rep(1,500),test_set[,M[1,i]+1], test_set[, M[2,i] +1],test_set[, M[3,i]+1])%*%m_3$coefficients)^2)
  
  aic_values[i+15] = -2*logLik(m_3) + 2*5
  
}


# Four regressors models

M = combn(x,4)

dim(M)

for(i in 1:5) {
  
  
  m_4 = lm(train_set$y ~ train_set[,M[1,i]+1] + train_set[,M[2,i]+1] + train_set[,M[3,i]+1] + train_set[,M[4,i]+1], data = train_set)
  
  cv_error[i+25] = mean((test_set$y - cbind(rep(1,500),test_set[,M[1,i]+1], test_set[, M[2,i] +1],test_set[, M[3,i]+1],test_set[, M[3,i]+1])%*% m_4$coefficients)^2)
  
  aic_values[i+25] = -2*logLik(m_4) + 2*6
  
}

help("logLik")

# Full model 

full_model = lm(train_set$y ~ ., data = train_set)

full_model$coefficients

cv_error_full = mean((test_set$y - cbind(rep(1,500), test_set$x1,test_set$x2,test_set$x3,test_set$x4,test_set$x5)%*% full_model$coefficients)^2)

cv_error[31] = cv_error_full

aic_values[31] = -2*logLik(full_model) + 2*7

min(cv_error)


# Remember that k=2 CV is not optimal wrt the choice of k (search online for info why 10 is used in practice) 


# correct model

correct_model = lm(train_set$y ~ train_set$x1 + train_set$x2 + train_set$x4, data = train_set)


cv_error_cor = mean((test_set$y - cbind(rep(1,500), test_set$x1,test_set$x2,test_set$x4)%*% correct_model$coefficients)^2)


## Part e

# If p=100 --> 2^100 - 1 is roughly 1.267*10^30 possible models

install.packages("tictoc")

help("Sys.time")

require(tictoc)

tic()

full_model = lm(train_set$y ~ ., data = train_set)

cv_error_full = mean((test_set$y - cbind(rep(1,500), test_set$x1,test_set$x2,test_set$x3,test_set$x4,test_set$x5)%*% full_model$coefficients)^2)

Sys.sleep(1)
toc()

# 1.01 sec * 10^30 (31.536.000 seconds in one year) so roughly 10^8 sec is three years!!!

# Total 30000000000000000000000 years ... :-) It is impossible, we need better model selection tecniques!

### EXERCISE 2 - Simulations and AIC ###

## Part a, b, c 

# Be careful with AIC definition, that is why is always useful to program your codes!

logLik(full_model)

AIC(full_model) # number of parameters which are 6 (5+1 intercept) and sigma. It is the dim(paramterspace) 

help("AIC")

-2*logLik(full_model) + 2*7

# We use the previous loop done for CV to retrieve the result

min(aic_values)

which.min(aic_values) # 17 is the true one

AIC(correct_model)

# Of course the task of part c is impossible for the same reasons of CV (it takes more than a billion of years)


## Part d

# There are different sources of randomness: X matrix, epsilon (error) --> y (induced) and the split for CV

# We need to choose what to preserve, I will replicate every time only the randomness of epsilon and of course the

# split of the CV


ind = 1:1000

pos_CV = rep(0,100)

pos_AIC = rep(0,100)


for(z in 1:100) {

y_hat = X%*%beta + rnorm(n,0,1) # you can also use previous formula (i.e. y_hat_eng = data + k*noise )

data = data.frame(cbind(y_hat,X))

colnames(data) = c("y","x1","x2","x3","x4","x5")

index = sample(x = ind,size = 500,replace = F)

train_set = data[index,]

test_set = data[-index,]

cv_error = rep(0,31)

aic_values = rep(0,31)

# One regressor only models

for(i in 1:5) {
  
  
  m_1 = lm(train_set$y ~ train_set[,i+1], data = train_set)
  
  cv_error[i] = mean((test_set$y - (cbind(rep(1,500),test_set[,i+1])%*% m_1$coefficients))^2)
  
  aic_values[i] = -2*logLik(m_1) + 2*3
  
}

# Two regressors models

x = c(1,2,3,4,5)

M = combn(x,2)


for(i in 1:10) {
  
  
  m_2 = lm(train_set$y ~ train_set[,M[1,i]+1] + train_set[,M[2,i]+1], data = train_set)
  
  cv_error[i+5] = mean((test_set$y - cbind(rep(1,500),test_set[,M[1,i]+1], test_set[, M[2,i] +1])%*%m_2$coefficients)^2)
  
  aic_values[i+5] = -2*logLik(m_2) + 2*4
}


# Three regressors models

M = combn(x,3)


for(i in 1:10) {
  
  
  m_3 = lm(train_set$y ~ train_set[,M[1,i]+1] + train_set[,M[2,i]+1] + train_set[,M[3,i]+1], data = train_set)
  
  cv_error[i+ 15] = mean((test_set$y - cbind(rep(1,500),test_set[,M[1,i]+1], test_set[, M[2,i] +1],test_set[, M[3,i]+1])%*%m_3$coefficients)^2)
  
  aic_values[i+15] = -2*logLik(m_3) + 2*5
  
}


# Four regressors models

M = combn(x,4)


for(i in 1:5) {
  
  
  m_4 = lm(train_set$y ~ train_set[,M[1,i]+1] + train_set[,M[2,i]+1] + train_set[,M[3,i]+1] + train_set[,M[4,i]+1], data = train_set)
  
  cv_error[i+25] = mean((test_set$y - cbind(rep(1,500),test_set[,M[1,i]+1], test_set[, M[2,i] +1],test_set[, M[3,i]+1],test_set[, M[3,i]+1])%*% m_4$coefficients)^2)
  
  aic_values[i+25] = -2*logLik(m_4) + 2*6
  
}

# Full model 

full_model = lm(train_set$y ~ ., data = train_set)

full_model$coefficients

cv_error_full = mean((test_set$y - cbind(rep(1,500), test_set$x1,test_set$x2,test_set$x3,test_set$x4,test_set$x5)%*% full_model$coefficients)^2)

cv_error[31] = cv_error_full

aic_values[31] = -2*logLik(full_model) + 2*7

pos_CV[z] = which.min(cv_error)

pos_AIC[z] = which.min(aic_values)

}

# We know that the right position is 17, from 25-30 are difficult model expecially number 28 and 26

pos_CV

pos_AIC

# Exact model proportion

sum(pos_CV == "17")/100 #0.36  #0.44 #0.45

sum(pos_AIC == "17")/100 #0.76 #0.63 #0.71

# Correct model proportion (linked to the consistency of the model selection procedure)

sum(pos_CV > 16)/100 

sum(pos_AIC > 16)/100

# Average number of regressors

# CV CASE

try = as.factor(pos_CV)

levels(try)

sum(pos_CV > 25 & pos_CV < 30) # Model with 4 regressors (1,2,4,5)

# 41 entries

sum(pos_AIC == "31") #full model: 23 entries 

# Average number

(3*45 + 4*31 + 5*24)/100  #3.87 average number of regressors #3.79

# AIC case

try = as.factor(pos_AIC)

levels(try) # correctly it looks at model 26 and 28 as mistakes: the first is (1,2,3,4) while the other is (1,2,4,5)

(3*71 + 4*24 + 4*5) /100 #3.24 #3.29



### WHY DO YOU THINK AIC PERFORMS BETTER THAN CV IN THIS CASE ??? ###

# AIC is derived from likelihood so when the model is correct (as in this case) it is optimal. On the other hand
# CV is non parametric thus inferior in this setting. Of course in real application you should remember that our
# model assumption may not be correct.


### EXERCISE 3 ###

# Load Zambia dataset 

data_zambia = load(file = "your_directory/data_zambia.Rda"")
     
data_zambia = data_zambia[,c(1:7,21,22,24)] #exclude the factors from the analysis
     
     
### Exhaustive search with leaps (AIC case) ###
     
require(leaps)
     
regsubsets.out <- regsubsets(data_zambia$`Height for age sd` ~ .,data = data_zambia,nbest = 1,
                             nvmax = NULL,    # NULL for no limit on number of variables
                             force.in = NULL, force.out = NULL,method = "exhaustive")
     
summary(regsubsets.out)
     

plot(regsubsets.out) # BIC is default
     
     
plot(regsubsets.out,scale = "Cp") #C_p case which is equal to AIC with a linear model
     
     
     
### Exhaustive search with glmulti (AIC case) ### 
     
     
     
glmulti.lm.out <- glmulti::glmulti(data_zambia$`Height for age sd` ~ .,data = data_zambia, 

level = 1,               # No interaction considered
     
method = "h",            # Exhaustive approach
     
crit = "aic",            # AIC as criteria
     
confsetsize = 5,         # Keep 5 best models
     
plotty = F, report = F,  # No plot or interim reports
     
fitfunction = "lm")      # lm function
     
     
     
### Exhaustive search with MumIn (AIC case) ###
     
require(MuMIn)
     
data_model <- lm(data_zambia$`Height for age sd` ~ .,data = data_zambia)
     
combinations <- dredge(data_model)
     
     
print(combinations)
     
### Exhaustive search with caret (CV case and AIC case) ###
     
     
require(caret)
     
     
attach(data_zambia)
     
     
# Unfortunately there is no exhaustive search based on CV in Caret, it is just stepwise.
     
     
#setting up 10-fold cross-validation
     
     
control <- trainControl(method="cv", number=10) 
     
     
#finding the ideal model by AIC criterion
     
     
model_AIC = train(`Height for age sd`~.,data=data_zambia,method="lmStepAIC",trControl=control)
     
     
#finding the ideal model by mean square error
     
     
modelCV = train(`Height for age sd`~.,data=data_zambia,method="lm",trControl=control)
     
     
detach(data_zambia)
     
     
# In order to do a full exhaustive search with CV, we should exploit the codes produced in exercise 1. Of course 
     
# as the number of variables increase, the task becomes impossible.
     
