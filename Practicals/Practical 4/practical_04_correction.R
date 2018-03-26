################################################
###                                          ###
###        PRACTICAL 4 - CORRECTION           ###
###                                          ###
################################################

rm(list=ls())

### EXERCISE 1 - SIMULATIONS OF A CORRELATED ENVIRONMENTS ###

## Part (a), (b) and (c)

# There are different sources of randomness: X matrix, epsilon (error) --> y (induced) and the split for CV

# We need to choose what to preserve, I will replicate every time only the randomness of epsilon and of course the

# split of the CV. We need then to set a seed for the X matrix. All this discussion of randomness is pivotal for a

# significant and controlled simulation setting. 


require(MASS)


set.seed(5)

n = 1000

p = 5

# change rho as you please to inspect other correlations among the predictors

rho = 0.5 

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


# beta = c(0,0,1,0,0) alternative beta to check bias properties

beta = c(3,1.5,0,2,0)

ind = 1:1000

pos_CV = rep(0,100)

pos_AIC = rep(0,100)


for(z in 1:100) {
  
  
  y_hat = X%*%beta + rnorm(n,0,1) 
  
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
    
    m_1 = lm(data$y ~ data[,i+1], data)
    
    aic_values[i] = -2*logLik(m_1) + 2*3
    
  }
  
  # Two regressors models
  
  x = c(1,2,3,4,5)
  
  M = combn(x,2)
  
  
  for(i in 1:10) {
    
    
    m_2 = lm(train_set$y ~ train_set[,M[1,i]+1] + train_set[,M[2,i]+1], data = train_set)
    
    cv_error[i+5] = mean((test_set$y - cbind(rep(1,500),test_set[,M[1,i]+1], test_set[, M[2,i] +1])%*%m_2$coefficients)^2)
    
    m_2 = lm(data$y ~ data[,M[1,i]+1] + data[,M[2,i]+1], data)
    
    aic_values[i+5] = -2*logLik(m_2) + 2*4
  }
  
  
  # Three regressors models
  
  M = combn(x,3)
  
  
  for(i in 1:10) {
    
    
    m_3 = lm(train_set$y ~ train_set[,M[1,i]+1] + train_set[,M[2,i]+1] + train_set[,M[3,i]+1], data = train_set)
    
    cv_error[i+ 15] = mean((test_set$y - cbind(rep(1,500),test_set[,M[1,i]+1], test_set[, M[2,i] +1],test_set[, M[3,i]+1])%*%m_3$coefficients)^2)
    
    m_3 = lm(data$y ~ data[,M[1,i]+1] + data[,M[2,i]+1] + data[,M[3,i]+1], data)
    
    aic_values[i+15] = -2*logLik(m_3) + 2*5
    
  }
  
  
  # Four regressors models
  
  M = combn(x,4)
  
  
  for(i in 1:5) {
    
    
    m_4 = lm(train_set$y ~ train_set[,M[1,i]+1] + train_set[,M[2,i]+1] + train_set[,M[3,i]+1] + train_set[,M[4,i]+1], data = train_set)
    
    cv_error[i+25] = mean((test_set$y - cbind(rep(1,500),test_set[,M[1,i]+1], test_set[, M[2,i] +1],test_set[, M[3,i]+1],test_set[, M[3,i]+1])%*% m_4$coefficients)^2)
    
    m_4 = lm(data$y ~ data[,M[1,i]+1] + data[,M[2,i]+1] + data[,M[3,i]+1] + data[,M[4,i]+1], data)
    
    aic_values[i+25] = -2*logLik(m_4) + 2*6
    
  }
  
  # Full model 
  
  full_model = lm(train_set$y ~ ., data = train_set)
  
  full_model$coefficients
  
  cv_error_full = mean((test_set$y - cbind(rep(1,500), test_set$x1,test_set$x2,test_set$x3,test_set$x4,test_set$x5)%*% full_model$coefficients)^2)
  
  cv_error[31] = cv_error_full
  
  full_model = lm(data$y ~ ., data)
  
  aic_values[31] = -2*logLik(full_model) + 2*7
  
  pos_CV[z] = which.min(cv_error)
  
  pos_AIC[z] = which.min(aic_values)
  
}

# We know that the right position should be 17 which is the exact model.

pos_CV

pos_AIC

# Exact model proportion

sum(pos_CV == "17")/100 

sum(pos_AIC == "17")/100 

# Correct model proportion (linked to the consistency of the model selection procedure)

sum(pos_CV > 16)/100 

sum(pos_AIC > 16)/100


# Average number of regressors

counted = c(rep(1,5),rep(2,10), rep(3,10), rep(4,5),5)

length(counted)


result_cv = rep(0,100)

result_aic = rep(0,100)


for (i in 1:100) {
  
  
  result_cv[i] = counted[pos_CV[i]]

  result_aic[i] = counted[pos_AIC[i]]  
  
}


# CV CASE

mean(result_cv)

# AIC case

mean(result_aic)


### CONCLUSIONS ### PART C ###

# Correlation does not play a role here because it biases our estimates but not that much that is necessary to 

# make AIC (less so CV) fails to recognize the significant regressors. This holds especially in the presence 

# of strong signal so for a beta with huge components. The transition chain is: I create correlated X, 

# I transmit correlation to the ys, the likelihood theory is not optimal anymore (y should be independent, X fixed) 

# and I get biased estimates.However I need a really important bias to make AIC fails to recognize significant 

# variables in the linear case. The GLM case is different: correlation is a plague because of the link function. 

# Even a small bias in that direction can alter the order of variables and thus AIC results. 


### EXERCISE 2 - THEORETICAL PART: q class of Error Measures ###

## See paper and pencil correction

### EXERCISE 3 - ROC CURVES ###

getwd()

load("data_leukemia_reduced.Rda")

## Part (a)

attach(data_leukemia_reduced)

m_1 = glm(y ~ V457, data = data_leukemia_reduced,family = "binomial")

summary(m_1)

length(m_1$fitted.values)

## Part (b)

# Confusion matrix

c0 = 0.5

conf_mat = table(y, m_1$fitted.values > c0) #unbalanced sample 47 ones and 25 0s

prop_confmat = prop.table(conf_mat, 1) # For ROC curve is on the line

TNR = conf_mat[1,1]/(conf_mat[1,1] + conf_mat[1,2]) # TRUE NEGATIVE (TNR)

FPR = conf_mat[1,2]/(conf_mat[1,1] + conf_mat[1,2])  # FALSE POSITIVE (FPR)

FNR = conf_mat[2,1]/(conf_mat[2,1] + conf_mat[2,2])  # FALSE NEGATIVE (FNR)
  
TPR = conf_mat[2,2]/(conf_mat[2,1] + conf_mat[2,2])  # TRUE POSITIVE (TPR)
                               
# Also possible with caret but with train and test arguments

# require(caret)

# confusionMatrix()

## Part (c)

help("seq")

c0 = seq(0.1,0.9,0.1)

TP = rep(0,length(c0))

FP = rep(0,length(c0))


for (i in 1:length(c0)) {
  
  conf_mat = table(y, m_1$fitted.values > c0[i]) #unbalanced sample 47 ones and 25 0s
  
  prop_confmat = prop.table(conf_mat, 1) # For ROC curve is on the columns
  
  TP[i] = prop_confmat[2,2]
  
  FP[i] = prop_confmat[1,2]
  
}

FP = sort(FP) # for a correct line

TP = sort(TP) # for a correct line

# Every ROC curve passes towards (0,0) and (1,1)

TP = c(0,TP,1)


FP = c(0,FP,1)


plot(FP,TP, col="green",main = "ROC Curve",xlab = "1 - Specificity",ylab = "Sensitivity",xlim=c(0, 1), ylim=c(0,1),type = "l")
lines(c(0, 1), c(0, 1), type='l',col="red")



## Part (d)

install.packages("pROC")

require(pROC)

help("roc")

roc_1 = roc(response=y, predictor = m_1$fitted.values)

plot(roc_1)


detach(data_leukemia_reduced)
