################################################
###                                          ###
###        PRACTICAL 6 - CORRECTION          ###
###                                          ###
################################################

rm(list=ls())

### EXERCISE 1 - Sure Independence Screening ### 

require(SIS) #install it if you do not have 



## Part a

leukemia_big <- read.csv("http://web.stanford.edu/~hastie/CASI_files/DATA/leukemia_big.csv")

leukemia_mat = as.matrix(leukemia_big)

leukemia_mat = t(leukemia_mat) #this is the design matrix for the analysis

y = c(rep(1,20),rep(0,14), rep(1,27), rep(0,11))  #the response vector

X = leukemia_mat

## Part b

data_leukemia = data.frame(cbind(y,X))

ind = 1:72


set.seed(11)

index = sample(x = ind,size = 36,replace = F) #Results depend on the split, source of randomness


train_set = data_leukemia[index,]

test_set = data_leukemia[-index,]



X_train = as.matrix(train_set[,2:7129])

y_train = train_set[,1]


X_test = as.matrix(test_set[,2:7129])

y_test = test_set[,1]

## Part c

model_leuk=SIS(X_train, y_train, family='binomial') #SCAD is default

model_leuk = SIS(X_train, y_train, family='binomial',penalty = 'lasso')

model_leuk = SIS(X_train, y_train, family='binomial',penalty = 'MCP')



model_leuk_scad$coef.est

model_leuk_mcp$ix

##  ALTERNATIVE SOLUTION: SIS package dataset 

train_set = data(leukemia.train)

test_set = data("leukemia.test")

train_set = as.data.frame(train_set)

test_set = as.data.frame(test_set)

dim(test_set)


X_train = as.matrix(leukemia.train[,1:7129])

y_train = leukemia.train[,7130]


X_test = as.matrix(leukemia.test[,1:7129])

y_test = leukemia.test[,7130]

# Model Phase

model_leuk_scad=SIS(X_train, y_train, family='binomial') #SCAD is default

model_leuk_lasso = SIS(X_train, y_train, family='binomial',penalty = 'lasso')

model_leuk_mcp = SIS(X_train, y_train, family='binomial',penalty = 'MCP')

# Prediction Phase

pred_scad=predict(model_leuk_scad,X_test,type="class")

sum(pred_scad == y_test)/34 #0.7941176 

pred_lasso=predict(model_leuk_lasso,X_test,type="class")

sum(pred_lasso == y_test)/34 #  0.7941176

pred_mcp=predict(model_leuk_mcp,X_test,type="class")

sum(pred_mcp == y_test)/34 # 0.7941176

# We got the same results for every method. 

model_leuk_mcp$ix

model_leuk_lasso$ix

model_leuk_scad$ix


# SCAD and MCP are near in terms of coefficient estimates but Lasso has one regressor more. 

model_leuk_mcp$coef.est

model_leuk_mcp$coef.est




### EXERCISE 2 - Programming the PC-simple Algorithm ###

## Simulation Setting

require(MASS)

set.seed(11)

n = 100

p = 10 

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

dim(X)

beta = c(3,1.5,0,2,rep(0,6))


y = X%*%beta + rnorm(n,0,1) #for us sigma is 1

## Part a

# Fix alfa at 5% for all the exercise

v_cor = cor(x = X,y = y)

q=rep(0,10)


for (i in 1:10) {
  
  if(sqrt(n-3)*abs(1/2*log((1 + v_cor[i])/(1-v_cor[i]))) > 1.96 ) {
    
    q[i]=1
    
  }
  
}

A_0 = X #Active set zero

# With the first step we select the first 5 variables, we reject H0 so that we have evidence that the

# correlation is different from 0. 

A_1 = X[,1:5] #new active set

## Part b

# We can now create the 20x1 vector of first order partial correlation and then apply previous formula


P = rep(0,25)

P = matrix(P,nrow = 5,ncol = 5)



for (z in 1:5) {
  
  for (i in 1:5) {
    
    if (i != z ) {
      
      
      mod_1 = lm(A_1[,z] ~ A_1[,i])
      
      e_1 = A_1[,z] - cbind(rep(1,n),A_1[,i])%*%mod_1$coefficients
      
      mod_2 = lm(y ~ A_1[,i])
      
      e_2 =  y - cbind(rep(1,n),A_1[,i])%*%mod_2$coefficients
      
      
      P[i,z] = cor(e_1,e_2) 
      
      
    }
    
  }
  
  
}

P = P + diag(5)

v_cor = c(P) #vectorization

# fix alfa at 5% and test the sample correlation.

q=rep(0,25)


# Cardinality of the active set is now 1 because we are evaluating partial correaltions of order 1

for (i in 1:25) {
  
  if(sqrt(n-4)*abs(1/2*log((1 + v_cor[i])/(1-v_cor[i]))) > 1.96 ) {
    
    q[i]=1
    
  }
  
}

q # variables 3 and 5 are eliminated from the active set

A_2 = X[,c(1,2,4)]

## Part c

# Now we need to check 2nd order partial correlation to discriminate.

# Note: we could also update estimation for partial correlation in order to speed up the computations

# but in this easy example is not necessary. 

# VARIABLE 1 ACTIVE SET A_2

mod_1 = lm(A_2[,1] ~ A_2[,2] + A_2[,3])

e_1 = A_2[,1] - cbind(rep(1,n),A_2[,2],A_2[,3])%*%mod_1$coefficients

mod_2 = lm(y ~ A_2[,2] + A_2[,3])

e_2 =  y - cbind(rep(1,n),A_2[,2],A_2[,3])%*%mod_2$coefficients

# Cardinality of active set now is two!

sqrt(n-5)*abs(1/2*log((1 + cor(e_1,e_2))/(1-cor(e_1,e_2)))) > 1.96

# VARIABLE 2 ACTIVE SET A_2

mod_1 = lm(A_2[,2] ~ A_2[,1] + A_2[,3])

e_1 = A_2[,2] - cbind(rep(1,n),A_2[,1],A_2[,3])%*%mod_1$coefficients

mod_2 = lm(y ~ A_2[,1] + A_2[,3])

e_2 =  y - cbind(rep(1,n),A_2[,1],A_2[,3])%*%mod_2$coefficients

# Cardinality of active set now is two!

sqrt(n-5)*abs(1/2*log((1 + cor(e_1,e_2))/(1-cor(e_1,e_2)))) > 1.96

# VARIABLE 3 ACTIVE SET A_2

mod_1 = lm(A_2[,3] ~ A_2[,1] + A_2[,2])

e_1 = A_2[,3] - cbind(rep(1,n),A_2[,1],A_2[,2])%*%mod_1$coefficients

mod_2 = lm(y ~ A_2[,1] + A_2[,2])

e_2 =  y - cbind(rep(1,n),A_2[,1],A_2[,2])%*%mod_2$coefficients

# Cardinality of active set now is two!

sqrt(n-5)*abs(1/2*log((1 + cor(e_1,e_2))/(1-cor(e_1,e_2)))) > 1.96

# Conclusions: all conditions are true, then the algorithm will not move anymore and we have 

# our final model with the original variables c(1,2,4) which is also the exact one. 

### EXERCISE 3 - Classification and Regression Trees ###

### CLASSIFICATION TREE - IRIS ###

## Part a

require(rpart)

data_iris = iris

ind = 1:150

index = sample(x = ind,size = 100,replace = F)

train= data_iris[index,]

test= data_iris[-index,]

## Part b

attach(train) 

fit = rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
            data=train,
            method="class")


printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree for Iris")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# Not easy to read and understand, better to use rpart.plot

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

## Part c

# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, main="Pruned Classification Tree for Iris")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree.ps", 
     title = "Pruned Classification Tree for Iris")

fancyRpartPlot(pfit)

Prediction <- predict(fit, test, type = "class") #pruned or not pruned trees are equal in this example

t = Prediction == test$Species

sum(t)/length(test$Species)


### REGRESSION TREE - ZAMBIA ###

load("malnutrion_zambia_cleaned.Rda")


data_zambia = data_zambia[,c(1:7,21,22,24)] #exclude the factors from the analysis

X = as.matrix(data_zambia[,2:10])

y = data_zambia[,1]


## Part a

ind = 1:1927

index = sample(x = ind,size = 1284,replace = F)

data_zambia_train= data_zambia[index,]

data_zambia_test= data_zambia[-index,]

## Part b

# grow tree 

attach(data_zambia_train)

fit <- rpart(`Height for age sd`~ 
               `Breastfeeding duration (months)` + `Age of the child (months)`+`Age of the mother (years)`+`BMI mother`+`Heigth mother (meter)`+`Weight mother (kg)`+`Wealth index factor score`+`Child weight at birth (kg)`+`Interval between births`, 
             method="anova", data=data_zambia_train)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

#plot the tree
plot(fit, uniform=TRUE, main="Regression Tree for Zambia ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# Nicer graph
fancyRpartPlot(fit)

## Part c

# prune the tree

fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

fit$cptable

pfit<- prune(fit, cp=0.02406722) # from cptable 

# plot the pruned tree 
plot(pfit, uniform=TRUE, main="Pruned Regression Tree for Zambia")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

fancyRpartPlot(pfit) 

# Notice the different tree after pruning: Age of the child and Height of the mother stays relevant

# while other variables disappear. 

Prediction <- predict(pfit, data_zambia_test, type = "vector") 

# We can use mean squared error to evaluate the predictions

mse = sum((Prediction - data_zambia_test$`Height for age sd`)^2)/length(Prediction)

detach(data_zambia_train)

# Now we should find a benchmark to understand how well is doing the Regression Tree

# In this case we will use linear regression plus model selection thanks to FDR rule.

X_train = as.matrix(data_zambia_train[,2:10])


mod_zambia = lm(data_zambia_train[,1] ~ X_train, data = data_zambia_train)

object = summary(mod_zambia)

# Fix a level q = 0.05 of FDR (see Hochberg&Benjamini), order the p-values and we reject three H0 thus

# selecting variables: Breastfeeding duration, Weight at birth and wealth index factor scores.

mod_zambia_final = lm(data_zambia_train[,1] ~ X_train[,c(1,7,8)], data = data_zambia_train)

summary(mod_zambia_final)

X_test = as.matrix(data_zambia_test[,c(2,8,9)])

dim(X_test)

predictions_linear = cbind(rep(1,643),X_test)%*%mod_zambia_final$coefficients

mse_linear = sum((predictions_linear - data_zambia_test$`Height for age sd`)^2)/length(Prediction)

# Similar to the one of the Regression Tree so there is not a big improvement.


