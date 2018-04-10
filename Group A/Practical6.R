### ====================================================================================
### ============================== TP6 - Model Selection  ==========================================
### ====================================================================================


### ===================== Set working space
# Clear objects
rm(list = ls())
# Clear console
cat("\014")


######################## EXERCISE 1 

########## a: load the data

leukemia_big <- read.csv("http://web.stanford.edu/~hastie/CASI_files/DATA/leukemia_big.csv")

########## b: split the data
dim(leukemia_big)

tran<-t(leukemia_big) #to have the obs in rows

dim(tran)

y = c(rep(1,20),rep(0,14), rep(1,27), rep(0,11))  #the response vector
length(y)
data<-cbind(y,tran)
dim(data)

set.seed(12)   
train_ind = sample(seq_len(nrow(data)),size = 36)  # to Randomly split the data
train =data[train_ind,] #creates the training dataset 
test=data[-train_ind,]  #creates the test dataset
dim(train)
dim(test)

train <- as.matrix(train)
test<-as.matrix(test)


########## c: SIS
require("SIS")

model1<-SIS(train[,2:7129], train[,1], family = "binomial",penalty = "SCAD",tune="aic")
model1$ix
model1$coef.est

model2<-SIS(train[,2:7129], train[,1], family = "binomial",penalty = "lasso",tune="aic")
model2$ix
model2$coef.est

model3<-SIS(train[,2:7129], train[,1], family = "binomial",penalty = "MCP", tune="aic")
model3$ix
model3$coef.est


p1<-predict(model1, test[,2:7129], lambda = model1$lambda,type="class")
test.error1<-mean(p1 != test[,1])

p2<-predict(model2, test[,2:7129], lambda = model2$lambda,type="class")
test.error2<-mean(p2 != test[,1])

p3<-predict(model3, test[,2:7129], lambda = model3$lambda,type="class")
test.error3<-mean(p3 != test[,1])







############################################## EXERCISE 2  ###
require(mvtnorm)
require(MASS)

##  1) - 2) - 3) - 4)
set.seed(11)
n = 1000
p = 10
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

X <- mvrnorm(n,mu,sigma)
dim(X)
beta <- c(3,1.5,0,2,rep(0,6))
y_hat <- X%*%beta + rnorm(n,0,1)

##### a)
alpha <- 0.05

## M1 is the set of indices for which the correlation of y and X_j is 0
## if corr(y, X_j) = 0 !=> corr.hat(y, X_j) = 0. We use Fisher's Z-transf.

## compute the correlation between the response and all the predictors
cor <- cor(y_hat, X)
## Fisher's Z-transformation
Z <- 1/2*log((1+cor)/(1-cor))

## H0: corr(y, X_j) = 0 is rejected when expression > qnorm(1-alpha/2) 
## for m=1, |M_c|=p
sqrt(n-p-3)*Z > qnorm(1-alpha/2)

## We reject H0 for 1 to 7 (i.e. we reject the fact that the correlation is 0)
## Since M1 is the set of indices for which the correlation of X_j and y is 0,
## M1 is composed of {8,9,10}
M1 <- c(8,9,10)



############################################## Exercice 3 


# Part 1


#### a)
require(rpart)
require(rpart.plot)
iris 
attach(iris)
data(iris)

##  the sample size
smp_size <- floor(2/3 * nrow(iris))

train_iris <- sample(seq_len(nrow(iris)), size = smp_size)

train <- iris[train_iris, ]
test <- iris[-train_iris, ]



#### b)

iris$Species.f = factor(iris$Species, labels=c("1", "2", "3"))
fit <- rpart(Species ~ Petal.Width + Petal.Length + Sepal.Width + Sepal.Length, method="class", data=iris)
printcp(fit)

plot(fit,uniform=TRUE, main="Classification Tree for iris") 
text(fit, use.n=TRUE, all=TRUE, cex=.5) 

prediction <- predict(fit)

prediction <- predict(object = fit,data = test,type = "class")


# Part 2

### a)

data_zambia <- read.table(choose.files(), header=TRUE)
data_zambia <- data_zambia[,c(1:7,21,22,24)]
n_2 <- nrow(data_zambia)
index <- sample(1:n_2, size=0.6*n_2)


data_train<- data_zambia[index,]
data_test<- data_zambia[-index,]

#### b)
model.zambia <- rpart(formula = `Height for age sd`~.,data = data_train,minbucket = 1)
plot(model.zambia)
plot(model.zambia, uniform = TRUE,compress = TRUE,margin = 0.11)
text(model.zambia, splits = TRUE,cex = 0.8)

### c)

prediction_zambia <- predict(object = model.zambia,newdata = data_test)
x  <- data_test[,1]
d    <- as.matrix(x-prediction_zambia)
rss <- t(d)%*%d




