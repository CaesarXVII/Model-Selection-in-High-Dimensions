library(MASS)
p<-5
rho<-0.7
n<-1000
sigma<-matrix(0,p,p)
for(i in 1:p){for(j in 1:p){
sigma[i,j]<-rho^abs(i-j)}}

Mu<-c(2,4,6,8,10) # location vector
X<-mvrnorm(n , Mu, sigma)

beta<-c(3,1.5,0,2,0)
e<-rnorm(n, mean = 0, sd = 1)
SNR<-mean(X%*%beta)/sqrt(var(e))

Y_hat<-X%*%beta+e

index <- sample(1:n, size=0.5*n)

# Split data
y_train<- Y_hat[-index,]
x_train<-X[-index,]
y_test<- Y_hat[index,]
x_test<-X[index,]

index_sub_choose<-c(1:p)
sub_matrix <- matrix(data = NA,ncol = p,nrow = 2^p-1)
t=0
for(i in 1:5)
{
  index_matrix <- combn(index_sub_choose,i)
  for(j in 1:ncol(index_matrix))
  {
    t <- t+1
    index_sub <- index_matrix[,j]
    sub_matrix[t,c(index_sub)]  <-  1
  }
}

k<-nrow(sub_matrix)
cv <- matrix(data=NA,nrow = k,ncol = 1)

for(j in 1:k){
  Xsub    <-x_train[,which(sub_matrix[j,]==1)]
  betaMLE <-solve(t(Xsub)%*%Xsub)%*%t(Xsub)%*%y_train
  new_Y   <-x_test[,which(sub_matrix[j,]==1)]%*%betaMLE
  cv[j,]      <- t(y_test-new_Y)%*%(y_test-new_Y)
  
}

BEST_cv<-which(sub_matrix[which.min(cv),]==1)
BEST_cv
Xsub_cv<-x_train[,BEST_cv]
betaMLE_cv<-solve(t(Xsub_cv)%*%Xsub_cv)%*%t(Xsub_cv)%*%y_train
betaMLE_cv

RSS<-rep(0,k)
AIC<-rep(0,k)
k<-nrow(sub_matrix)
for(j in 1:k){
  Xsub<-as.matrix(X[,which(sub_matrix[j,]==1)])
  betaMLE<-solve(t(Xsub)%*%Xsub)%*%t(Xsub)%*%Y_hat
  new_Y<-Xsub%*%betaMLE
  for(i in 1:(n/2)){
    RSS[j]<-RSS[j]+(new_Y[i]-Y_hat[i])^2
  }
  AIC[j]<-RSS[j]/var(e)+2*ncol(Xsub)
}
BEST<-which(sub_matrix[which.min(AIC),]==1)
BEST
Xsub<-as.matrix(X[,BEST])
betaMLE<-solve(t(Xsub)%*%Xsub)%*%t(Xsub)%*%Y_hat
betaMLE

#################

cv <- matrix(data=NA,nrow = k,ncol = 1)
Exact<-data.frame(t(c(0,0)))
colnames(Exact)<-c("AIC","CV")
AverageN<-matrix(NA,nrow=100,ncol = 2)
colnames(AverageN)<-c("AIC","CV")
Correct<-data.frame(t(c(0,0)))
colnames(Correct)<-c("AIC","CV")

for(l in 1:100){
  
  ### SETTING ###
  n<-1000
  p<-5
  Mu<-c(2,4,6,8,10) # location vector
  sigma<-diag(5) # scale matrix as the identity
  X<-mvrnorm(n , Mu, sigma)
  beta<-c(3,1.5,0,2,0)
  e<-rnorm(n, mean = 0, sd = 1)
  Y_hat<-X%*%beta+e
  
  ### AIC ###    
  RSS<-rep(0,k)
  AIC<-rep(0,k)
  
  for(j in 1:k){
    Xsub<-as.matrix(X[,which(sub_matrix[j,]==1)])
    betaMLE<-solve(t(Xsub)%*%Xsub)%*%t(Xsub)%*%Y_hat
    new_Y<-Xsub%*%betaMLE
    for(i in 1:(n/2)){
      RSS[j]<-RSS[j]+(new_Y[i]-Y_hat[i])^2
    }
    AIC[j]<-RSS[j]/var(e)+2*ncol(Xsub)
  }
  BEST<-sub_matrix[which.min(AIC),]
  BEST[is.na(BEST)] <-0
  if(sum(BEST-c(1,1,0,1,0))==0){
    
    Exact[1]<-Exact[1]+1
    
  }
  if(sum((BEST[c(1,2,4)]-c(1,1,1)))==0){
    Correct[1]<-Correct[1]+1
  }
  AverageN[l,1]<-sum(BEST)
  
  
  ### CV ###
  index <- sample(1:n, size=0.5*n)
  y_train<- Y_hat[-index,]
  x_train<-X[-index,]
  y_test<- Y_hat[index,]
  x_test<-X[index,]
  
  for(j in 1:k){
    Xsub    <-x_train[,which(sub_matrix[j,]==1)]
    betaMLE <-solve(t(Xsub)%*%Xsub)%*%t(Xsub)%*%y_train
    new_Y   <-x_test[,which(sub_matrix[j,]==1)]%*%betaMLE
    cv[j,]      <- t(y_test-new_Y)%*%(y_test-new_Y)
    
  }
  
  BEST_cv<-sub_matrix[which.min(cv),]
  BEST_cv[is.na(BEST_cv)] <-0
  if(sum(BEST_cv-c(1,1,0,1,0))==0){
    
    Exact[2]<-Exact[2]+1
    
  }
  if(sum((BEST_cv[c(1,2,4)]-c(1,1,1)))==0){
    Correct[2]<-Correct[2]+1
  }
  AverageN[l,2]<-sum(BEST_cv)
  
}

Exact/100
Correct/100
colMeans(AverageN)

####################################################




load("C:/Users/user/Desktop/Spring2018/ModelSelection/Model-Selection-in-High-Dimensions/Practicals/Practical 4/data_leukemia_reduced.Rda")
Leukemia<-data_leukemia_reduced
Leukemia<-data.frame(Leukemia)
model<-glm(y~.,family=binomial("logit"),data=Leukemia,maxit=100)

plot(Leukemia[,1],Leukemia[,12])

library(glmnet)
X<-as.matrix(Leukemia[,2:11])
Y<-as.matrix(Leukemia[,1])
fit<-glmnet(X,Y,family = "binomial")
summary(fit)
pred<-predict(fit,X,type="class")
pred_coef<-predict(fit,type="coefficient")
cbind(predict(fit,X,type="class",s=2),Y)
