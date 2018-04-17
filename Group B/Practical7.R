## Exercise 1

install.packages("glmnet")
library("glmnet")
load("malnutrion_zambia_cleaned.Rda")

x_zambia<-as.matrix(data_zambia[,c(2:7,21,22,24)])
y_zambia<-data_zambia[,1]

ind<-sample(1:length(y_zambia),round(length(y_zambia)/3))
x.test<-x_zambia[ind,]
x.train<-x_zambia[-ind,]
y.test<-y_zambia[ind]
y.train<-y_zambia[-ind]

#1.a
zambia.ridge<-glmnet(x.train,y.train,alpha=0,standardize=TRUE)
plot(zambia.ridge,xvar="lambda",label = TRUE)
# as lambda increases, scale of the coefficient is decreasing.

#1.b
zambia.cv.ridge<-cv.glmnet(x.train,y.train,alpha=0,nfolds = 10,standardize=TRUE)
plot(zambia.cv.ridge)
range(zambia.cv.ridge$lambda) # default range of lambda is 0.04866511 to 433.41832598


#1.c
ridge.pred.cv<-predict(zambia.cv.ridge,x.test,s="lambda.1se",type="response")
ridge.coef<-coef(zambia.cv.ridge, s = "lambda.1se")

zambia<-data.frame(cbind(y.train,x.train))
zambia.lm<-lm(y.train~.,data = zambia)
lm.coef<-zambia.lm$coefficients
lm.pred<-predict(zambia.lm,data.frame(x.test))

coef<-cbind(ridge.coef,lm.coef)
colnames(coef)<-c("ridge","lm")
coef

# The ridge regression includes all the covariate. It is not for model selection.
# For the ridge regression compare to lm, The ridge estimation has smaller coefficient of covariate than 
# the one for lm.

#2.1
zambia.lasso<-glmnet(x.train,y.train,alpha=1,standardize=TRUE)
plot(zambia.lasso,xvar="lambda",label = TRUE)
# as lambda increases, the number of covariate is decreasing. model selecton

#2.2
zambia.cv.lasso<-cv.glmnet(x.train,y.train,alpha=1,nfolds = 10,standardize=TRUE)
plot(zambia.cv.lasso)
range(zambia.cv.lasso$lambda) # default range of lambda is 0.04866511 to 433.41832598
best.lambda<-zambia.cv.lasso$lambda.1se # with this value of lambda we take 5 covariates

#2.c
lasso.pred.cv<-predict(zambia.cv.lasso,x.test,s="lambda.1se",type="response")
lasso.coef<-coef(zambia.cv.lasso, s = "lambda.1se")[-which(lasso.coef==0)]

# we select Breastfeeding duration (months),Heigth mother (meter),Weight mother (kg),
# Wealth index factor score and Child weight at birth (kg)

x.train.sub<-x.train[,c(1,5:8)]
zambia.2<-data.frame(cbind(y.train,x.train.sub))
zambia.lm.lasso<-lm(y.train~.,data = zambia.2)
lm.coef<-zambia.lm.lasso$coefficients

coef<-cbind(lasso.coef,lm.coef)
colnames(coef)<-c("lasso","lm")
coef

# The lasso can reduce the number of covariates to include in the model, which means it is model selection method.
#  For the lasso regression compare to lm, The lasso estimation has smaller coefficient of covariate than 
# the one for lm.



