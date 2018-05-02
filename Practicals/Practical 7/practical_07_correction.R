################################################
###                                          ###
###        PRACTICAL 7 - CORRECTION          ###
###                                          ###
################################################

rm(list=ls())

### EXERCISE 1 - Ridge Regression ###

require(glmnet)

load("malnutrion_zambia_cleaned.Rda")


data_zambia = data_zambia[,c(1:7,21,22,24)] #exclude the factors from the analysis

X = as.matrix(data_zambia[,2:10])

y = data_zambia[,1]

## Part a

# Ridge penalty is obtained with parameter alpha = 0

mod_pen_L2 = glmnet(X,y,family = "gaussian",alpha = 0)

plot(mod_pen_L2, xvar="lambda") #more smoothed, no model selection property just shrinkage

mod_pen_L2$beta #shrinked but all present

## Part b

# Selection based on 10 fold CV

cv.zero_L2 <- cv.glmnet(X,y, alpha=0) #a way in which you can select lambda (10k CV)

cv.zero_L2$lambda.min

plot(cv.zero_L2) 

# The two vertical lines are respectively: the value of lambda that minimizes the CV error curve and 

# the value lambda.1SE which is one standard error from lambda.min on the right.


## Part c

# Easier with predict rather than check all the order based on the single value of lambda

### MESSAGE FROM HASTIE AND TIBSHIRANI ###

# Use directly lambda.min it is prone to errors

# WARNING: use with care. Do not supply a single value for lambda (for predictions after CV use predict() instead). 

# Supply instead a decreasing sequence of lambda values. glmnet relies on its warms starts for speed, and its often 

# faster to fit a whole path than compute a single fit.


coef_L2_min = predict(cv.zero_L2,type="coefficient",s = "lambda.min")

coef_L2_1SE = predict(cv.zero_L2,type="coefficient")  #default is lambda.1SE

model_linear = lm(y ~ .,data = data_zambia[,c(2:10)])

summary(model_linear)

# We observe the shrinkage property of L2 norms penalties, the coefficients are shrinked to achieve a better 

# compromise between bias and variance. For supplementary theory see Stein's estimators.


### EXERCISE 2 - LASSO ###

## Part 1

# Lasso penalty is obtained by setting alpha equal to one

mod_pen = glmnet(X,y,family = "gaussian",alpha = 1)


mod_pen$beta #order of the lasso

mod_pen$lambda

plot(mod_pen, xvar="lambda") #plot coefficients as lambda varys, use lambda found below to select. 

## Part 2

# Selection based on 10 fold CV

cv.zero_mod <- cv.glmnet(X,y, alpha=1) #a way in which you can select lambda (10k CV)

cv.zero_mod$lambda.min

plot(cv.zero_mod) 

# The two vertical lines are respectively: the value of lambda that minimizes the CV error curve and 

# the value lambda.1SE which is one standard error from lambda.min in the direction of a more 

# parsimonious model.

## Part 3

mod_pen = glmnet(X,y,family = "gaussian",alpha = 1,lambda = 0.00201) #wrong, do not use this way!

# The right way is to use the predict function

coef_L1_min = predict(cv.zero_mod,type="coefficient",s = "lambda.min")

coef_L1_1SE = predict(cv.zero_mod,type="coefficient")  #default is lambda.1SE

# As you can notice, lambda min tends to select more regressors so it is less parsimonious then 1.SE


model_linear = lm(y ~ .,data = data_zambia[,c(2,3,6:9)])

summary(model_linear)


# Lasso estimates are known to be biased and we are not even sure about the signs if dimensions of predictor space 

# is greater than 2. There are methods which are called "debiasing the lasso" that you can search if you are 

# interested by this topic. Of course here we are using real data so we have always to be aware of the assumptions

# of each model that we are fitting. 



