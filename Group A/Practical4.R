### ====================================================================================
### ============================== TP4 ==========================================
### ====================================================================================


### ===================== Set working space ========================================

# Clear objects
rm(list = ls())
# Clear console
cat("\014")
# Set working directory
setwd("/Users/TaliaKimber/Documents/Unige/2_MA/Model_Selection/Model_Selection/Group A")

par(mfrow = c(1,1))



### ===================== EX.3 =============================================
## ========================================================================

## a)
# Data
load("data_leukemia_reduced.Rda")
dat <- data_leukemia_reduced
attach(dat)
# GLM fit with one predictor
fit.glm = glm(formula = y ~ V457,family = "binomial")
summary(fit.glm) 

## b)
y.hat <- fit.glm$fitted
c <- 0.5
for (i in 1: length(y)){
  if (y.hat[i] >= c)
    y.hat[i] = 1
  else y.hat[i] = 0
}
TP <- numeric(1)
FP <- numeric(1)
TN <- numeric(1)
FN <- numeric(1)

for (i in 1: length(y)){
  if (y.hat[i] == 1 & y[i] == 1){
    TP <- TP + 1 # True Positive
  }
  else if (y.hat[i] == 1 & y[i] == 0){
    FP <- FP + 1 # False Positive
  }
  else if (y.hat[i] == 0 & y[i] == 0){
    TN <- TN + 1 # True Negative
  }
  else if (y.hat[i] == 0 & y[i] == 1){
    FN <- FN + 1 # False Negative
  }
}

length(y) == TP + FP + TN + FN

## c)
# Cut-off grid
c.seq <- seq(0,1,length.out = 10)
# Predicted values : number of rows is the number of observations
# number of columns is the number of c's
y.hat.seq <- matrix(NA, nrow = length(y), ncol = length(c.seq))
# All columns start with the fitted values of GLM. Then use threshold
y.hat.seq[, 1:length(c.seq)] <- fit.glm$fitted

for (k in 1:length(c.seq)){
  for (i in 1: length(y)){
    if (y.hat.seq[i,k] >= c.seq[k])
      y.hat.seq[i,k] = 1
    else y.hat.seq[i,k] = 0
  }
}

# Define the proportion of TP and FP per values of c
TP.seq <- matrix(0, nrow = 1, ncol = length(c.seq))
FP.seq <- matrix(0, nrow = 1, ncol = length(c.seq))
for (k in 1:length(c.seq)){
  for (i in 1: length(y)){
    if (y.hat.seq[i,k] == 1 & y[i] == 1){
      TP.seq[k] <- TP.seq[k] + 1 # True Positive
    }
    else if (y.hat.seq[i,k] == 1 & y[i] == 0){
      FP.seq[k] <- FP.seq[k] + 1 # False Positive
    }
  }
}


plot(y = TP.seq * 100/72, x = FP.seq* 100/72, type = "line", main ="ROC Curve",
     xlab = "False Positive rate, P(FP) % ", ylab = "True Positive rate, P(TP) % ", col = 'blue')
abline(0, 1, col= "black")


## d)
require("pROC")
roc(y,V457, percent =T, plot = T, smooth = T)


detach(dat)

