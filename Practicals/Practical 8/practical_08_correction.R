################################################
###                                          ###
###        PRACTICAL 8 - CORRECTION          ###
###                                          ###
################################################

### EXERCISE 1 ###


## Part a - Simulation Setting

require(MASS)

set.seed(11)

beta = c(4,2,-4,-2,rep(0,996))

## Part b

n = 200 

p = 1000 

# Three values needed for rho = (0 0.2 0.5)

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

## Part c


y = X%*%beta + rnorm(n,0,1) #for us sigma is 1


## Part d - Compare solutions path

# Paths will be presented for gamma = (2, default values, 5)

# The value 1.5 for gamma is not possible for SCAD while 3 and 3.7 are the default value for MCP and Scad respectively

require(ncvreg)

help("ncvreg")

# Default gamma

model_scad = ncvreg(X = X,y = y,family = "gaussian",penalty = "SCAD") #gamma default is 3.7

model_mcp = ncvreg(X = X,y = y,family = "gaussian",penalty = "MCP") #gamma default is 3

model_lasso = ncvreg(X = X,y = y,family = "gaussian",penalty = "lasso")


# Gamma = 2

model_scad_2 = ncvreg(X = X,y = y,family = "gaussian",penalty = "SCAD",gamma = 2.1) #no scad for gamma lower than 2

model_mcp_2 = ncvreg(X = X,y = y,family = "gaussian",penalty = "MCP", gamma = 2)


# Gamma = 5

model_scad_5 = ncvreg(X = X,y = y,family = "gaussian",penalty = "SCAD", gamma = 5)

model_mcp_5 = ncvreg(X = X,y = y,family = "gaussian",penalty = "MCP", gamma = 5)




par(mfrow=c(1,3))

# gamma = 2

plot(model_lasso,main="Lasso")
plot(model_scad_2, main="Scad - Gamma 2.1")
plot(model_mcp_2, main="MCP - Gamma 2")

par(mfrow=c(1,3))

#default gamma

plot(model_lasso,main="Lasso")
plot(model_scad,main="Scad - Gamma 3.7")
plot(model_mcp, main="MCP - Gamma 3")

par(mfrow=c(1,3))

# gamma = 5

plot(model_lasso,main="Lasso")
plot(model_scad_5,main="Scad - Gamma 5")
plot(model_mcp_5, main="MCP - Gamma 5")


# Comments: 

# 1) We can already see that there is a non convex behavior of Scad and MCP while Lasso path is more 

# smoothed.

# 2) As gamma goes to plus infinity we notice that we go back to lasso path (smoothing procedure). 

# 3) The bias is minimized as gamma approaches his minimum value.


## We can also inspect adaptive lasso function within the parcor package. Unfortunately there is no plot as lambda 

## varies that is why we have not included it in our previous graphs. 


require(parcor)


help("adalasso")

model_adaptive = adalasso(X = X,y = y)

model_adaptive$coefficients.adalasso[1:4] #less biased

model_adaptive$coefficients.lasso[1:4]








