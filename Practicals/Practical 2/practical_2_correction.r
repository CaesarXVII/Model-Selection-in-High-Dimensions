###########################################
###                                     ###
###           PRACTICAL 2               ###
###                                     ###
###########################################

rm(list = ls())


### Exercise 1 and 2 ### 

# See correction on .rmd file or on the ebook, here just the R code necessary for the tasks.

## Part b (ex. 1)

hist(iris$Sepal.Width[iris$Species == "setosa"], main = "Histogram for Iris Setosa")

## Part c (ex. 1)

sub_joint = subset(iris,iris$Species == "virginica" & iris$Sepal.Width > 3)

sub_marg = iris$Sepal.Width[iris$Sepal.Width > 3]

result = dim(sub_joint)[1]/length(sub_marg)

result


### EXERCISE 3 ###

## Part a : load dataset and prepare variables


require(foreign)  # install foreign package if you do not have it yet


##### See section 1.6.2 e-book for information on the dataset. ####

# dat = read.spss("Zambia.SAV", add.undeclared.levels = "no")

dat = read.spss("Zambia.SAV")

# Construct system matrix

# The idea behind this exercise is to be aware that data cleaning is most of the times the real issue 
# with a real problem. It is sensitive to say that 80% of the work is cleaning and only 20% is modeling.

# Extract response variable i.e. HW70 Height for age standard deviation (according to WHO)
y = dat$HW70
y[y == 9996] = NA
y[y == 9997] = NA
y[y == 9998] = NA
y[y == 9999] = NA

# Revert tranformation (i.e. z-score)
y = y/100

# Variable 1: The calculated months of breastfeeding gives the duration of breastfeeding
x1 = dat$M5
x1[x1 == 94] = 0
x1[x1 == 97] = NA
x1[x1 == 98] = NA
x1[x1 == 99] = NA
x1[x1 > 40] = NA

# Variable 2: Age in months of the child
x2 = dat$HW1

# Variable 3: Age of the mother at birth
x3 = dat$V012 - dat$B8
x3[x3>45] = NA

# Variable 4: Body mass index (BMI) of the mother
x4 = dat$V445

x4 = x4/100  # no sense without this division

# Variable 5: Height of the mother in meters
x5 = dat$V438
x5[x5 == 9998] = NA
x5[x5 == 9999] = NA
x5[x5 < 1300] = NA
x5[x5 > 1900] = NA

x5 = x5/1000  # it was in mm, we need to transform from original

# Variable 6: Weight of the mother in kilograms
x6 = dat$V437

x6=x6/10 # we need to go back to Kg

# Variable 7: De facto region of residence

# Creating dummies (i.e. indicator functions) for each level of an existing factor enables
# to check the coefficients of each level in a possible future model estimation

x7 = as.factor(dat$V101)


x7 = model.matrix(~x7-1)

dim(x7)

# Variable 8: Mother highest education level attended
x8 = as.factor(dat$V106)
x8 = model.matrix(~x8-1)

dim(x8)

# Variable 9: Wealth index factor score
x9 = dat$V191

# Variable 10: Weight of child at birth given in kilograms with three implied decimal places
x10 = dat$M19
x10[x10 == 9996] = NA
x10[x10 == 9997] = NA
x10[x10 == 9998] = NA
x10[x10 == 9999] = NA
x10 = x10/1000

# Variable 11: Child Sex
x11 = dat$B4

# Variable 12: Preceding birth interval is calculated as the difference in months between 
# the current birth and the previous birth

x12 = dat$B11
x12[x12 > 125] = NA

# Variable 13: Drinking Water
x13 = dat$V113
x13 = model.matrix(~x13-1)
x13 = x13[,c(2,3,4,8,9,13,17,18)]

dim(x13)

levels(x13)

mat.sys = na.omit(cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13))
dim(mat.sys)[2]


# Number of regressor
p = dim(mat.sys)[2]

# Construct X and Y
y = mat.sys[,1]
X = mat.sys[,2:p]

# Create a dataframe

data_zambia = cbind(y,X)

data_zambia = data.frame(data_zambia)



## Part b: Associate proper names to each variable (hint: look at the comments in the r chunk).

View(X)

colnames(data_zambia) = c("Height for age sd", "Breastfeeding duration (months)","Age of the child (months)", "Age of the mother (years)", "BMI mother", "Heigth mother (meter)", "Weight mother (kg)", "Region:Central", "Region:Copperbelt", "Region:Eastern", "Region:Luapula", "Region:Lusaka", "Region:Northern", "Region:Northwestern", "Region:Southern", "Region:Western", "Ed:No education", "Ed:Primary", "Ed:Secondary", "Ed:Higher", "Wealth index factor score", "Child weight at birth (kg)", "Child sex", "Interval between births","Water:Piped into dwelling", "Water:Piped to yard/plot", "Water:Public tap/standpipe", "Water:Protected well", "Water:Unprotected well", "Water:River/dam/lake/ponds/stream/canal/irrigation channel", "Water:Bottled water", "Water:Other")

View(data_zambia)

## Part c: Perform a linear regression on all the available variables.

attach(data_zambia)


lm_zambia = lm(`Height for age sd` ~ . -`Region:Central`- `Ed:No education`, data = data_zambia)

# We take off two levels to avoid multicollinearity. This should always be done when you create dummies.

summary(lm_zambia) # read the output understand the benchmark of the factor

lm_zambia_full = lm(`Height for age sd` ~ . , data = data_zambia)

summary(lm_zambia_full) #here it is R who choses the benchmark for the factors (i.e. NA variables)

detach(data_zambia)


## Part d: Reduce the number of covariates (e.g. using the t-test) and add some interactions. 
#          Perform a linear regression on the new dataset.

attach(data_zambia)

# Eliminate variables with t-test in a stepwise manner (fixed alfa = 0.05 in this case)

model_zambia_reduced = lm(`Height for age sd` ~ ., data = data_zambia[,c(1:2,4,9:16,21:23)])


summary(model_zambia_reduced) # notice what is happening to the age of the mother variable 


# Introduce one interaction in the reduced model. We start with the childsex factor.

model_zambia_int = lm(`Height for age sd` ~ . + `Breastfeeding duration (months)`*`Child sex`, data = data_zambia[,c(1:2,4,9:16,21:23)])


summary(model_zambia_int) #We take out the interaction from the model as it is not significant

#### Remember: the hierarchical effect states that anytime you add an interaction also the marginal effects

#### should be part of your model

detach(data_zambia)


# Other available procedures for a first model selection in this specific case:

# (1) VIF (variance inflation factor) for avoiding multicollinearity, 

# (2) Automatic Stepwise procedures (e.g. forward and backward) 

# (3) Exhaustive search (See practical 3 exercises)

# Example with an automatic stepwise procedure

help("step")

stepwise_procedue = step(lm_zambia_full,direction = "backward") #or forward

# This procedure evaluates, given a criterion, a sequence of variables stopping when
# the criterion is increasing

## Part e: Analyse your chosen estimated model
#  with a residual analysis (e.g. residuals vs fitted plot, normal QQ plot etc.).

# Validate your model looking at residuals vs fitted plot and normal QQ plot

plot(model_zambia_reduced, which = 1)  # Residuals vs fitted: no particular structure

plot(model_zambia_reduced, which = 2) 

# Normal QQ plot: We observe right tail which is not compatible with a normal assumption


### EXERCISE 4 ###

## Part a: load the data

leukemia_big <- read.csv("http://web.stanford.edu/~hastie/CASI_files/DATA/leukemia_big.csv")


## Part b: Create the response variable y according to the number of ALL and AML patients. 
#          In the same fashion create the matrix X of independent variables. 


leukemia_mat = as.matrix(leukemia_big)

dim(leukemia_mat)

leukemia_mat = t(leukemia_mat) #this is the design matrix for the analysis

# Generate the 0 and 1 values for the two different categories: there are 20 ALL, 14 AML, 27 ALL and

# 11 AML for a total of 47 ALL and 25 AML.


# Given the above excerpt from the cancer society, I have decided to code ALL as 1 and AML as 0 since

# doctors are interested in knowing the characteristics which differentiate ALL from AML in order to

# understand if we can use standard treatment or a more aggressive one.


y = c(rep(1,20),rep(0,14), rep(1,27), rep(0,11))  #the response vector

length(y)

X = leukemia_mat

dim(X)


## Part c: - Choose the correct exponential family for this situation and perform a GLM on the data. 
#            Comment on the results that you obtain. 

model_glm = glm(formula = y ~ X,family = "binomial")


summary(model_glm)  #singularity issues in the IWLS algorithm of GLM. It is impossible to invert the matrix.

### SEE THEORETICAL DERIVATION on IWLS algorithm to understand where the problem lies...

# The binary Lasso is a possible way to solve the issue and have an actual estimate. See glmnet package.









