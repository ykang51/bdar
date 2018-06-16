####################
#### Unit 5 - Introduction to Statistical Learning
#### R codes
####################

library(data.table); library(ggplot2)
# advertising data, skip the frist column
advertise = data.table::fread("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv",
                              verbose = T, stringsAsFactors = F, data.table = F, drop = 1) 

# There are some updates on the origianl CSV files. Here we rename the columns
colnames(advertise) = c("TV", "Radio", "Newspaper", "Sales")

# Plot (TV, Radio, Newspaper) by Sales, with regression line 
gp = Map(function(x) ggplot(advertise, aes(advertise[,x], Sales) ) + geom_point() + xlab(x) + stat_smooth(method = lm),
         colnames(advertise[,-4]) )
gridExtra::marrangeGrob(gp, ncol = 3, nrow= 1)


# log-likelihood function for later MLE
LL = function(beta0, beta1){
  # Residuals
  resid = advertise$Sales - (advertise$TV * beta1) - beta0
  # Likelihood for the residuals 
  ll = dnorm(resid, 0, 1); 
  # Sum of the log likelihoods for all of the data points 
  return(-sum(log(ll)));
}

# Maximum likelihood estimation of parameter values
stats4::mle(LL, start = list(beta0 = 0, beta1 = 0))
# Or, just use lm() to make our life easier. 
lm(Sales ~ TV, advertise) # Note that lm() in R uses QR factorization instead

ad_fit = lm(Sales ~ TV + Radio + Newspaper, data = advertise)
summary(ad_fit); ad_fit

resid = advertise$Sales - predict(ad_fit, advertise) # or just residuals(ad_fit)
qqnorm(resid); qqline(resid)
# Or normality tests on residuals
nortest::lillie.test(resid) # Kolmogorov-Smirnov normality test

# RMSE for training dataset
rmse_lm = function(f, d){
  m = lm(formula = f, data = d, na.action = na.omit);
  return(  round(sqrt(mean(resid(m)^2)),4)) ;
}
# e.g.rmse_lm(Sales ~ Radio, advertise)

# Leave-one-out CV RMSE for lm()
LOOCV_lm_rmse = function(f, d){
  numOfRec = nrow(d);
  rmse_vec = rep(0,numOfRec); # n RMSEs
  reponse_var = all.vars(f)[1]; # Get the name of reponse variable
  
  for(i in 1:numOfRec){
    m = lm(formula=f, data=d[- i,],na.action = na.omit);
    rmse_vec[i] = (d[[reponse_var]][i]) - predict(m,newdata=d[i,]);
  }
  return( paste("LOOCV RMSE for lm(", format(f),") =", 
                round(sqrt(mean(rmse_vec ^ 2)),4)) );
}

# A bit more "functional-style" version of LOOCV_lm_rmse()
LOOCV_lm_rmse = function(f, d){
  errs = sapply(1:nrow(d), FUN = function(k){
    reponse_var = all.vars(f)[1]; # Name of reponse variable
    m = lm(f, d[- k,], na.action = na.omit)
    return((d[[reponse_var]][k]) - predict(m, newdata = d[k,]))
  })
  return(round(sqrt(mean(errs ^ 2)),4))
}

Map(function(x) LOOCV_lm_rmse(x, advertise), 
    c(Sales ~ TV, Sales ~ Radio, Sales ~ Newspaper, Sales ~ Radio + TV, Sales ~ TV + Radio + Newspaper)) 

# Or we can simply use functions in package "caret"
Map(function(f) 
  caret::train(f , data = advertise, method = "lm", 
               trControl = caret::trainControl(method = "LOOCV")),
  c(Sales ~ TV, Sales ~ Radio, Sales ~ Newspaper, Sales ~ Radio + TV, Sales ~ TV + Radio + Newspaper)
)
# Standardize all variables using scale()
z_advertise = data.frame(scale(advertise))
z_advertise_lm = lm(Sales ~ TV + Radio + Newspaper, z_advertise)
data.frame(abs_std_coef = abs(z_advertise_lm$coefficients[-1]))

# Rank based on absolute t-statistics
caret::varImp(z_advertise_lm)
# variable importance from randomForest 
z_advertise_rf = randomForest::randomForest(Sales ~ TV + Radio + Newspaper, z_advertise)
randomForest::importance(z_advertise_rf)

Map(function(x) LOOCV_lm_rmse(x, advertise), 
    c(Sales ~ TV + Radio, Sales ~ TV + Radio + Newspaper)) 

LOOCV_lm_rmse(Sales ~ TV * Radio, advertise) 

summary(lm(Sales ~ TV * Radio, advertise))

# Variable importance
# Rank importance based on absolute t-statistics
library("randomForest"); library("caret"); library("ISLR"); data("Auto")
# Only keep variables we need
Auto_xy = Auto[, ! colnames(Auto) %in% c("name", "year")]
Auto_scale_xy = data.frame(sapply(Auto_xy[, 2:6], scale))
Auto_scale_xy$origin = factor(Auto_xy$origin, levels = 1:3, 
                              labels = c("American", "European", "Japanese"))
Auto_scale_xy$mpg = Auto_xy$mpg

lm_Auto = lm(mpg ~ ., data = Auto_scale_xy)
summary(lm_Auto)
# Importance
sort(abs(lm_Auto$coefficients)[-1], decreasing = T)
cbind(caret::varImp(lm(mpg ~ . , data = Auto_xy)), importance(randomForest(mpg ~ ., data = Auto_xy)))

Hmisc::rcorr(as.matrix(Auto_xy[, ! colnames(Auto_xy) %in% c("name", "year")]),type = "spearman")

Map(function(x) LOOCV_lm_rmse(x, Auto_xy), 
    c(mpg ~ . -origin, mpg ~ . + displacement:cylinders, mpg ~ .  - acceleration)) 

Map(function(x) summary(lm(x, Auto_xy)), 
    c(mpg ~ ., mpg ~ . + displacement:cylinders, mpg ~ . + displacement:cylinders - acceleration))

Map(function(x) summary(lm(x, Auto_xy)), 
    c(mpg ~ (cylinders +displacement +horsepower + weight  + acceleration +origin)^2
      , mpg ~ . + displacement:cylinders, mpg ~ . + displacement:cylinders - acceleration))


library("caret"); library("doMC");
registerDoMC(cores = detectCores() - 1 ) # DO NOT use all CPU cores

rfe_auto = caret::rfe(x = Auto_xy[,-1], y = Auto_xy$mpg, metric = "RMSE",
                      rfeControl = rfeControl(method = "LOOCV", functions =  lmFuncs,  allowParallel = T))
rfe_auto$optVariables

# Or we can use stats::step() 
nullModel = lm( mpg ~ 1 , data=Auto_xy) # Base/null model. We may add "forced-in" variables here
fullModel = lm( mpg ~ (.)^2 , data=Auto_xy) # Full model. Here we consider up to pairwise interactions.
# Forward Selection
stepAutoXY = step(nullModel, scope = list(lower = nullModel, upper = fullModel ), direction = "forward", test = "F" )
stepAutoXY

# To predict "murder rate" of 50 states in the U.S. (1977)
data(state); murder = data.frame(state.x77)

# Lists of variable importance could give you some senses of the data
varImp(lm(Murder ~ ., data = murder))

murder_rf = randomForest(Murder ~ ., data = murder, ntree = 100); importance(murder_rf)

# Let's try RFE
selVarMurder = caret::rfe(x = murder[,-5], y = murder$Murder, metric = "RMSE",
                          rfeControl = rfeControl(method = "LOOCV", functions = lmFuncs,  allowParallel = T))
selVarMurder
# The top 4 variables (out of 4):
# Life.Exp, Illiteracy, HS.Grad, Frost

# Hands-on hybrid feature selection
# Step 1 - bivariate analysis
Map(function(x) {fit = lm(x, data = murder); anova(fit)$"Pr(>F)"[1]}, 
    Map(as.formula, paste("Murder ~", colnames(murder)[-5])))
# Step 2 - fit model with those variables with p-values < 0.1
summary(lm(Murder ~ Population + Illiteracy + Life.Exp + HS.Grad + Frost, murder))

# Step 3 - fit model again but remove those variables with p-values < 0.1
summary(lm(Murder ~ Population + Illiteracy + Life.Exp + HS.Grad , murder))

Hmisc::rcorr(as.matrix(murder,type = "spearman"))

Map(function(x) LOOCV_lm_rmse(x, murder), 
    list(Murder ~ Population + Illiteracy + Life.Exp + HS.Grad + Frost, 
         Murder ~ Population + Illiteracy + Life.Exp + HS.Grad,
         Murder ~ Population + Illiteracy + Life.Exp,
         Murder ~ Population + Illiteracy + Life.Exp * HS.Grad))

# Polynomial Regression
Auto_xy_hp2 = transform(Auto_xy, horsepower2 = horsepower ^ 2 )
summary(lm(mpg ~ horsepower + horsepower2, Auto_xy_hp2)) 
# Or, just use I()
summary(lm(mpg ~ horsepower + I(horsepower ^ 2), Auto_xy)) 

summary(lm(mpg ~ poly(horsepower, degree = 5), data = Auto_xy))

# Seems like fitting to the degree of 2 is reasonable.
Map(function(d) LOOCV_lm_rmse( mpg ~ poly(horsepower, d) , Auto_xy) , 1:5)

library("ISLR"); data("Default")
glm_default = glm(default ~ balance, data = Default, family = "binomial")
summary(glm_default)
# What is the probability the probability of default=Yes, say the balance is $2000?
exp(-10.65 + 0.0055 * 2000) / (1 + exp(-10.65 + 0.0055 * 2000))
# Or use predict() to make our life easier
predict(glm_default, data.frame(balance = 2000), type="response")

glm_default = glm(default ~ student, data = Default, family = "binomial")
summary(glm_default)
predict(glm_default, data.frame(student = c("Yes", "No") ), type="response")


p = prop.table(xtabs( ~ student + default, Default), margin = 1); p

odds = p[,2] / p[,1]
odds[2] / odds[1] # odd ratio

glm_default = glm(default ~ balance + student, data = Default, family = "binomial")
summary(glm_default)
glm_default$coefficients
exp(-0.714877620)

epiDisplay::logistic.display(glm_default)


glm_default = glm(x - default ~ student + balance, data = Default, family = "binomial")

## logistic regression for classification
set.seed(1); numOfRows = nrow(Default) # Number of observations
# Split dataset into training and testing
train_idx = sample(1:numOfRows,size = numOfRows * 0.7) # 70% (7,000) as training
test_idx = setdiff(1:numOfRows, train_idx) # 3,000 as testing
default_LR = glm(default ~ student + balance, data = Default[train_idx,], family = "binomial")
test_pred_prob = predict(default_LR, newdata = Default[test_idx,],type = "response")
hist(test_pred_prob, xlab = "Predicted Probability")

# Distribution of our discrete outcome
prop.table(table(Default[train_idx,]$default))

# 3 confusion matrices for 3 cutoffs
pred_class = Map(function(x) factor(ifelse( test_pred_prob > x, "Yes", "No")), c(0.8, 0.5, 0.1) )
CMs = Map(function(x) table(relevel(x,"Yes"), relevel(Default[test_idx, "default"], "Yes") ) , pred_class)
CMs

library("pROC")
default_glm_roc = roc(Default[test_idx, "default"], test_pred_prob)

# ROC for 3 cutoffs
plot.roc(default_glm_roc, print.thres = c(0.8, 0.5, 0.1), print.auc = T, xlim = c(1, 0))

# Optimal threshold/cutoff based on Youden's Index 
# Notice that, again, in real-world applications, you should use an independent # sample (instead the testing data in our sample code) to identify this cutoff.
plot.roc(default_glm_roc, print.thres = "best", print.thres.best.method = "youden", print.auc = T)

# k-fold CV confusion matrix for Logistic Regression model
# f: formula, d: data, k: number of folds, cutoff: cutoff point 0-1
k_fold_CV_logit = function(f, d, k, cutoff){
  numOfRec = nrow(d) # number of observations
  reponse_var = all.vars(f)[1] # name of the reponse variable
  # k indices used to split data into k parts
  sample_idx_k = rep(sample(1:k),round(numOfRec / k) + 1)[1:numOfRec]
  # k models for k subsets of data
  k_fits = Map( function(x) glm(f, d[sample_idx_k != x, ], family = "binomial"), 1:k)
  # Predicted & actual classes for each hold-out subset 
  predActualClass = Map(function(x){
    predictedProb = predict(k_fits[[x]], d[sample_idx_k == x,], type = "response")
    predictedClass = ifelse(predictedProb > cutoff, 1, 0)
    return(data.frame("predictedClass" =  predictedClass, "actualClass" = d[sample_idx_k == x, reponse_var] ) )
  }, 1:k)
  # A data frame with all predicted & actual classes
  output_DF = Reduce(function(x, y) rbind(x, y), predActualClass)
  output_DF$predictedClass = factor(output_DF$predictedClass, levels=c(0,1),labels = c("No", "Yes"))
  return( table(output_DF$predictedClass, output_DF$actualClass))
}

# Different cutoffs
Map(function(cutoff) k_fold_CV_logit(default ~ student + balance, Default[train_idx,], 10, cutoff),
    list(0.8, 0.5, 0.1))

# Different k
Map(function(k_fold) k_fold_CV_logit(default ~ student + balance, Default[train_idx,], k_fold, 0.5),
    list(5, 10, 20))

# Accuracy of prediction for models with different feature set
Map(function(x) sum(diag(k_fold_CV_logit(x, Default[train_idx,], 10, 0.5))) / nrow(Default[train_idx,]),
    list(default ~ student,  default ~ balance, default ~ student + balance, default ~ student + balance + income ))

# Try it
titanic = read.table("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic.txt",
                     header = T, sep = ",", stringsAsFactors = T, strip.white = T)

# Using package "rpart" (Recursive Partitioning and Regression Trees)
library(ISLR); library(rpart); library(rpart.plot); data(Hitters)

# Plot the regression tree with 3 nodes on the slide
library(partykit)
plot(as.party( 
  rpart(log(Salary) ~ Years + Hits, Hitters, 
        control = rpart.control(maxdepth = 2, minbucket = 50))))

# Building regression tree with depth = 2
hitSalary_RT = rpart(log(Salary) ~ Years + Hits, Hitters, 
                     control = rpart.control(maxdepth = 2))
hitSalary_RT

# Plot it!
library(partykit)
plot.party(as.party(hitSalary_RT))

# Regression tree for Hitters again but with 10-fold CV
set.seed(1)
hitSalary_RT = rpart(log(Salary) ~ Years + Hits, Hitters, 
                     control = rpart.control(xval = 10))
# Plot tree
rpart.plot(hitSalary_RT, digits = 3)

# Print normalized errors and cost-complexity parameters
printcp(hitSalary_RT)

# Plot cost-complexity parameters (CP) to help select the best tree 
plotcp(hitSalary_RT, upper = "splits")

# To get an CP table with "real" errors by multipling root node error
realCPTable = data.frame(printcp(hitSalary_RT))
Map(function(x) realCPTable[,x] <<- realCPTable[,x] * 0.78766, c(1,3,4,5 ))

# Seems that a tree with 2 or 3 splits (3 or 4 leaves) perform
# relatively ok. Let's say we'd like the tree with 3 splits. We can 
# prune the tree as:
# Always prune tree with cp threshold higher then cp of the tree we need 
hitSalary_RT_3split = prune(hitSalary_RT,cp = 0.019)    
printcp(hitSalary_RT_3split)
# Plot the tree
rpart.plot(hitSalary_RT_3split)

# k-fold CV + 1 SE
realCPTable$xerr_1std = realCPTable$xerror + realCPTable$xstd

# Let rpart() select its best tree
hitSalary_RT = rpart(log(Salary) ~ Years + Hits, Hitters, 
                     control = rpart.control(xval = 10))
rpart.plot(hitSalary_RT)

# Download heart dataset, skip the first column
heart = read.table("http://www-bcf.usc.edu/~gareth/ISL/Heart.csv", header = T, sep = ",")[,-1]
# Fully-grown tree with 13 leaves
set.seed(1)
heart_CT = rpart(AHD ~ ., data = heart, control = rpart.control( cp = 0, xval = 10))
# Show all possible cost-complexity prunings
plotcp(heart_CT, upper = "splits"); printcp(heart_CT)
# Let rpart select the best tree (a tree with 5 splits?) 
seet.seed(1)
heart_best_CT = rpart(AHD ~ ., data = heart, control = rpart.control(xval = 10))
printcp(heart_best_CT)
rpart.plot(heart_best_CT,extra = 1)

# Split dataset into training and testing
library(ISLR); library(randomForest); data(Hitters)
Hitters = na.omit(Hitters); numOfRows = nrow(Hitters) 
set.seed(1); train_idx = sample(1:numOfRows,size = numOfRows * 0.7) # 70%  as training
test_idx = setdiff(1:numOfRows, train_idx) # 30% as testing
# Build a random forest with 500 trees (by default)
set.seed(1)
hitSalary_RF = randomForest(log(Salary) ~ Years + Hits, Hitters[train_idx,])
# Plot the "Errors vs. # of trees". Or just enter "plot(hitSalary_RF)" 
qplot(1:500, hitSalary_RF$mse, geom = "line") + labs(x = "# of trees", y = "MSE")

# Get RMSEs for 5 different models
hitters_RMSE = Map(function(f){ 
  set.seed(1)
  fit = f(log(Salary) ~ Years + Hits, Hitters);
  pred_response = predict(fit, newdata = Hitters[test_idx,]);
  return(sqrt(mean( (pred_response - log(Hitters[test_idx,"Salary"]) ) ^2)) );
},c("RF_50" = function(f, d) randomForest(formula = f, data = d, ntree=50),
    "RF_200" = function(f, d) randomForest(formula = f, data = d, ntree=200),
    "RF_500" = function(f, d) randomForest(formula = f, data = d, ntree=500),
    "RT" = rpart, "LM" = lm))

# Heart dataset again!
heart = read.table("http://www-bcf.usc.edu/~gareth/ISL/Heart.csv", header = T, sep = ",")[,-1]
heart = na.omit(heart); numOfRows = nrow(heart)
set.seed(1); train_idx = sample(1:numOfRows,size = numOfRows * 0.7) 
test_idx = setdiff(1:numOfRows, train_idx) 
heart_RF = randomForest(AHD ~ ., heart[train_idx,])
heart_RF_err = reshape2::melt(heart_RF$err.rate)
colnames(heart_RF_err) = c("n_of_trees", "err.typ","err.rate")
# Plot OOB error and class errors (PPV and NPV)
qplot(n_of_trees, err.rate, color=err.typ, data = heart_RF_err, geom="line") + ylim(0.05, 0.3)
heart_RF

library(pROC)
# ROC and AUCs for testing data
heart_measures = Map(function(f){ 
  set.seed(1);
  fit = f(AHD ~ ., heart[train_idx,]);
  # Get predicted probability for ROCs
  if(class(fit)[1] == "glm"){
    pred_prob = predict(fit, newdata = heart[test_idx,], type = "response");
  }else{
    pred_prob = predict(fit, newdata = heart[test_idx,], type = "prob")[, "Yes"];
  }
  return(roc(heart[test_idx, "AHD"], pred_prob));
}, c("RF_500" = randomForest,"CT" = rpart, 
     "Logistic" = function(f, d) glm(formula = f, data = d, family = "binomial") ))

# By setting mtry = # of all variables, we can get "bagging trees" result
set.seed(1)
heart_BT = randomForest(AHD ~ ., heart[train_idx,], mtry = (ncol(heart) - 1))
heart_BT_pred_prob = predict(heart_BT, newdata = heart[test_idx,], type = "prob")[, "Yes"];
heart_BT_roc = roc(heart[test_idx, "AHD"], heart_BT_pred_prob)

# Plot ROCs
plot.roc(heart_BT_roc, col = "green")
plot.roc(heart_measures[[1]], add = T, col = "blue")
plot.roc(heart_measures[[2]], add = T, col = "red")
plot.roc(heart_measures[[3]], add = T, col = "yellow")
legend("bottomright", legend=c("BaggingTrees", "RandomForest", "CART","Logistic"),
       col=c("green", "blue", "red", "yellow"), lwd=2)

# Get all AUCs
heart_BT_roc$auc
Map(function(x) x$auc, heart_measures)

# Principal component analysis
data(USArrests)
X = scale(as.matrix(USArrests)) # Center the data
# Covariance matrix of standarized variables
cov_X = cov(X)

# Eigen decomposition of the covariance matrix
eigen_arrest = eigen(cov_X)
# 1st PC with loadings
eigen_arrest$vectors[,1]
# 1st PC score
as.matrix(X) %*% matrix(eigen_arrest$vectors[,1], ncol = 1) 
sum(eigen_arrest$vectors[,1] ^ 2) # sum of squared loadings is 1

# PCA using princomp()
pc_arrest = princomp(X, scores = T)
# loadings of each PC
pc_arrest$loadings
# scores of each PC
pc_arrest$scores
# percentages of explained variance 
pc_arrest$sdev ^2 / sum((pc_arrest$sdev)^2)
# A biplot should help you understand more about PCs
biplot(pc_arrest)
# Better biplot ggbiplot::ggbiplot() 
# library("devtools"); install_github("vqv/ggbiplot")
ggbiplot::ggbiplot(pc_arrest, labels = rownames(X), labels.size = 5)
ggbiplot::ggscreeplot(pc_arrest)

# A simple DTM
dtm = data.frame("programming"=c(2,0,3,0,3), "database"=c(3,3,4,0,0), 
                 "SQL" = c(1,2,5,0,0), "finance" = c(0,2,0,5,0), "rate" = c(0,2,0,2,0),
                 "market" = c(0,1,0,3,0), "data" = c(1,2,0,2,2))
rownames(dtm) = paste("doc", 1:5, sep = "_")

# LSA using SVD
dtm_svd = svd(dtm) 
# Create a data frame used to plot the terms/documents in 2-D semantic map
dtm_loc = data.frame("doc_term" = c(rownames(dtm),colnames(dtm)),
                     "dt_type" = c(rep("document",5), rep("term",7)),
                     "x1" = c(dtm_svd$u[,1], dtm_svd$v[,1]),
                     "x2" = c(dtm_svd$u[,2], dtm_svd$v[,2]) ) 
ggplot(dtm_loc, aes(x = x1, y = x2, color=factor(dt_type))) + geom_point(size = 3) +
  geom_text(aes(label=doc_term),hjust=0, vjust=0) + xlim(-0.75,0) + xlab("Topic 1") + ylab("Topic 2")


# PCA using SVD
data(USArrests); X = scale(as.matrix(USArrests)) # Center the data
svd_X = svd(X)
# We can recover the original scaled matrix
X_prime = svd_X$u %*% diag(c(svd_X$d[1:4]) ) %*% t(svd_X$v)
# Low-rank approximation (say rank = 2) or noise reduction of the original matrix
X_prime = svd_X$u %*% diag(c(svd_X$d[1:2], 0, 0) ) %*% t(svd_X$v)
# Each columns in V contains loadings of each PC (information about columns)
svd_X$v

# Each column in U contains information about observations 
# Let's plot them onto a 2-D "concept map".
# We may also evaluate the similarity of states in terms of PC1 and PC2
U = data.frame(svd_X$u)
rownames(U) = rownames(USArrests)
colnames(U) = paste("PC_", 1:4, sep = "")
ggplot(U, aes(x = PC_1, y = PC_2, )) + geom_point(size = 3) + xlim(-0.3, 0.4) +
  geom_text(aes(label=rownames(U)),hjust=0, vjust=0, size = 5) + xlab("PC 1") + ylab("PC 2")


