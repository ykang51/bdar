####################
#### Unit 3 R codes
####################

library("vcd") # Load package "vcd" for data "Arthritis"
data(Arthritis)
mytable = xtabs(~ Treatment + Improved, data=Arthritis)
addmargins(mytable) # marginal sums
addmargins(prop.table(mytable)) # marginal percentages

# Bar chart
ggplot(Arthritis, aes(x = Arthritis$Treatment, fill=Arthritis$Improved)) + geom_bar()

library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved, chisq = T, fisher = T,
           format ="SAS")

# Get the five numbers for "mpg" of mtcars given different "am"
lapply(split(mtcars$mpg, mtcars$am), FUN = fivenum)

# Boxplot
qplot(factor(am,levels = c(0,1), labels = c('Automatic','Manual')),
      mpg,data = mtcars, geom = "boxplot",xlab = "Transmission")

# Histogram with density curve for MPG
hist(mtcars$mpg, freq=F, breaks=10, col="red", xlab="MPG", main="Histogram with density curve")
lines(density(mtcars$mpg), col="blue", lwd=2)

# By different "am". Use ggplot instead.
ggplot(mtcars, aes(x = mtcars$mpg, fill=factor(mtcars$am))) + geom_density(alpha=0.3) + xlim(0,50)

# Fair coin (p=0.5)
dbinom(x = 5,size = 10,prob = 0.5)

# Histogram for binomial random numbers given p=0.5, size = 10
qplot(rbinom(10e3, 10, 0.5), geom = "histogram") +
  scale_x_continuous(breaks = 0:10) +
  theme(axis.text= element_text(size=15))

# Is this a fair dice?
pointFreq1 = c( 31, 22, 17, 13, 9, 8)
chisq.test(x = pointFreq1,p = rep(1/6, 6))
# How about this one?
pointFreq2 = c( 17, 16, 15, 12, 19, 21)
chisq.test(x = pointFreq2,p = rep(1/6, 6))

set.seed(0)
# normal vs uniform
twoDists = data.frame("data"= c(rnorm(100,0,1), runif(100, -3, 3)),
                      "dist" = factor( c(rep("normal",100), rep("uniform",100))) )
ggplot(twoDists, aes(x = twoDists$data, fill=factor(twoDists$dist) )) + geom_density(alpha=0.3) + xlim(-5,5)
twoDists_2c = split(twoDists$data, twoDists$dist)
ks.test(twoDists_2c$normal,twoDists_2c$uniform)

# H0: sample is taken from a normal distribution
shapiro.test(mtcars$mpg)
ks.test(scale(mtcars$mpg), "pnorm")

nortest::lillie.test(faithful$waiting)

# Try It! (DownloadFestival)
dlfest = read.table("http://www.discoveringstatistics.com/docs/DownloadFestival(No%20Outlier).dat",
                    header = T, fill = T )

# Or we can just load the data in class package "bigdataR"
library(bigdataR); dlfest = bigDataR::DownloadFestival

# Create q-q plot for "day1"
qqnorm(dlfest$day1); qqline(dlfest$day1)

library(reshape2)
qplot(dlfest$day1, geom = "density")

dlfest_long = melt(data = dlfest, id.vars = c("ticknumb", "gender"))

# Density plot for 3 days
ggplot(dlfest_long, aes(x = dlfest_long$value, fill=factor(dlfest_long$variable))) +
  geom_density(alpha=0.3) 

# Normality Tests
Map(dlfest[,paste("day", 1:3, sep="")],
    f = function(x) list("shapiro" = shapiro.test(x),
                         "lillie" = nortest::lillie.test(x),
                         "Anderson" = nortest::ad.test(x)))
# Only day1 may be considered "normally-distributed"

# Paired/repeated measures tests

# Parametric, assuming the distributions of day1/day2 are normal
t.test(dlfest$day1, dlfest$day2, paired = T)
# Equivalanet to below 1-sample t test
diff = dlfest$day1 - dlfest$day2
t.test(diff)

# Non-Parametric, Wilcoxon rank sum test (more
# popularly known as the Mann–Whitney U test)
wilcox.test(dlfest$day1, dlfest$day2, paired = T)

#
library("MASS"); UScrime = as.data.frame(UScrime)
UScrime$So = as.factor(UScrime$So)
# Consider built-in dataset MASS::USCrime"
ggplot(UScrime, aes(x = UScrime$Prob, fill=factor(UScrime$So))) + geom_density(alpha=0.3)

# We here compare "Southern and non-Southern states" (So) on the "probability of
# imprisonment" (Prob) without the assumption of equal variances of "So"
t.test(formula = Prob ~ So, data = UScrime)

usc_lm = lm(Prob ~ So, data = UScrime)
summary(usc_lm)

car::Anova(usc_lm, type=3)

# Normality tests
library("nortest")
Map(function(f) f(UScrime$Prob), c(shapiro.test, lillie.test, ad.test) )
# Wilcoxon rank-sum test
wilcox.test(Prob ~ So, data = UScrime)

library(multcomp) # Get "cholesterol" dataset
cholesterol = as.data.frame(cholesterol)

?cholesterol
qplot(trt,response,data = cholesterol,geom = "boxplot",xlab = "Treatment")

# H0: all group means are equal
cho_aov = aov(response ~ trt, data = cholesterol); summary(cho_aov) # ANOVA

# Equivalent to lm()
cho_lm = lm(response ~ trt, data = cholesterol);

summary(cho_lm)

car::Anova(cho_lm,type = 3)

# A trained model can also be used to predict
predict(cho_lm ,newdata = data.frame(trt=c("1time","drugD")))

summary.lm(cho_aov)
cholesterol_c = cholesterol;

#"4 times" as reference level
contrasts(cholesterol_c$trt) = contr.treatment(5, base=3)
cho_aov = aov(response ~ trt, data = cholesterol_c);
summary.lm(cho_aov)

TukeyHSD(cho_aov)
# H0: medians of all groups are equal
kruskal.test(response ~ trt, data = cholesterol)

# The correlations among mpg, hp, and wt
Hmisc::rcorr(as.matrix(mtcars[,c("mpg","hp","wt")]),type = "pearson")

# A strong positive correlation between x and y
x = c(3, 13,14,55,56); y = c(49,50, 66, 99,1000)
cor.test(x,y,method = "pearson"); cor.test(x,y,method = "spearman")

# May not be able to compute p-value if sample size is big and there # are some ties!
cor.test(x, y, method = "kendall")

# Correlation before controlling for wt
mpg_hp = cor(mtcars$mpg, mtcars$hp,method = "pearson")
mpg_hp ^ 2 # R squared (coefficient of determination)

# Correlation after controlling for wt
mpg_hp.ctl.wt = ppcor::pcor.test(mtcars$mpg, mtcars$hp, mtcars[,c("wt")], method = "pearson"); mpg_hp.ctl.wt
mpg_hp.ctl.wt$estimate ^ 2

## Feature Scaling & Transformation
# Using Default data
library(ggplot2);library(ISLR); library(class); library(caret); data(Default)
set.seed(1); # Sample with Default = Yes and Default = No, 50 records for each 
default_100_rn = sapply(split(Default, Default$default), 
                        function(df) sample(rownames(df), 50))
rm(Default)
# 100 records
default_100 = Default[rownames(Default) %in% default_100_rn, c(1,3,4)]
set.seed(0); train_idx = sample(1:100, size = 90)
# Training & testing sets, unscaled
default_train = default_100[train_idx,]
default_test = default_100[-train_idx, - 1]
# Data treatment/processing plan. Here we consider "standardization"
def_plan = caret::preProcess(default_train, method=c("center", "scale"))
def_plan$method # double check with "methods"
# Training & testing sets, scaled
default_train_std = predict(def_plan, default_train)
default_test_std = predict(def_plan, default_test)

# Plot with original scales, along with testing records
qplot(income, balance, color = default, data = default_train, geom = "point") +   
  geom_point(aes(income, balance), default_test, colour = "black", size = 4)

# KNN with k = 3
default_test$predClass = knn(default_train[, 2:3], default_test, k = 3, cl = default_train$default)  

# Check the classification result
qplot(income, balance, color = default, data = default_train, geom = "point") +   
  geom_point(aes(income, balance, color = predClass), default_test, size = 4)

# How about using "Scaled" data?
qplot(income, balance, color = default, data = default_train_std, geom = "point") +   
  geom_point(aes(income, balance), default_test_std, colour = "black", size = 4)

# Again, KNN with k = 3
default_test_std$predClass = knn(default_train_std[, 2:3], default_test_std, k = 3, cl = default_train_std$default)  

# Again, check the classification result
qplot(income, balance, color = default, data = default_train_std, geom = "point") +   
  geom_point(aes(income, balance, color = predClass), default_test_std, size = 4)

## Using data pre-processing functions in caret
library(ggplot2); library(caret)
rm(list = ls()) # Housekeeping
diamonds = as.data.frame(ggplot2::diamonds); numOfObs = nrow(diamonds)
# Split data into training and testing set
set.seed(1) # Setting random seed
# Indices for training/testing set
train_idx = sample(1:numOfObs, 0.7 * numOfObs)
test_idx = setdiff(1:numOfObs, train_idx)

diamonds = as.data.frame(diamonds)
# Create feature processing/transformation/treatment plans
# Here we consider leaving "price" out as the outcome
dmdFeatureProc = preProcess(diamonds[train_idx, -7 ], 
                            method = c("center", "scale"), verbose = T)

dmdFeatureProc$method # Double check with the plan
train_d = predict(dmdFeatureProc, diamonds[train_idx,])
test_d = predict(dmdFeatureProc, diamonds[test_idx,])

# Dummy coding example 
mt = mtcars;mt$cyl = factor(mt$cyl)
mt$am = factor(mt$am, levels = c(0, 1), labels = c("auto", "manual"))
# Checking the model matrix
model.matrix(mpg ~ am + cyl, data = mt)
# Coding matrix
contrasts(mt$cyl)
# Change the reference group/level, note that the column names are the level values
contrasts(mt$cyl) = contr.treatment(3, base = 2); contrasts(mt$cyl)
model.matrix(mpg ~ am + cyl, data = mt)
# lm() will automatically do the coding for us 
lm_mt = lm(mpg ~ am + cyl, data = mt, x = T)
summary(lm_mt); lm_mt$x

# Binomail probability distribution given p(head) = 0.3
round(dbinom(0:10,size = 10,prob = 0.3), digits = 5)

# Generate outcomes that follow the binomial
# distribution with parameter θ, p(head) = 0.2
set.seed(1); x = rbinom(100,10,0.2); x

# Let's reuse previous "x" as that outcome for simulation
set.seed(1); x = rbinom(100,10,0.2)
# Function used to calculate the likelihood
LL_binom = function(p){
  ll = dbinom(x,10,p); return(-sum(log(ll)));
}
# Maximum likelihood estimate of the parameter, 0 <= P(H) <= 1
# The goal is to find a P(H) that maximize likelihood.
optim(par = 0.5, fn = LL_binom, method = "Brent", lower=0, upper=1)


# Take a look the coefficients of below model
lm(data = mtcars, formula= mpg ~ wt)
# We can do it by using MLE instead
LL_LM = function(beta0, beta1) {
  resid = mtcars$mpg - (mtcars$wt * beta1) - beta0; # calculate residuals
  ll = dnorm(resid, 0, 1); # likelihood for the residuals
  return(-sum(log(ll))); #sum of the log likelihoods
}
stats4::mle(LL_LM, start=list(beta0 = 0, beta1 = 0), method = "Nelder-Mead")

# correlated data analysis
read_age = data.frame(
  subjID= rep(1:6, each=2),
  read_ability = c(90, 95,  80, 85, 78, 88, 50, 58, 50, 61, 10, 14),
  age = c(10, 15, 11, 16, 18, 23, 18, 21, 23, 26, 22, 26 ),
  gender = factor(rep(c('F','M','F','F','F','M'),each = 2), levels=c('F','M')) )

# ** This is WRONG! **
summary(lm(read_ability ~ age, read_age)) # Simple linear regression

read_age$timepoint = rep(c("timepoint1","timepoint2"),6)
read_age_w = dcast(data = read_age, formula = subjID ~ timepoint, value.var = "read_ability")
read_age_w$gap = read_age_w$timepoint1 - read_age_w$timepoint2
wilcox.test(read_age_w$gap, mu = 0)

wilcox.test(read_age$read_ability ~ read_age$timepoint, paired=T)
t.test(read_age$read_ability ~ read_age$timepoint, paired=T)


library("lme4") # linear mixed-effect models
summary(lmer(read_ability ~ age + gender + ( 1| subjID), read_age))

