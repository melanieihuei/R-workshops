
## Education Investigation
#####################################################################
##### Download the dataset here:
##### https://github.com/melanieihuei/R-workshops/blob/master/datasets/education2000.csv

### Getting started
#####################################################################
#### 1 setup your environment
setwd('<path-to-your-file>')
#### 2 load your dataset
# Specify the NA strings
education = read.csv('education2000.csv', header = T, na.strings = "")
# Preview First 7 rows of first 8 columns
head(education[,1:8], 7)


### Data Summary
#####################################################################
#### 1 Extract rows with *miss* is 0 (Drop missing values)
# A new dataframe only containing variable "miss" is 0
education_miss0 = education[education$miss==0, ]
# Preview First 7 rows of rist 8 columns
head(education_miss0[,1:8], 7)

#### 2 Read the distribution of *educ*
# Draw three figures in a window
layout(matrix(c(1, 2, 3), 1, 3, byrow = T))
# qqplot: check normality
qqnorm(education_miss0$educ, col = "SteelBlue")
qqline(education_miss0$educ, col = "red")
# boxplot:
boxplot(education_miss0$educ, col = "SteelBlue")
title('Boxplot of education years')
mtext('Years', side = 2, line = 2.1, cex = 0.7)
# histogram:
hist(education_miss0$educ, col = "SteelBlue", breaks = 10, freq = F, 
     main = "", xlab = "")
title('Histogram of education years')
mtext('Education Years', side = 1, line = 2.1, cex = 0.7)
box()

#### 3 Test the skewness and kurtosis of *educ*
library(moments)
# get skewness and test it
cat('Skeness: ', skewness(education_miss0$educ))
agostino.test(education_miss0$educ)
# get kurtosis and test it
cat('Kurtosis: ', kurtosis(education_miss0$educ))
anscombe.test(education_miss0$educ)

#### 4 Generate a new variable *educsq* instead of dropping missing values
# create a new column in original dataset
education$educsq = education$educ^2
# Read the distribution of new variable
# Draw two figures in a window
layout(matrix(c(1, 2), 1, 2, byrow = T))
# qqplot: check normality
qqnorm(education$educsq, col = "SteelBlue")
qqline(education$educsq, col = "red")
# histogram:
hist(education_miss0$educ, col = "SteelBlue", breaks = 10, freq = F, 
     main = "", xlab = "")
title('Histogram of (education years)^2')
mtext('(Education Years)^2', side = 1, line = 2.1, cex = 0.7)
box()


### Modeling
#####################################################################
### Model 1:
### Linear regression using *educ* as dependent variable
model1 = lm(educ ~ female + age + west + midwest + nrthest, data = education_miss0)
summary(model1)
anova(model1)

#### 1 Extract specific values from the results
str(summary(model1))
# residuals for observation 1 to 10
summary(model1)$residuals[1:10]
# you can also just do this:
residuals(model1)[1:10]
# R squared and adjusted R squared of this model
cat('R squared: ', summary(model1)$r.squared)
cat('adj. R squared: ', summary(model1)$adj.r.squared)
# coefficients of this model
summary(model1)$coefficients

#### 2 Extract the variables information if they are significant
# coefficients table
coef_table1 = summary(model1)$coefficients
# extract significant variables
coef_table1[coef_table1[,"Pr(>|t|)"]<0.05, ]
### -------------------------------------------------------------

### Model 2: 
### Linear regression adding variable *race*
model2 = lm(educ ~ female + age + west + midwest + nrthest + black + other,
            data = education_miss0)
summary(model2)
anova(model2)

#### Test Model Improvement
library(lmtest)
waldtest(model1, model2)
### -------------------------------------------------------------

### Model 3: 
### Linear regression adding variable *paeduc*
model3 = lm(educ ~ female + age + west + midwest + nrthest + black + other + paeduc,
            data = education_miss0)
summary(model3)
anova(model3)

#### Contingency Table
xtabs(~female+black, data = education_miss0)
### -------------------------------------------------------------

### Model 4: 
### Linear regression adding interaction variable
# Adding asterisk(*) between two variables gives you the individual and interaction variables
model4 = lm(educ ~ female*black + age + west + midwest + nrthest + other + paeduc,
            data = education_miss0)
summary(model4)
anova(model4)
### -------------------------------------------------------------

### Model 5: 
### Linear regression using *educsq* as dependent variable
model5 = lm(educsq ~ paeduc + age + polviews, data = education)
summary(model5)
anova(model5)


### Diagnostics
#####################################################################
#### 1 Check assumptions of linear models via graphs
# Draw four figures in a window
layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = T))
# Call all the figures once
plot(model5) # gives you which=1,2,3,5
# Make layout return to default
dev.off()
# Call only one figure
plot(model5, which=1)

#### 2 Check assumptions of linear models via tests
library(lmtest)
# Ramsey's RESET test for omitting variables (same as ovtest in STATA)
resettest(model5)
# Breusch-Pagan test for heteroskedasticity (same as hettest in STATA)
bptest(model5)

#### 3 Get those specific values
# Draw two figures in a window
layout(matrix(c(1, 2), 1, 2, byrow = T))
# Plot by built-in
plot(model5, which=5)
# Plot by your own
rsd = rstandard(model5)
lev = hatvalues(model5)
plot(lev, rsd, cex.main = 0.9,
     main = "Residuals-squared vs Leverage",
     xlab = "Leverage", ylab = "Standardized residuals")
abline(h=0, col="red")

#### 4 Compare residuals and specific variables
# Draw three figures in a window
layout(matrix(c(1, 2, 3), 1, 3, byrow = T))
# Calculate residuals
res = residuals(model5)
plot(education$paeduc[!is.na(education$paeduc)], res[!is.na(education$paeduc)],
     xlab = 'Parents Education', ylab = "Residuals", cex.lab = 0.95,
     main = 'Residuals Plot', cex.main = 0.95)
abline(h=0, col = "red")
plot(education$polviews[!is.na(education$polviews)], res[!is.na(education$polviews)],
     xlab = 'Polviews', ylab = "Residuals", cex.lab = 0.95,
     main = 'Residuals Plot', cex.main = 0.95)
abline(h=0, col = "red")
plot(education$age[!is.na(education$age)], res[!is.na(education$age)],
     xlab = 'Age', ylab = "Residuals", cex.lab = 0.95,
     main = 'Residuals Plot', cex.main = 0.95)
abline(h=0, col = "red")

#### 5 Check the side effect of variables
library(car)
avPlots(model5, id.n = 3)

#### 6 Check outliers
# generate cook's distance values
cooks = cooks.distance(model5)
# cook's distance plot
plot(model5, which = 4)

#### 7 Measure the influencial points
# Generate DfBETA values for each observation
model5_dfbeta = as.data.frame(dfbeta(model5))
summary(model5_dfbeta)
# Generate DFBeta, DFFIT, cooks.distance, hatbalues
influence.measures(model5)$infmat[1:10,]

