
## Premarital Sex
#####################################################################
##### Download the dataset here:
##### https://github.com/melanieihuei/R-workshops/blob/master/datasets/premarsex.dta

### Getting started
#####################################################################
#### 1 setup your environment
setwd('<path-to-your-file>')
#### 2 load your dataset
library(haven)
premarsex = data.frame(read_dta('premarsx.dta'))
# Preview First 7 rows of first 8 columns
head(premarsex[,1:8], 7)


### Data Summary
#####################################################################
#### 1 Cross Table
library(gmodels)
with(premarsex, CrossTable(premarsx, missing.include=T))

#### 2 Generate binary variable *notwrng*
premarsex$notwrng <- ifelse(premarsex$premarsx==4, 1, 0)
with(premarsex, CrossTable(notwrng, missing.include=T))


### Modeling
#####################################################################
### Model 1:
### Controls on age, educational attainment, year questioned, and gender
LRmodel1 = glm(notwrng ~ age + educ + year2 + factor(female), 
               data = premarsex, family = 'binomial')
summary(LRmodel1)
### -------------------------------------------------------------

### Model 2: 
### Add in variables based on the region of the country a person 
### is from and their religious views:
LRmodel2 = glm(notwrng ~ age + educ + year2 + factor(female) + 
                 factor(protest) + factor(cathlic) + factor(jewish) + factor(othrel) +
                 factor(nonaff) + factor(nrthest) + factor(south) + factor(midwest), 
               data = premarsex, family = 'binomial')
summary(LRmodel2)

#### Likelihood Ratio Test
library(lmtest)
lrtest(LRmodel1, LRmodel2)
### -------------------------------------------------------------

### Model 3: 
### Change Model 1 as an Ordinal Logistic Regression
library(MASS)
# Hess=F gives you the observed information matrix 
OLRmodel1 = polr(factor(premarsx) ~ age + educ + year2 + factor(female), 
                 data = premarsex, Hess = F)
summary(OLRmodel1)
# add p-values to the result
OLRcoef1 = coef(summary(OLRmodel1))
OLRpvalue1 = pnorm(abs(OLRcoef1[,"t value"]), lower.tail = F)*2
OLRtable1 = cbind(OLRcoef1, "p value" = OLRpvalue1)
head(OLRtable1, 8)
### -------------------------------------------------------------

### Model 4: 
### Change Model 2 as an Ordinal Logistic Regression
OLRmodel2 = polr(factor(premarsx) ~ age + educ + year2 + factor(female) + 
                   factor(protest) + factor(cathlic) + factor(jewish) + factor(othrel) +
                   factor(nonaff) + factor(nrthest) + factor(south) + factor(midwest), 
                 data = premarsex, Hess = F)
summary(OLRmodel2)
### add p-values to the result
OLRcoef2 = coef(summary(OLRmodel2))
OLRpvalue2 = pnorm(abs(OLRcoef2[,"t value"]), lower.tail = F)*2
OLRtable2 = cbind(OLRcoef2, "p value" = OLRpvalue2)
head(OLRtable2, 8)

#### Likelihood Ratio Test
lrtest(OLRmodel1, OLRmodel2)
### -------------------------------------------------------------

### Model 5: Change Model 1 to Multinomial Logistic Regression
library(nnet)
multiLR.model1 = multinom(factor(premarsx) ~ age + educ + year2 + factor(female), 
                          data = premarsex)
summary(multiLR.model1)
### -------------------------------------------------------------

### Model 6: Change Model 2 to Multinomial Logistic Regression
multiLR.model2 = multinom(factor(premarsx) ~ age + educ + year2 + factor(female) + 
                            factor(protest) + factor(cathlic) + factor(jewish) + factor(othrel) +
                            factor(nonaff) + factor(nrthest) + factor(south) + factor(midwest), 
                          data = premarsex)
summary(multiLR.model2)

#### Likelihood Ratio Test
lrtest(multiLR.model1, multiLR.model2)

