source("01_import_data.R")
source("02_data_curation.R")

#Binary-Choice Logit-Model
BCLogit = glm(Select ~ PBC + Cert + PLE + PMV + RA + GamEx + ProdInv + LBSpend + Gend + Age, 
            family = binomial(link = logit), data = data_snake)
summary(BCLogit)

#oddsratio
exp(BCLogit$coefficients)

BCLogit.mod = glm(Select ~ PBC*Cert + PLE*Cert + PMV + RA + GamEx + ProdInv + LBSpend + Gend + Age, 
            family = binomial(link = logit), data = data_snake)
summary(BCLogit.mod)
#oddsratio
exp(BCLogit.mod$coefficients)

#model diagnostics
library(rcompanion)
nagelkerke(BCLogit)
logLik(BCLogit)
nagelkerke(BCLogit.mod)
logLik(BCLogit.mod)
anova(BCLogit, BCLogit.mod,test = "Chisq")
#omnibus chisq
BCLogit.omnibus = glm(Select ~ 1, 
                  family = binomial(link = logit), data = data_snake)
anova(BCLogit.omnibus, BCLogit,test = "Chisq")


library(aod)
L <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), nrow = 1, byrow = TRUE)
wald.test(b = coef(BCLogit.mod), Sigma = vcov(BCLogit.mod), L = L)
rm(L)

#construct diagnostics
library(psych)
#alphas
#PBC
alpha(data.frame(data_snake$PBC1, data_snake$PBC2), check.keys=TRUE)
#PMV
alpha(data.frame(data_snake$PMV1, data_snake$PMV2, data_snake$PMV3), check.keys=TRUE)
#RA
alpha(data.frame(data_snake$RA1, data_snake$RA2, data_snake$RA3), check.keys=TRUE)

#AVE
library(semTools)
HS.model <- 'pmv  =~ PMV1 + PMV2 + PMV3
              ra =~ RA1 + RA2 + RA3
              pbc =~ PBC1 + PBC2'
fit <- cfa(HS.model, data = data_snake)
reliability(fit)
#Squareroot
sqrt(reliability(fit, return.total = TRUE))
#clean
rm(HS.model,fit)

#CR composite reliabilty
library(lavaan)
# Define the model
model <- 'pmv  =~ PMV1 + PMV2 + PMV3'
model <- 'ra =~ RA1 + RA2 + RA3'
model <- 'pbc =~ PBC1 + PBC2'

#Berechnung jedes Konstrukt
# Fit the model
fit <- cfa(model, data = data_snake)
sl <- standardizedSolution(fit)
sl <- sl$est.std[sl$op == "=~"]
sl  # These are the standardized factor loadings for each item
# Compute residual variance of each item
re <- 1 - sl^2
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))

#clean
rm(fit,model,re,sl)

#corr table 
require('fBasics')
COR = data.frame(data_snake$Cert, data_snake$PLE, data_snake$PBC, data_snake$PMV, data_snake$RA, data_snake$ProdInv, 
                 data_snake$GamEx, data_snake$LBSpend, data_snake$Age, data_snake$Gend)
names(COR) = c("PWR","Prev. Loss Exp.","Perceived Behavioral Control","Perceived Monetary Value","Risk Aversion",
               "Product Involvement","Gaming Experience","Loot Box Spending","Age","Gender (male)")
colMeans(COR)
colStdevs(COR)
corr.test(COR)

#CMV
#harman single factor
factanal(factors = 1, covmat = cor(COR), 
         rotation = "varimax")