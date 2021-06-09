# ----------------------------- Remission prediction - Logistic Regression -----------------------------

# Packages
library(car)
library(caret)
library(MASS)
library(corrplot)


# Set Invironment
setwd('/Users/tsuerh/Documents/Depaul/DSC423\ Data\ Analysis\ and\ Regression/Assignments/4_66')
remission <- read.csv('remission.csv')

# Check dataset
summary(remission)
plot(remission)
cor <- cor(remission)
corrplot(cor, method = 'circle')

# Transfer y to factor
remission$remiss <- as.factor(remission$remiss)

# Build model
logit <- glm(remiss ~., data = remission, family = 'binomial')
summary(logit)
vif(logit)

logit2 <- glm(remiss ~ cell + smear + infil + li + temp, data = remission, family = 'binomial')
summary(logit2)
vif(logit2)

logit3 <- glm(remiss ~ cell + smear + li + temp, data = remission, family = 'binomial')
summary(logit3)
vif(logit3)

# Create additional variables
remission$cellSQ <- remission$cell * remission$cell
remission$smearSQ <- remission$smear * remission$smear
remission$infilSQ <- remission$infil * remission$infil
remission$liSQ <- remission$li * remission$li
remission$blastSQ <- remission$blast * remission$blast
remission$tempSQ <- remission$temp * remission$temp
remission$cel_sm <- remission$cell * remission$smear
remission$cel_in <- remission$cell * remission$infil
remission$cel_li <- remission$cell * remission$li
remission$cel_bla <- remission$cell * remission$blast
remission$cel_temp <- remission$cell * remission$temp
remission$sm_in <- remission$smear * remission$infil
remission$sm_li <- remission$smear * remission$li
remission$sm_bla <- remission$smear * remission$blast
remission$sm_temp <- remission$smear * remission$temp
remission$in_li <- remission$infil * remission$li
remission$in_bla <- remission$infil * remission$blast
remission$in_temp <- remission$infil * remission$temp
remission$li_bla <- remission$li * remission$blast
remission$li_temp <- remission$li * remission$temp
remission$bla_temp <- remission$blast * remission$temp
remission$cel_sm_in <- remission$cell * remission$smear *remission$infil
remission$cel_sm_li <- remission$cell * remission$smear *remission$li
remission$cel_sm_bla <- remission$cell * remission$smear *remission$blast
remission$cel_sm_temp <- remission$cell * remission$smear *remission$temp
remission$in_bla_temp <- remission$infil * remission$blast * remission$temp
remission$li_bla_temp <- remission$li * remission$blast * remission$temp






# Modeling
org <- glm(remiss ~ cell + smear + li + temp, data = remission, family = 'binomial')

l1 <- glm(remiss ~ cell + smear + temp + cellSQ + smearSQ +
            infilSQ + blastSQ + tempSQ, data = remission, family = 'binomial')
summary(l1)

l2 <- glm(remiss ~ cell + smear + temp + cellSQ + smearSQ + 
            blastSQ + tempSQ, data = remission, family = 'binomial')
summary(l2)
confint(l2)

l3 <- glm(remiss ~ cell + temp + smear+ smearSQ + tempSQ + cellSQ + cel_sm_in + cel_sm_li + cel_sm_bla +
            cel_sm_temp  , data = remission,
          family = 'binomial')
summary(l3)
confint(l3)
exp(coef(l3))-1


l3 <- glm(remiss ~ cell + temp + smear+ smearSQ + tempSQ + cellSQ + cel_sm_in + cel_sm_li + cel_sm_bla +
            cel_sm_temp   , data = remission,
          family = 'binomial')
summary(l3)
confint(l3)
exp(coef(l3))-1