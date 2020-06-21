##############################################################################
## Final Project
## Authors: "Nimisha Goel & Saurabh Gupta"
## Description: R Script for the final report
##  Exploratory analysis of birthwt dataset
##  Contains Bivariate, Multivariate, Logistic Regression Models and Machine
##  Learning: Out of Sample Validation
##              
##############################################################################
## Clearing the environment
rm(list = ls())

## Loading the Libraries : MASS, data.table, ggplot2, stats and kknn
library(MASS)
library(data.table)
library(ggplot2)
library(stats)
library(kknn)

## Loading the data and converting it to data.table
data("birthwt")
birthwt <- data.table(birthwt)

## Converting categorical variables to numeric
birthwt[, race:= as.factor(race)]
birthwt[, smoke:=as.factor(smoke)]
birthwt[, ht:=as.factor(ht)]
birthwt[, ui:=as.factor(ui)]

### LOGISTIC REGRESSION / MACHINE LEARNING ###

## Likeliness of Healthy Baby weight(>2.5 KG) v/s Mother's Weight
logistic_reg <- glm(low ~ lwt, data=birthwt,family = binomial)
summary(logistic_reg)

## The intercept value in the above logistic regression is 0.998331 and the log odds of the
## birthweight of the baby to be less than 2.5 kg is  2.714. 
## The Coefficient value is -0.01406, which means the logs odds ratio for a pound increase in mother's 
## weight will increase the odds ratio of baby's weight to be above 2.5 kg by 0.986 times. 

birthwt[, predicted_wt:= predict(logistic_reg, type="response")]
ggplot(birthwt, aes(x=lwt)) +
  geom_point(aes(y=low)) +
  geom_line(aes(y=predicted_wt), size=1) +
  labs(title = "Logistic Regression curve for finding probability baby to be below 2.5 kg",
       x= "Mother's Weight",
       y= "Probability of baby being below 2.5 KG")

## Likeliness of Healthy Baby weight(>2.5 KG) v/s Mother's Weight and smoking status
logistic_reg_smoke <- glm(low ~ lwt + smoke, data=birthwt,family = binomial)
summary(logistic_reg_smoke)
## The intercept value in the above logistic regression is 0.62200 and the log odds of the
## birthweight of the baby to be less than 2.5 kg is  1.863. 
## The Coefficient value for lwt is -0.01332, which means the logs odds ratio for a pound increase in mother's 
## weight will increase the odds ratio of baby's weight to be above 2.5 kg by 0.986 times.
## The Coefficient for smoke1 is 0.67667, so for a smoker mother the baby being born below 2.5 kg is 1.967 times
## more likely than for a non-smoker mother.


### Out-of-Sample Validation for one variable and two variables ###

## 1. Splitting the data into training and testing set (85%-15%)
random_order <- sample(nrow(birthwt))
birthwt <- birthwt[random_order]
testing_set <- birthwt[1:30]
training_set <- birthwt[31:189]

## 2. Running both logistic regressions on the training set;
logistic_reg_bothpredvar <- glm(low ~ lwt + smoke, data=training_set,family = binomial)
logistic_reg_onepredvar <- glm(low ~ lwt,data=training_set,family = binomial)

## 3. Predicting values from each regression for the testing set and converting those values to binary variables;
testing_set[, predict_bothpredvar:= predict(logistic_reg_bothpredvar, newdata = testing_set, type = "response")]
testing_set[, predict_onepredvar:= predict(logistic_reg_onepredvar, newdata = testing_set, type = "response")]

## 4. Determining which regression predicts "better"-- because your outcome is binary, the error metric you should use is
##    "what percent of the time did my regression predict correctly?" 
## Take the Root Mean Square values for both regressions
testing_set[, squared_bothpredvar:= (predict_bothpredvar-low)^2]
testing_set[, squared_onepredvar:= (predict_onepredvar-low)^2]

rmse_bothpredvar <- sqrt(sum(testing_set$squared_bothpredvar)/nrow(testing_set))
print(rmse_bothpredvar)
rmse_onepredvar <- sqrt(sum(testing_set$squared_onepredvar)/nrow(testing_set))
print(rmse_onepredvar)
## For two predictor variables (lwt and smoke), the value is predicted correctly 42.99596% of times
## For one predictor variable, the value is predicted correctly 44.65154% of times.
## Thus, the logistic regression with ONE Predictor Variable predicts BETTER.

##For this, we make ggplot of both regression, with both variables(lwt+smoke in blue) and one varibale (lwt in red)
ggplot(testing_set, aes(x=lwt)) + 
  geom_point(aes(y=low)) +
  geom_line(aes(y=predict_bothpredvar), color="blue") + 
  geom_line(aes(y=predict_onepredvar), color="red") + 
  labs(title = "Out of Sample Validation for two variables(blue) and one variable(red)",
       x="Mother's Weight in pounds",
       y="Probability of baby being above 2.5 KG")
