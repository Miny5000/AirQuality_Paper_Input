#Readme - RandomForest model calibration CS AMS3 and AMS4, NO2
#Authors: MR, JF 
#Step1: Setwd, import combined dataframe 
#Step2: Split into train and test data sets
#Step3: a. Build random forest model AMS3
#       b. Build random forest model AMS4         
#Step4: a. AMS3 Rsquared, Feature importance, Mean absolute error, Mean squared error
#       b. AMS4 Rsquared, Feature importance, Mean absolute error, Mean squared error
#Step5: Print evaluation metrics 
#End Readme 

#load libraries
library(rio)
library(randomForest)
library(caret)
library(ggplot2)
library(e1071)

#Step 1 - Set wd, import df 
#MR version wd 
setwd("C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/Output")
#JF version wd
#setwd("~/R/CS-calibration-files")

#do a tukeys test for outlier removal (take 95%interval)
joined_df<- import("dcmrNO_dcmrPM_cs3_cs4_combined.csv")

#look at normal & natural log transformation distribution
hist(log(joined_df$temperature_opc_cs3))

#linear regression 
linear_model <- lm(NO2_ppb ~ gas_op2_w_cs3+temperature_opc_cs3+humidity_opc_cs3, data=joined_df)
summary(linear_model)

#Step 2 - split into train and test dataset (rationale: rf handles outliers well, + done in previous literature)
set.seed(123) #set.seed(42)
train_indices <- sample(1:nrow(joined_df), 0.7*nrow(joined_df)) #try with 0.8
train_data <- joined_df[train_indices,]
test_data <- joined_df[-train_indices,]

#Step 3- randomForest models; predicted = official NO2 data, predictors are NO2 gassensor CS, temperature and humidity (finetunes number of trees)
##CS3
rf_CS3 <- randomForest(NO2_ppb ~ gas_op2_w_cs3+temperature_opc_cs3+humidity_opc_cs3, data = train_data, ntree=400, importance =TRUE)
##CS4
rf_CS4 <- randomForest(NO2_ppb ~ gas_op2_w_cs4+temperature_opc_cs4+humidity_opc_cs4, data = train_data, ntree=400, importance =TRUE)

#tuning the three (otherwise, try per ntrees)
bestmtry <- tuneRF(train_data, train_data$NO2_ppb, stepFactor = 1.2, improve = 0.01, trace=T, plot =T)
bestmtry

#Step 4 - Evaluate RandomForest models
##CS3 
###Rsquared 
pred_CS3 <- predict(rf_CS3, test_data)
summary(pred_CS3)
rf_CS3_2 <- 1 - sum((test_data$NO2_ppb - pred_CS3)^2) / sum((test_data$NO2_ppb - mean(test_data$NO2_ppb))^2)

##JLF : want to take a peak at the outputs:
ggplot(test_data, aes(x=NO2_ppb, y=pred_CS3)) +
  geom_point() +
  xlab("DCMR NO2 (ppb)") +
  ylab("CS3 RF NO2 prediction (ppb)")

###feature importance 
feature_importance_CS3 <- rf_CS3$importance

###mean absolute and squared error
actual_CS3 <- test_data$NO2_ppb
absolute_errors <- abs(actual_CS3 - pred_CS3)
mae_CS3 <- mean(absolute_errors)
mse_CS3 <- mean(actual_CS3 - pred_CS3)^2

###Compare the predicted values with the actual values
comparison_CS3 <- data.frame(Predicted = pred_CS3, Actual = actual_CS3)
#View(comparison_CS3) ##JLF editted

##CS4
###Rsquared
pred_CS4 <- predict(rf_CS4, test_data)
summary(pred_CS4)
rf_CS4_2 <- 1 - sum((test_data$NO2_ppb - pred_CS4)^2) / sum((test_data$NO2_ppb - mean(test_data$NO2_ppb))^2)

###feature importance 
feature_importance_CS4 <- rf_CS4$importance

###mean absolute and squared error
actual_CS4 <- test_data$NO2_ppb
absolute_errors <- abs(actual_CS4 - pred_CS4)
mae_CS4 <- mean(absolute_errors)
mse_CS4 <- mean(actual_CS4 - pred_CS4)^2

###Compare the predicted values with the actual values
comparison_CS4 <- data.frame(Predicted = pred_CS4, Actual = actual_CS4)
#View(comparison_CS4)

##JLF : want to take a peak at the outputs:
ggplot(comparison_CS4, aes(x=Actual, y=Predicted)) +
  geom_point() +
  xlab("DCMR NO2 (ppb)") +
  ylab("CS4 RF NO2 prediction (ppb)")

#Step5- print evaluation metrics
print(paste("R-squared CS3:", rf_CS3_2))
print(mae_CS3)
print(mse_CS3)
feature_importance_CS3

print(paste("R-squared CS4:", rf_CS4_2))
print(mae_CS4)
print(mse_CS4)
feature_importance_CS4


