#Readme - RandomForest model calibration CS AMS3 and AMS4, PM 2.5
#Authors: MR, JF
#Step1: Setwd, import combined dataframe 
#Step2: Split into train and test data sets
#Step3: a. Build random forest model AMS3
#       b. Build random forest model AMS4         
#Step4: a. AMS3 Rsquared, Feature importance, Mean absolute error, Mean squared error
#       b. AMS4 Rsquared, Feature importance, Mean absolute error, Mean squared error
#Step 5: Print evaluation metrics 
#End Readme 

#load libraries
library(rio)
library(randomForest)
library(caret)
library(ggplot2)

#Step 1 - Set wd, import df 
#MR version wd 
setwd("C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/Output")
#JF version wd
#setwd("~/R/CS-calibration-files")

joined_df<- import("dcmrNO_dcmrPM_cs3_cs4_combined.CSV")

#Step 2 - split into train and test dataset
set.seed(123)
train_indices <- sample(1:nrow(joined_df), 0.7*nrow(joined_df))
train_data <- joined_df[train_indices,]
test_data <- joined_df[-train_indices,]

#Step 3- randomForest PM; predicted = official PM2.5 data, predictors are PM2.5, temperature and humidity 
##CS3
#rf_CS3 <- randomForest(palas.pm25 ~ PM25_cs3+temperature_opc_cs3+humidity_opc_cs3, data = train_data, ntree=400, importance =TRUE)
rf_CS3 <- randomForest(palas.pm25 ~ PM25_cs3, data = train_data, ntree=400, importance =TRUE)

##CS4
#rf_CS4 <- randomForest(palas.pm25 ~ PM25_cs4+temperature_opc_cs4+humidity_opc_cs4, data = train_data, ntree=400, importance =TRUE)
rf_CS4 <- randomForest(palas.pm25 ~ PM25_cs4, data = train_data, ntree=400, importance =TRUE)

#Step 4 - Evaluate RandomForest models
##CS3 
###Rsquared 
pred_CS3 <- predict(rf_CS3, test_data)
summary(pred_CS3)
rf_CS3_2 <- 1 - sum((test_data$palas.pm25 - pred_CS3)^2) / sum((test_data$palas.pm25 - mean(test_data$palas.pm25))^2)

##JLF : want to take a peak at the outputs:
ggplot(test_data, aes(x=palas.pm25, y=pred_CS3)) +
  geom_point() +
  xlab("DCMR PM25") +
  ylab("CS3 RF PM25")

###feature importance 
feature_importance_CS3 <- rf_CS3$importance

###mean absolute and squared error
actual_CS3 <- test_data$PM25_cs3
absolute_errors <- abs(actual_CS3 - pred_CS3)
mae_CS3 <- mean(absolute_errors)
mse_CS3 <- mean(actual_CS3 - pred_CS3)^2

###Compare the predicted values with the actual values
comparison_CS3 <- data.frame(Predicted = pred_CS3, Actual = actual_CS3)
View(comparison_CS3) ##JLF editted

##CS4
###Rsquared
pred_CS4 <- predict(rf_CS4, test_data)
summary(pred_CS4)
rf_CS4_2 <- 1 - sum((test_data$palas.pm25 - pred_CS4)^2) / sum((test_data$palas.pm25 - mean(test_data$palas.pm25))^2)

###feature importance 
feature_importance_CS4 <- rf_CS4$importance

###mean absolute and squared error
actual_CS4 <- test_data$palas.pm25
absolute_errors <- abs(actual_CS4 - pred_CS4)
mae_CS4 <- mean(absolute_errors)
mse_CS4 <- mean(actual_CS4 - pred_CS4)^2

###Compare the predicted values with the actual values
comparison_CS4 <- data.frame(Predicted = pred_CS4, Actual = actual_CS4)
View(comparison_CS4)

##JLF : want to take a peak at the outputs:
ggplot(comparison_CS4, aes(x=Actual, y=Predicted)) +
  geom_point() +
  xlab("DCMR PM25") +
  ylab("CS4 RF PM25 prediction")

#step 5 - Print results 
print(paste("R-squared CS3:", rf_CS3_2))
print(mae_CS3)
print(mse_CS3)
feature_importance_CS3

print(paste("R-squared CS4:", rf_CS4_2))
print(mae_CS4)
print(mse_CS4)
feature_importance_CS4


