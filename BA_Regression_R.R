
#loading the data
toyotaCorolla<-read.csv("ToyotaCorolla.csv", sep = ",")
head(toyotaCorolla)
str(toyotaCorolla)
sum(is.na(toyotaCorolla))  #checking missing values, 0

View(toyotaCorolla) #showing the data in rstudio

#selecting the variables of interest for modeling
library(tidyverse) #loading neccesary libraries

toyota_new_df <- toyotaCorolla %>% select(c(Price, Age_08_04, KM, Fuel_Type, HP, Automatic,
                                           Doors, Quarterly_Tax, Mfr_Guarantee, Guarantee_Period, Airco, 
                                           Automatic_airco, CD_Player, Powered_Windows, Sport_Model, Tow_Bar))
head(toyota_new_df) #viewing the first 5 rows

#converting Fuel type to dummy variables
library("fastDummies") #loading the library

toyota_new_df <- dummy_cols(toyota_new_df, select_columns = c("Fuel_Type"), 
                                  remove_first_dummy = TRUE, remove_selected_columns = TRUE)

#Splitting the data into 50% training set; 30% validation set; and 20% testing test
library(caret)  #for partitioning
set.seed(123)

trainIndex <- createDataPartition(toyota_new_df$Price, p = 0.5, list = FALSE)
trainData <- toyota_new_df[trainIndex, ]

tempData <- toyota_new_df[-trainIndex, ]
validIndex <- createDataPartition(tempData$Price, p = 0.6, list = FALSE)
validationData <- tempData[validIndex, ]
testData <- tempData[-validIndex, ]

# Display the dimensions of the datasets
cat("Training Set Size: ", dim(trainData), "\n")
cat("Validation Set Size: ", dim(validationData), "\n")
cat("Test Set Size: ", dim(testData), "\n")

#performing the regression model using the training data
reg_model_toyota<-lm(Price ~ ., data=trainData)
options(scipen = 999)
summary(reg_model_toyota)

#using stepwise regression to determine most important predictors
reg_model_toyota_step <-step(reg_model_toyota, direction = "backward",steps = 10)
summary(reg_model_toyota_step)

#Model performance:
install.packages("forecast")
library("forecast")

predict_step_toyota <- predict(reg_model_toyota_step, validationData)
accuracy(predict_step_toyota,validationData$Price)
residuals <- validationData$Price - predict_step_toyota

par(mfrow=c(1,2))
hist(residuals, breaks = 25, xlab = "Residuals")
boxplot(residuals, main="Boxplot of Residuals")
