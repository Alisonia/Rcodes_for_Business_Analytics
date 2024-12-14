
#laod neccesary libraries
library(tidyverse)

#importing the data
toyotaCorolla<-read.csv("ToyotaCorolla.csv", sep = ",")
head(toyotaCorolla)
str(toyotaCorolla)

#a. Exploring the data using data visualization techniques

# subseting the numeric variables for visualization
toyota_numeric <- toyotaCorolla %>% select(c(Price,Age_08_04,KM,HP,Weight,CC))

#plotting the correlation pairs
pairs(toyota_numeric, panel = panel.smooth, main = "Scatterplot Matrix",upper.panel = NULL)


#b.1 converting Fuel Type and Color to categorical variables
install.packages("fastDummies")  #installing the package

library("fastDummies") #loading the library

# Converting categorical variables FuelType and Color into dummy variables
toyotaCorolla_dummy <- dummy_cols(toyotaCorolla, select_columns = c("Fuel_Type", "Color"), 
                                  remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Display the first few rows of the updated dataset with dummy variables
head(toyotaCorolla_dummy)
fix(toyotaCorolla_dummy)  #assesing the new data using the seperate window
write.csv(toyotaCorolla_dummy,"toyotaCorrola_dummy.csv",row.names = FALSE) #saving the transformed data into .csv file



#b.2 Preparing the data into data mining techniques

#Load the caret package
library(caret)

# Setting the default seed
set.seed(123)

# Creating the partition indices for training (50%), validation (30%), and test (20%) sets
trainIndex <- createDataPartition(toyotaCorolla_dummy$Price, p = 0.5, list = FALSE)
trainData <- toyotaCorolla_dummy[trainIndex, ]

tempData <- toyotaCorolla_dummy[-trainIndex, ]
validIndex <- createDataPartition(tempData$Price, p = 0.6, list = FALSE)
validationData <- tempData[validIndex, ]
testData <- tempData[-validIndex, ]

# Display the dimensions of the datasets
cat("Training Set Size: ", dim(trainData), "\n")
cat("Validation Set Size: ", dim(validationData), "\n")
cat("Test Set Size: ", dim(testData), "\n")

#saving the train, validation, and test dataset
write.csv(trainData, "train_toyotaCorolla.csv",row.names = FALSE)
write.csv(testData, "test_toyotaCorrola.csv",row.names = FALSE)
write.csv(validationData,"validation_toyotaCoolla.csv",row.names = FALSE)
