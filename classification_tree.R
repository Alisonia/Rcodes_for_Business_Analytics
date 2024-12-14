
#loading the libraries
library(tidyverse)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

#loading the data
flight_data <- read.csv("FlightDelays.csv",sep = ",", header = T)
head(flight_data)             #view the first 5 rows
dim(flight_data)              #check the rows x columns (variables):: 2201 rows x 13 variables
str(flight_data)              #view the structure of the data
colnames(flight_data)         #show all the column names

#Data Preprocessing 

#transforming DAY_WEEK into categorical variable
flight_data$DAY_WEEK <- as.factor(flight_data$DAY_WEEK)

#Binning the schedule departure time into 8-bins
flight_data$CRS_DEP_TIME <- cut(flight_data$CRS_DEP_TIME, breaks = 8, labels = FALSE)

#Excluding departure time and days of the month
flight_data <- flight_data %>% select(-c(DEP_TIME, DAY_OF_MONTH))

#Showing the structure of the updated data
str(flight_data)
dim(flight_data)           #2201 rows x 11 variables

#Partitioning the data into training (80%) and validation set (20%)
set.seed(123)              # for reproducibility

train_index <- createDataPartition(flight_data$Flight.Status, p = 0.8, list = FALSE)
train_data <- flight_data[train_index,]
validation_data <- flight_data[-train_index]


#a. Fit a classification tree without DEP_TIME (actual departure time)

#Unprunned
tree_model_unpr <- rpart(Flight.Status ~ CRS_DEP_TIME+DEST+DISTANCE+CARRIER+
                       ORIGIN+Weather+DAY_WEEK, data = train_data, method = "class")
prp(tree_model_unpr, type = 1, extra = 1, under = TRUE, split.font=2)


#Pruned
tree_model_pr <- rpart(Flight.Status ~ CRS_DEP_TIME+DEST+DISTANCE+CARRIER+
                          ORIGIN+Weather+DAY_WEEK, data = train_data, method = "class", 
                    control = rpart.control(maxdepth = 8, cp = 0.001))

#plotting the tree
prp(tree_model_pr, type = 1, extra = 1, under = TRUE, split.font=2)

#c. Fit a classification tree without Weather

#Unprunned tree
tree_no_weather_unpr <- rpart(Flight.Status ~ CRS_DEP_TIME+DEST+DISTANCE+CARRIER+
                                 ORIGIN+DAY_WEEK+FL_DATE, data = train_data, method = "class")

prp(tree_no_weather_unpr, type = 1, extra = 1, under = TRUE, split.font=2)


#Pruned tree
tree_no_weather_pruned<- rpart(Flight.Status ~ CRS_DEP_TIME+DEST+DISTANCE+CARRIER+
                                  ORIGIN+DAY_WEEK, data = train_data, method = "class", 
                    control = rpart.control(maxdepth = 8, cp = 0.001))

prp(tree_no_weather_pruned, type = 1, extra = 1, under = TRUE, split.font=2)


#Top three predictors of the unprunned tree.

#feature Importance
importance <-varImp(tree_no_weather_unpr, scale=FALSE)
print(importance)

#making plots
importance_ <- c(70.83,51.70,28.00,27.20,25.84,24.98,21.92)
variables <- c("FL_DATE","CARRIER ","DISTANCE","CRS_DEP_TIME","ORIGIN","DEST","DAY_WEEK")
importance_df <- data.frame(importance_, variables)
barplot(importance_df$importance_ ~ importance_df$variables,horiz = TRUE)

barplot(
   importance_[order(importance_, decreasing = F)],  # Order the importance values in descending order
   names.arg = variables[order(importance_, decreasing = F)],  # Order the variable names accordingly
   horiz = T,  # Make the bar plot horizontal
   las = 1,  # Make labels horizontal
   col = "steelblue",  # Set color for the bars
   xlab = "Importance",  # Label for the x-axis
   main = "Variable Importance"  # Title for the plot
)





