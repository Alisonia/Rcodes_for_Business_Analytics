
#loading the data
toysR<-read.csv("ToysRUsRevenues.csv", sep = ",")
head(toysR)
colnames(toysR)
str(toysR)
sum(is.na(toysR))  #checking missing values, 0

#Plotting the trend line
library(ggplot2)  #for nice plots

ggplot(toysR, aes(x = Index, y = Revenue)) + 
   geom_line(color = "blue", size = 1) + 
   geom_point(color = "red", size = 2) +
   labs(title = "Quarterly Revenues of Toys 'R' Us (1992-1995)", 
        x = "Index", 
        y = "Revenue (in million $)") +
   theme_bw() +
   scale_x_continuous(breaks = toysR$Index, labels = toysR$QuarterYear)

#Fitting the regression with linear trend and additive seasonality

# Convert 'Quarter' to a factor (for additive seasonality)
toysR$Quarter <- as.factor(toysR$Quarter)

# Split the data into training (excluding the last two quarters) and validation sets
train_data <- toysR[1:14, ]
test_data <- toysR[15:16, ]  

# Fit a linear regression model with additive seasonality and linear trend
linear_seasonal_model <- lm(Revenue ~ Index + Quarter, data = train_data)
summary(linear_seasonal_model)


# Predict revenue for the last two quarters (test data)
predictions <- predict(linear_seasonal_model, newdata = test_data)

# Show predictions and compare with actual values
data.frame(QuarterYear = test_data$QuarterYear, 
           Actual_Revenue = test_data$Revenue, 
           Predicted_Revenue = predictions)




