

#loading the data
cereals <- read.csv("Cereals.csv",sep=",",header = T)
head(cereals)
str(cereals)

#b. computing descriptive statistics for quantitative
cereals.numeric<- cereals[,-c(1:3,13)]  #getting all numeric variables
head(cereals.numeric)

descriptive.cereals <- data.frame(mean=round(sapply(cereals.numeric, mean, na.rm=TRUE),2),
                                  median=round(sapply(cereals.numeric, median, na.rm=TRUE),2),
                                  min=round(sapply(cereals.numeric, min, na.rm=TRUE),2),
                                  max=round(sapply(cereals.numeric, max, na.rm=TRUE),2),
                                  std.dev=round(sapply(cereals.numeric, sd, na.rm=TRUE),2)
                                  )
print(descriptive.cereals)
write.csv(descriptive.cereals,"descriptive_cereals.csv", row.names = TRUE)

#c. Histogram of numeric variables
par(mfrow=c(3,4)) #setting the parameters for 4 by 4 plots
hist(cereals.numeric$calories,main="calories",xlab="values")
hist(cereals.numeric$protein,main="proteins",xlab="values")
hist(cereals.numeric$fat,main="fat",xlab = "values")
hist(cereals.numeric$sodium,main = "sodium",xlab = "values")
hist(cereals.numeric$fiber,main="fiber",xlab="values")
hist(cereals.numeric$carbo,main="carbo",xlab="values")
hist(cereals.numeric$sugars,main="sugars",xlab = "values")
hist(cereals.numeric$potass,main = "potass",xlab = "values")
hist(cereals.numeric$vitamins,main="vitamins",xlab="values")
hist(cereals.numeric$weight,main="weight",xlab = "values")
hist(cereals.numeric$cups,main = "cups",xlab = "values")
hist(cereals.numeric$rating,main = "rating",xlab = "values")

#d. side-by-side plot of calories vs type (hot and code)
?boxplot #for help on boxplots

par(mfrow=c(1,1)) #returning to default plot 
boxplot(cereals$calories ~ cereals$type, main="Calories in Hot and Cold Cereals", ylab = "Calories",
        xlab="Type of cereals (H=Hot, C=Cold)",)

#e. side-by-side plot of consumers rating by shelf height
boxplot(cereals$rating ~ cereals$shelf, main="Consumer rating by Shelf Height",ylab="Consumer rating",
        xlab="Shelf Height")

#f. correlation tab for the numeric or quantitative variable
?cor #for help on correlation esp to avoid missing rows.
correlation_cereals <- round(cor(cereals.numeric, use = "complete.obs"),2)
write.csv(correlation_cereals, "corr_cereals_numeric.csv")

plot(cereals.numeric) #plotting the correlation

