
#MACHINE LEARNING FOR BUSINESS ANALYTICS


#Part 1
#Part A. Creating a vector for stock prices over the last 10 trading days (Earliest to most recent)

# NIFTY 50 (^NSEI)
StockNSEI <- c(25235.90, 25278.70, 25279.85, 25198.70, 25145.10,
                24852.15, 24936.40, 25041.10, 24918.45, 25388.90)
print(StockNSEI)

# NYSE AMEX COMPOSITE INDEX (^XAX)
StockXAX <- c(5237.45, 5198.53, 5050.92, 4977.82, 4932.21, 
             4827.96, 4830.80, 4780.22, 4786.02, 4827.06)
print(StockXAX)

# Creating a vector for the period (10 days)
Days <- 1:10

# Display the Days vector
print(Days)


# Combining the Days, StockNSEI, and StockXAX vectors into a matrix
StocksMatrix <- cbind(Days, StockNSEI, StockXAX)

# Displaying the matrix
print(StocksMatrix)


# Converting the matrix into a dataframe
StocksDataFrame <- as.data.frame(StocksMatrix)

# Displaying the data frame
print(StocksDataFrame)

# Exporting the dataframe to a CSV file named "StocksLast10Days.csv"
write.csv(StocksDataFrame, "StocksLast10Days.csv", row.names = FALSE)

#PART II
# Importing the AdSales dataset into R
library(readxl)
Sales <- read_excel("AdSales.xlsx",sheet=1)
head(Sales)


# Loading the xtable library
library(xtable)

# Exporting the Sales dataframe to an HTML file
SalesTable <- xtable(Sales)
print.xtable(SalesTable, type = "html", file = "AdSales.html")

