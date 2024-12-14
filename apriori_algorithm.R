#1. Laod package
library(readxl)

install.packages("arules") #install arules package, must have quote
library(arules)

install.packages("arulesViz") #install arulesViz package, must have quote
library(arulesViz) 

#2. Data preprocessing and exploring
retail <- read_excel('100GroceryProductPurchase.xlsx')
head(retail)
colnames(retail) #View column names


#3. For Apriori algorithm, data need to be converted into transaction format

#remove all missing values
retail <- retail[complete.cases(retail), ] 

#converting all columns to factor variable
retail[] <- lapply(retail, factor)
str(retail)

#Now, creating the transaction
transactions <- as(retail, "transactions")
summary(transactions)
head(transactions)

#4. Run the Apriori Algorithm
rules <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.8))

# Inspect the top 10 rules
inspect(rules[1:10])

#sort rules by confidence and inspect again
rules_confidence <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules_confidence)

#5. Plot top 10 rules by confidence
topRules_confidence <- rules_confidence[1:10]
plot(topRules_confidence, jitter=5)
plot(topRules_confidence, method="graph")
plot(topRules_confidence, method = "grouped")
