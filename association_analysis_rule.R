#1. Laod package

install.packages("arules") #install arules package, must have quote
library(arules)

install.packages("arulesViz") #install arulesViz package, must have quote
library(arulesViz) 

#2. Data loading
cosmetic <- read.csv("Cosmetics.csv", sep=",",header = T)
head(cosmetic)
nrow(cosmetic)
colnames(cosmetic) #View column names


#3. For Apriori algorithm, data need to be converted into transaction format

#remove all missing values if any
cosmetic <- cosmetic[complete.cases(cosmetic), ] 

#convert to matrix
cosmetic.matrix <-as.matrix(cosmetic[,-1])
head(cosmetic.matrix)

#Now, creating the transaction using the cosmetic matrix.
transactions <- as(cosmetic.matrix, "transactions")
summary(transactions)
head(transactions)

#4. Run the Apriori Algorithm
rules <- apriori(transactions)

# Inspect the top 3 rules
inspect(rules[1:3])

# redundancy

# Sort first rules by lift 
rules_sorted_by_lift <- sort(rules, by = "lift", decreasing = TRUE)
inspect(rules_sorted_by_lift[1:3])


# Apply apriori with higher minimum support and confidence
rules_filtered <- apriori(transactions, parameter = list(supp = 0.05, conf = 0.7))
inspect(rules_filtered[1:3])

#sort rules by confidence and inspect again
rules_confidence <- sort(rules_filtered, by='lift', decreasing = TRUE)
summary(rules_confidence)

#5. Plot top 10 rules by confidence
topRules_confidence <- rules_confidence[1:10]
plot(topRules_confidence, jitter=5)
plot(topRules_confidence, method="graph")
plot(topRules_confidence, method = "grouped")
