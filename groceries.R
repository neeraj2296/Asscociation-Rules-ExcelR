#Including the necassary Libraries
library(arules)
library(arulesViz)

#Loading the data set
setwd("E:\\Neeraj\\Exam and Careers\\DataScience\\Data Sets")
groceries <- read.transactions("groceries.csv", sep=",")
summary(groceries)
arules::inspect(groceries[1:5])#Inspecting the rules
#inspect(groc[1:5])
View(groceries[1:5])

#Vizualizing the data set
itemFrequency(groceries[, 1:3])
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)
image(groceries[1:5])
image(sample(groceries, 100))

#Applying the apriori Algorithm
apriori(groceries)
groc_rules <- apriori(groceries, parameter = list(support =0.006, confidence = 0.25, minlen = 1))

# summary of grocery association rules
summary(groc_rules)
arules::inspect(groc_rules)
arules::inspect(sort(groc_rules, by = "lift"))
groce<-arules::inspect(sort(groc_rules, by = "count"))
rules <- subset(groc_rules, items %in% "other vegetables")

#Inspecting the rules 
arules::inspect(rules)

#Finding Redundant Rules.
subset.matrix<-is.subset(rules,rules, sparse = FALSE)
#subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=2.25
which(redundant)

#Removing Redundant Rules
rules.pruned<-rules[!redundant]
rules.pruned<-sort(rules.pruned, by='lift')
arules::inspect(rules.pruned)
plot(rules.pruned)
plot(rules.pruned, method = 'grouped')

#Writing the finalised rules to groceryrules.csv
write(rules.pruned, file = "groceryrules.csv",sep = ",", quote = TRUE, row.names = FALSE)
groceryrules_df <- as(rules.pruned, "data.frame")
str(groceryrules_df)
View(groceryrules_df)
