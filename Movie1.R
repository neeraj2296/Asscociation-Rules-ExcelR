#Including the necassary Libraries
library(arules)
library(arulesViz)

#Loading the data set
mov <- read.csv(file.choose(), header = T, colClasses = 'factor')
movi <- mov[,6:15]
#Factorising the data for better classification
movi$Sixth.Sense<-as.factor(movi$Sixth.Sense)
movi$Gladiator<-as.factor(movi$Gladiator)
movi$LOTR1<-as.factor(movi$LOTR1)
movi$Harry.Potter1<-as.factor(movi$Harry.Potter1)
movi$Patriot<-as.factor(movi$Patriot)
movi$LOTR2<-as.factor(movi$LOTR2)
movi$Harry.Potter2<-as.factor(movi$Harry.Potter2)
movi$Braveheart<-as.factor(movi$Braveheart)
movi$Green.Mile<-as.factor(movi$Green.Mile)

str(movi)
#Applying the Apriori Alogithm and figuring out rules
rules<-apriori(movi)
rules.sorted<-sort(rules, by = 'lift')

#Visualising the rules
plot(rules, jitter = 0)
plot(rules, method = 'grouped')
summary(rules)
summary(movi)

# rules with rhs containing Patriot only & with all others taken as bought( i.e. 1)
rules = apriori(movi,parameter = list(minlen = 1,supp = 0.1,conf = 0.8),appearance = list(rhs = c("Patriot=1"), lhs = c("Sixth.Sense=1",'Gladiator=1','LOTR1=1','Harry.Potter1=1','LOTR2=1','Harry.Potter2=1','LOTR=1','Braveheart=1','Green.Mile=1')))
plot(rules)                                                                                          
inspect(rules) 

# rules with rhs containing Sixth Sense only & with all others taken as bought( i.e. 1)
rules = apriori(movi,parameter = list(minlen = 1,supp = 0.1,conf = 0.8),appearance = list(rhs = c("Sixth.Sense=1"), lhs = c("Patriot=1",'Gladiator=1','LOTR1=1','Harry.Potter1=1','LOTR2=1','Harry.Potter2=1','LOTR=1','Braveheart=1','Green.Mile=1')))
plot(rules)                                                                                          
inspect(rules)

# rules with rhs containing Patriot and Sixth Sense both & with all others taken as bought( i.e. 1)
rules = apriori(movi,parameter = list(minlen = 1,supp = 0.1,conf = 0.8),appearance = list(rhs = c("Patriot=1","Sixth.Sense=1"), lhs = c('Gladiator=1','LOTR1=1','Harry.Potter1=1','LOTR2=1','Harry.Potter2=1','LOTR=1','Braveheart=1','Green.Mile=1')))
plot(rules)                                                                                          
inspect(rules)

#Finding Redundant Rules.
subset.matrix<-is.subset(rules,rules, sparse = FALSE)
#subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=1.66
which(redundant)

#Removing Redundant Rules
rules.pruned<-rules[!redundant]
rules.pruned<-sort(rules.pruned, by='lift')
inspect(rules.pruned)
plot(rules.pruned)
plot(rules.pruned, method = 'grouped')
plot(rules,method = 'graph')
