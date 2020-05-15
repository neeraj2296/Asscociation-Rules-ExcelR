#Including the necassary Libraries
library(arules)
library(arulesViz)

#Loading the data set
book <- read.csv(file.choose())
#Factorising the data for better classification
book$ChildBks<-as.factor(book$ChildBks)
book$YouthBks<-as.factor(book$YouthBks)
book$CookBks<-as.factor(book$CookBks)
book$DoItYBks<-as.factor(book$DoItYBks)
book$RefBks<-as.factor(book$RefBks)
book$ArtBks<-as.factor(book$ArtBks)
book$GeogBks<-as.factor(book$GeogBks)
book$ItalCook<-as.factor(book$ItalCook)
book$ItalAtlas<-as.factor(book$ItalAtlas)
book$ItalArt<-as.factor(book$ItalArt)
book$Florence<-as.factor(book$Florence)
#rules$conf
str(book)

#Applying the Apriori Alogithm and figuring out rules
rules = apriori(book)
arules::inspect(rule)
rules.sorted<-sort(rules, by = 'lift')
arules::inspect(rules)

#Visualising the rules
plot(rules)
summary(book)

# rules with rhs containing CookBks only
rules = apriori(book,parameter = list(minlen = 1,supp = 0.11,conf = 0.5),appearance = list(rhs = "CookBks=1"))
plot(rules, jitter = 0)

#Summarised view f rules with rhs having CookBks only
summary(rules)
arules::inspect(rules)
?apriori
inspect(rules)

# rules with rhs containing CookBks only & with all others taken as bought( i.e. 1)
rules = apriori(book,parameter = list(minlen = 1,supp = 0.1,conf = 0.3),appearance = list(rhs = c("CookBks=1"),lhs = c("RefBks=1","ArtBks=1","YouthBks=1","GeogBks=1","DoItYBks=1","ChildBks=1","ItalArt=1","ItalAtlas=1","ItalCook=1"), default ="none"))
plot(rules, jitter = 0)
#plot(rules,method='grouped')
#plot(rules,method = 'graph',control = list(type='items'))
summary(rules)
arules::inspect(rules)
?apriori          
summary(rules)
inspect(rules)

#Finding Redundant Rules.
subset.matrix<-is.subset(rules,rules, sparse = FALSE)
#subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=1.65
which(redundant)

#Removing Redundant Rules
rules.pruned<-rules[!redundant]
rules.pruned<-sort(rules.pruned, by='lift')
inspect(rules.pruned)
plot(rules.pruned)
plot(rules.pruned, method = 'grouped')
