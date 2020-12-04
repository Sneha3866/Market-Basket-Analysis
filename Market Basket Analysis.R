###my codes ####
library(arules)
library(arulesViz)
library(dplyr)

?Groceries
data("Groceries")
inspect(Groceries)

#we use item frequency plot to understand which all products have highest transactions
itemFrequencyPlot(Groceries, topN=10, type="absolute", col="wheat2", xlab = "Item Name",main = "Items with highest Frequency")

head(Groceries)

itemFrequency(Groceries [,1:50])
#itemfrequency allows us to see the SUPPORT of any item


# Support: The fraction of which our item set occurs in the dataset
# Confidence: Probability that a rule is correct for a new transaction with item on the left
# Lift: The ratio by which the confidence of a rule exceeds the expected outcome



?apriori
grrules<-apriori(Groceries)
grrules<-apriori(Groceries, parameter = list(supp = .01, conf = .8))
inspect(grrules[1:50])
#we see there are no rules hence we reduce the support 
?apriori

grrules<-apriori(Groceries, parameter = list(supp = .001, conf = .8))
summary(grrules)


#if the no of rules are too high (ex 65000)then we can reduce no of rules by secifying like:
#grrules<-apriori(Groceries, parameter = list(minlen = 2, maxlen = 3, supp = .007))supp is increased to 7 times. It will cut down rules drastically


options(digits = 2)
inspect(grrules[1:5])
# natural probability of buying whole milk is (conf/lift): 0.91/3.6, 0.81/3.2....

inspect(grrules[1:15])

0.90/11.2

#remove duplicate rules
grrules
redundant_rules <- is.redundant(grrules)
redundant_rules
summary(redundant_rules)
#remove these 18 rules that are duplicate
grrules <- grrules[!redundant_rules]
grrules


inspect(grrules[1:5])
grrules

#sort them according to support
grrules1<-sort(grrules, by = "support", decreasing = T)
inspect(grrules1[1:10])

0.89/4.6
#0.19

0.82/3.2
#0.26



grrules001<-sort(grrules, by = "confidence", decreasing = T)
inspect(grrules001[1:15])


#finding interesting rules
#first we check for whole milk as we have seen in the item frequency chart that whole milk occurs maximum number of time
grrules2 <- apriori(Groceries, parameter = list(supp = .001, conf = .8), appearance =
                          list(default = "rhs", lhs = "whole milk"))
grrules2 <- apriori(Groceries, parameter = list(supp = .0001, conf = .8), appearance =
                          list(default = "rhs", lhs = "whole milk"))

grrules2 <- apriori(Groceries, parameter = list(supp = .001, conf = .08, minlen = 2), appearance =
                          list(default = "rhs", lhs = "whole milk"))
?apriori
grrules0002 <- apriori(Groceries, parameter = list(supp = .001, conf = .08, minlen = 2, maxlen = 3), appearance =
                      list(default = "lhs", rhs = "whole milk"))
grrules2<-sort(grrules2, by = "confidence", decreasing = T)
grrules0002<-sort(grrules0002, by = "confidence", decreasing = T)
inspect(grrules2[1:5])
inspect(grrules0002[1:5])

###now sorting according to lift###
grrules01<-sort(grrules, by = "lift", decreasing = T)
inspect(grrules01[1:10])

grrules00002 <- apriori(Groceries, parameter = list(supp = .001, conf = .08, minlen = 2), appearance =
                      list(default = "rhs", lhs = "yogurt"))

inspect(grrules00002[1:5])


####now we start playing with the data##
###as we already know whole milk, other vegetables, rolls/buns, soda, yogurt are most frequent added items to cart###
###so we fix these items at LHS to check which all items are at RHS. So when a customer comes to a store to buy a particular product 
###they might need to buy all their needs from one particular store
###rather than visiting many
###so understanding the support of the products also bought with the lhs the store can increase the availability 
###this will help ib boosting the sales

###whole milk
grrules100 <- apriori(Groceries, parameter = list(supp = .001, conf = .08, minlen = 2), appearance =
                      list(default = "rhs", lhs = "whole milk"))
grrules100<-sort(grrules100, by = "support", decreasing = T)
inspect(grrules100[1:5])

###other vegetables 
grrules1001 <- apriori(Groceries, parameter = list(supp = .001, conf = .08, minlen = 2), appearance =
                        list(default = "rhs", lhs = "other vegetables"))
grrules1001<-sort(grrules1001, by = "support", decreasing = T)
inspect(grrules1001[1:5])

###rolls/buns
grrules10010 <- apriori(Groceries, parameter = list(supp = .001, conf = .08, minlen = 2), appearance =
                         list(default = "rhs", lhs = "rolls/buns"))
grrules10010<-sort(grrules10010, by = "support", decreasing = T)
inspect(grrules10010[1:5])

###soda
grrules100100 <- apriori(Groceries, parameter = list(supp = .001, conf = .08, minlen = 2), appearance =
                          list(default = "rhs", lhs = "soda"))
grrules100100<-sort(grrules100100, by = "support", decreasing = T)
inspect(grrules100100[1:5])

###yogurt
grrules1001000 <- apriori(Groceries, parameter = list(supp = .001, conf = .08, minlen = 2), appearance =
                           list(default = "rhs", lhs = "yogurt"))
grrules1001000<-sort(grrules1001000, by = "support", decreasing = T)
inspect(grrules1001000[1:5])




#we repeat the same steps again for other vegetables as its next in frequency

grrules3 <- apriori(Groceries, parameter = list(supp = .001, conf = .08), appearance =
                          list(default = "rhs", lhs = "other vegetables"))

inspect(grrules003[1:10])
grrules003<-sort(grrules3, by = "confidence", decreasing = T)

#you can make either side rhs or lhs as default

plot(grrules, method = "graph", interactive = T)
plot(grrules)
plot(grrules, method = "grouped")
plot(grrules, method = "graph")
plot(grrules, method = "graph", control = list(type="items"))