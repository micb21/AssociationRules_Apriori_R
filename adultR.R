colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'educatoin','educatoin_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex','capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')
adultdata<-adult
summary(adultdata)
class(adultdata)

adultdata$educatoin <- NULL
adultdata$fnlwgt <- NULL
adultdata$educatoin_num <- NULL
adultdata$workclass <- NULL
adultdata$marital_status <- NULL
adultdata$relationship <- NULL
adultdata$capital_gain <- NULL
adultdata$capital_loss <- NULL
adultdata$hours_per_week <- NULL
adultdata$native_country <- NULL
adultdata$age<-discretize(adultdata$age, method = "frequency", breaks = 4, labels = c("Young", "Middle-aged", "Senior", "Old"))
adultdata$age
library("arules")
#create transactions
Adult <- as(adultdata, "transactions")
Adult
rules <- apriori(adultdata, parameter = list(support = 0.004, confidence = 0.1,minlen =2))


sort.rule<-sort(rules,by="lift")
sort.rule
top5rules <- head(rules, n = 5, by = "lift")
inspect(head(rules,5))
library("arulesViz")
plot(head(rules, n = 5, by = "lift"),method="graph")
summary(quality(rules))
adult_rules_rhs <- apriori(adultdata, parameter = list(support = 0.004,confidence = 0.1), appearance = list (rhs ="sex= Female"))

inspect(sort(adult_rules_rhs, by = 'lift')[1:5])

adult_rules_lhs <- apriori(adultdata, parameter = list(support = 0.004,confidence = 0.1), appearance = list (lhs ="sex= Female"))

inspect(sort(adult_rules_lhs, by = 'lift')[1:5])
subsets <- which(colSums(is.subset(rules, rules)) > 1)
maximal_rules <- rules[-subsets]
rules
maximal_rules
inspect(sort(maximal_rules, by = "lift")[1:5])
plot(maximal_rules)
#closed itemsets
closed_itemset <- apriori(adultdata,parameter = list(sup = 0.004, conf = 0.1,
                                                     target="closed"))
inspect(sort(closed_itemset, by = "support")[1:5])
#maximal itemsets
max_itemset <- apriori(adultdata,parameter = list(sup = 0.004, conf = 0.1,
                                                  target="maximally"))
inspect(sort(max_itemset, by = "support")[1:5])

plot(rules, method="paracoord", control=list(reorder=TRUE))