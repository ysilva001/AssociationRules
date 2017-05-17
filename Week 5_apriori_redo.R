###################
# Title : Book Data
# Data Source: https://github.com/WinVector/zmPDSwR/blob/master/Bookdata/bxBooks.RData 
# Author : Yesenia Silva
# MSDS664x70_Stat Infer Predictive Analytics
###################

##############Packages#############
library(arules) #Association Rules and Frequent Itemsets
library(ggplot2)#Visualizations
library(dplyr) #Data Manipulation
library(arulesViz) #Visualizing Association Rules and Frequent Itemsets
library(RColorBrewer) #Colors

##load data
load("C:/Users/ysilva/Downloads/bxBooks.RData")
summary(bxBooks)

##How many NA's
row.has.na <- apply(bxBooks, 1, function(x){any(is.na(x))})
sum(row.has.na)

books.row.has.na <- apply(bxBooks, 1, function(x){any(is.na(x))})
sum(books.row.has.na)

users.row.has.na <- apply(bxUsers, 1, function(x){any(is.na(x))})
sum(users.row.has.na)

##Reduce ratings dataset to 135,000 rows
ratings <- bxBooks[sample(nrow(bxBooks), 135000), ]
summary(ratings)
str(ratings)
summary(bxUsers)
str(bxUsers)

######Explore Data Prior to Tranforming to Transaction########

##TOP 20 book that were reviewed by user
r20 <- ratings %>%
  count(ISBN) %>%
  top_n(20) %>%
  arrange(n, ISBN) %>%
  mutate(ISBN = factor(ISBN, levels = unique(ISBN)))

ratings %>%
  filter(ISBN %in% r20$ISBN) %>%
  mutate(ISBN = factor(ISBN, levels = levels(r20$ISBN))) %>%
  ggplot(aes(x = ISBN)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Blues")  +
  coord_flip()

## Investigate ISBN 0971880107 as it has the most reviews

subset(ratings,ISBN== "0971880107")
out <- ratings[ratings$ISBN == "0971880107",]
str(out)
summary(out)
out %>% group_by(User.ID, ISBN, Book.Rating) %>% summarize(count=n())

#Investigate if USERID appears more than 1 for each ISBN
select(ratings, everything()) 

user.isbn.count <-
  ratings %>% 
  group_by(User.ID, ISBN) %>%
  summarize(total.count=n()) 

#counts of each rating
as.data.frame(table(ratings$Book.Rating))

######conver to transaction#####
##First convert rating to factor
ratings$User.ID <- as.factor(ratings$User.ID)
ratings$ISBN <- as.factor(ratings$ISBN)
ratings$Book.Rating <- factor(ratings$Book.Rating, labels=c("1","2","3","4","5","6","7","8","9","10"))

summary(ratings)
str(ratings)

Transactions <- as(ratings, "transactions")
Transactions

summary(Transactions)

# plot the frequency of items
par(mfrow = c(1, 2))

itemFrequencyPlot(Transactions, topN = 20,col="deepskyblue4",horiz = TRUE) #Only plot the topN items with the highest item frequency
itemFrequencyPlot(Transactions, support = 0.009,col="darkslateblue", horiz=TRUE) #support = Only display items which have a support of at least

#######Use apriori() algorithm to display the top ten rules (sort by confidence), and summary information.#####
## Build apriori model with Min Support as 0.005 and confidence as 0.70
rules <- apriori (Transactions, 
                  parameter = list(supp = 0.0005, conf = 0.7))
## Build apriori model with Min Support as 0.001 and confidence as 0.80
rules1 <- apriori (Transactions, 
                  parameter = list(supp = 0.001, conf = 0.8))
## Build apriori model with Min Support as 0.001 and confidence as 0.70
rules2 <- apriori (Transactions, 
                   parameter = list(supp = 0.001, conf = 0.7))
## Build apriori model with Min Support as 0.001 and confidence as 0.70
rules3 <- apriori (Transactions, 
                   parameter = list(supp = 0.001, conf = 0.6))
## Build apriori model with Min Support as 0.005 and confidence as 0.70
rules4 <- apriori (Transactions, 
                  parameter = list(supp = 0.001, conf = 0.5))

##top ten rules (sort by confidence), and summary information for rules2
summary(rules)
inspect(rules[1:12])
  
#order rules by confidence
rulessortconf <-sort(rules, by="confidence", decreasing=TRUE)
#order rules by support
rulessortsup <-sort(rules, by="support", decreasing=TRUE)
#order rules by lift
rulessortlift <-sort(rules, by="lift", decreasing=TRUE)
  
#look at the first 20 rules

inspect(rulessortconf[1:12])
inspect(rulessortsup[1:12])
inspect(rulessortlift[1:12])

##Subset and Exlude redundant rules
subset <- is.subset(rulessortlift, rulessortlift)
subset[lower.tri(subset, diag=T)] <- NA
redundant <- colSums(subset, na.rm=T) >= 1
pruned <- rules[!redundant]
rules.pruned<-pruned
rulessortlift
rules.pruned
plot(rulessortlift[1:10],method="graph",interactive=TRUE)

##rules with rhs containing "Rating=5"
rules <- apriori(Transactions,
        parameter = list(minlen=2, supp=0.001, conf=0.6),
        appearance = list(rhs=c("Book.Rating=5"),
                    default="lhs"),
                    control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#generate subsets of specific rules
# finding subsets of rules that precede book rating of 5
rating5 <- subset(Transactions, subset = rhs %in% "5")
#sort
rating5 <-sort(rating8, by="confidence", decreasing=TRUE)
inspect(rating5[1:5])

##Use library(e.g. aruleViz) to display rules
rules <- apriori(Transactions, parameter = list(supp = 0.0005, conf = 0.7))
rules <-sort(rules, by="confidence", decreasing=TRUE)
ig <- plot(rules, method="graph", control=list(type="items") )


#*******After going through exercise decided to go back to origianl file and exclude ratings=0 from the set*****#
#Filter dataset for ratings that do not equal 0
newbookrating <- filter(bxBooks, Book.Rating != 0)
summary(newbookrating)
##Reduce ratings dataset to 135,000 rows
ratings <- newbookrating[sample(nrow(newbookrating), 135000), ]
##RUN Through the code above. 
