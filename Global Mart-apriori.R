#########################################

          # Global Mart #

#########################################       

#clear Enivronment

rm(list=ls())
detach("package:arulesViz",unload = TRUE)

#Loading the packages

install.packages("arules")
install.packages("xts")
install.packages("psych")

library("arulesViz")
library(Matrix)
library(psych)
library(arules)

#setting the working directory
  setwd("E:/Upgrad R/module 4/Association Rules Mining/Assignment")  

# Data Understanding & Data Preparation ------------------------------------------------------

# Read GlobalMart Dataset into system
  mart <- read.csv("Global Superstore.csv",stringsAsFactors = T, header = T, strip.white = T)
  dim(mart)
  View(mart)
  trans <- mart[,c(2,17)]
   
# Understand the structure of the collated file

  describe(trans)
  str(trans)
  head(trans)
  tail(trans)
  unique(trans$Sub.Category)
  unique(trans$Order.ID)
  


# Date Prepration
#Coverting data into transcations structure 
rules = apriori(trans)
inspect(trans)


#converting mart_mini1 to transactions
store_Transactions1 <- as(trans, "transactions")
str(store_Transactions1)

#do initial explatory analysis
itemFrequencyPlot(trans,topN=10,type="absolute") 

#We can explore the transaction level data using apriori principle


rules = apriori(store_Transactions1)
rules = apriori(store_Transactions1, parameter = list(support = 0.02, confidence=0.01, minlen=2))
inspect(rules)

#reduce to smaller number of rules
rules2 <- apriori(store_Transactions1, parameter = list(support=0.005, confidence=0.01, minlen=3))
summary(rules2)
inspect(rules2)
rules3 <- apriori(store_Transactions1, parameter = list(support=0.005, confidence=0.01, minlen=3))
top5<-head(rules3, n=5, by= "confidence")
inspect(top5)


rules_without_binders2 <- subset(rules2,(subset= rhs %in% c('Accessories',
                                                            'Appliances',
                                                            'Bookcases',
                                                            'Chairs',
                                                            'Copiers',
                                                            'Envelopes',
                                                            'Fasteners',
                                                            'Furnishings',
                                                            'Labels',
                                                            'Machines',
                                                            'Paper',
                                                            'Phones',
                                                            'Supplies',
                                                            'Tables',
                                                            'Storage',
                                                            'Art')))

top5<-head(rules_without_binders2, n=5, by= "confidence")

inspect(top5)

rules_without_binders3 <- subset(rules3,(subset= rhs %in% c('Accessories',
                                                            'Appliances',
                                                            'Bookcases',
                                                            'Chairs',
                                                            'Copiers',
                                                            'Envelopes',
                                                            'Fasteners',
                                                            'Furnishings',
                                                            'Labels',
                                                            'Machines',
                                                            'Paper',
                                                            'Phones',
                                                            'Supplies',
                                                            'Tables',
                                                            'Storage',
                                                            'Art')))

top5<-head(rules_without_binders3, n=5, by= "confidence")

inspect(top5)
