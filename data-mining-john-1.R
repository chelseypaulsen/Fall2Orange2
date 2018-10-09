library(tidyverse)
library(reshape2)
library(haven)

setwd('C:/Users/johnb/OneDrive/Documents/MSA/Fall 2/Data Mining/HW1/')
# read in the csv
restaurant <- read_csv('restaurant.csv')

# Inspect the dataset
restaurant %>% filter(type=='Meat') %>% group_by(order) %>% summarise(n=n())
restaurant %>% filter(type=='Wine') %>% group_by(order) %>% summarise(n=n())
restaurant %>% filter(type=='Side') %>% group_by(order) %>% summarise(n=n())

restaurant %>% select(order) %>% group_by(order) %>% summarise(n=n()) %>%
  arrange(desc(n))

restaurant %>% group_by(orderNumber) %>% summarise(n=n()) %>% filter(n>3) %>% nrow()
restaurant %>% group_by(orderNumber) %>% summarise(n=n()) %>% filter(n<3) %>% nrow()

head(restaurant)
# aggregate the orders onto single lines for association analysis
restaurant.wide <- aggregate(order~orderNumber,
                             data=restaurant,
                             paste,collapse=',')
# make it a dataframe
restaurant.wide <- data.frame(ordernumber=restaurant.wide$orderNumber,
                              order=restaurant.wide$order)

restaurant.wide %>% group_by(order) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head()

typeof(restaurant.wide)
# write the new wide dataframe to a csv
write.table(restaurant.wide,
          'restaurant_wide.csv')
# one without the id column for later use
restaurant.wide %>% select(order) %>% write.table('restaurant_wide2.csv',quote=F,
                                                sep=',',eol='\r\n',
                                                row.name=F,col.name=F)
# NOTE: the below code does create a .sas7bdat file but it cannot be opened
# in SAS, this is a known bug
write_sas(restaurant.wide,
          'restaurantWide.sas7bdat')

# Association analysis
#install.packages('arules')
library(arules)

orders <- read.transactions('restaurant_wide2.csv',sep=',',rm.duplicates=F)
summary(orders)

inspect(orders[1:10])

rules <- apriori(orders, parameter = list(support=0.05, confidence=0.25, minlen=2))
summary(rules)

inspect(sort(rules,by='lift')[1:20])


rules2 <- apriori(orders, parameter = list(support=0.006, confidence=0.25, minlen=2))
summary(rules2)

inspect(sort(rules2,by='lift')[1:20])
