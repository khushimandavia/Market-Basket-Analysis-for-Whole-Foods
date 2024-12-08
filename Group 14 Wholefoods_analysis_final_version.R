
##### load packages #####
library(tidyverse)
library(readr)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(gridExtra)
library(readr)



##### load data #####

wf <- read_csv("Whole_Foods_Transaction_Data.csv")
View(wf)




##### Data exploration #####

summary(wf) # -->> found 5 NA in department

#check how many 'missing' in department column
sum(wf$department == "missing") # -->> 541 rows contain 'missing' in department column

#check unique product
length(unique(wf$product_name))
#23321 unique product

#check unique department
length(unique(wf$department))
#21 unique department

#check unique aisle
length(unique(wf$aisle))
#134 aisle





##### Data Cleaning #####

#delete rows contain missing
wf_no_missing <- wf %>%
  filter(department != "missing", aisle != "missing")

# remove NAs
wf<-drop_na(wf) # -->> product analysis
wf_no_missing<-drop_na(wf_no_missing) # -->> department analysis





##### Product Sold  #####

#Product Sold Count
wf1 <- wf %>%
  group_by(product_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# plot
ggplot(head(wf1, 30),
       aes(x = reorder(product_name, -count),y = count)) + 
  geom_bar(stat = "identity", fill = "skyblue", width = 0.8) +
  labs(x = "Product Name", y = "Count", title = "Top 30 Products Bought") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# mostly fresh produce product





##### Aisles Sold #####
#total sold in aisles
portfolio <- wf %>%
  group_by(aisle) %>%
  summarise(total_count = n(), .groups = 'drop')

# Convert aisles to a factor and order by total_count
portfolio$aisle <- factor(portfolio$aisle, levels = portfolio$aisle[order(-portfolio$total_count)])

# Select the top 15
top_aisles <- portfolio %>%
  top_n(15, total_count)

# Plot
ggplot(top_aisles, aes(x = aisle, y = total_count, fill = aisle)) +
  geom_col() +
  labs(title = "Top 15 Aisles by Transaction Count",
       y = "Transaction Count", x = "") +
  scale_fill_discrete(name = "Department") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






##### department sold #####

# Group by department, count transactions
portfolio <- wf %>%
  group_by(department) %>%
  summarise(total_count = n(), .groups = 'drop')

# Convert department to a factor and order by total_count
portfolio$department <- factor(portfolio$department, 
                               levels = portfolio$department[order(-portfolio$total_count)])

# Select the top 10 departments by total transaction count
top_departments <- portfolio %>%
  top_n(10, total_count)

# Plot
ggplot(top_departments, aes(x = department, y = total_count, fill = department)) +
  geom_col() +
  labs(title = "Top 10 Departments by Transaction Count",
       y = "Transaction Count", x = "") +
  scale_fill_discrete(name = "Department") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#####  Product Association Rules ##### 

#group by id
transactions<-wf%>%
  group_by(transaction_id)%>%
  summarise(items = list(product_name))%>%
  ungroup()

# transform to transaction format
transactions <- as(transactions$items, "transactions")

#testing different level of support and confidence
#1
rules1 <- apriori(transactions,parameter = list(supp=0.00035,conf=0.5))
summary(rules1)

#2
rules2 <- apriori(transactions,parameter = list(supp=0.0005,conf=0.5))
summary(rules2)

#3
rules3 <- apriori(transactions,parameter = list(supp=0.00035,conf=0.7))
summary(rules3)


#inspect top 10 using the rules1
sorted_rules1 <- sort(rule1, by='lift', decreasing = T)
inspect(head(sorted_rules1, n = 10))






##### Department Association Rules ##### 

#group by department 
department_transations<- wf_no_missing%>%
  group_by(transaction_id)%>%
  summarise(department= list(unique(department)))%>%
  ungroup()

# transform to transaction format
department_transations<- as(department_transations$department,"transactions")


#1
rules_department1 <- apriori(department_transations,parameter = list(supp=0.05,conf=0.8))
summary(rules_department)
#support=0.05 confidence=0.8 281 rules, median lift = 1.228
#2
rules_department2 <- apriori(department_transations,parameter = list(supp=0.03,conf=0.8))
summary(rules_department)
#support=0.03 confidence=0.8 636 rules, median lift = 1.248
#3
rules_department3 <- apriori(department_transations,parameter = list(supp=0.05,conf=0.5))
summary(rules_department)
#support=0.05 confidence=0.5 630 rules, median lift = 1.2886


#inspect department
two_item_rules = subset(rules_department1, 
                        subset = size(lhs(rules_department1)) + size(rhs(rules_department1)) == 2)

sorted_rules_department1 <- sort(two_item_rules, by = "lift", decreasing = TRUE)
inspect(head(sorted_rules_department1,n=10))









