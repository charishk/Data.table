# Setup
library(pacman) 
pacman::p_load(data.table, gmodels) 

# Load data
data_amazon <- fread("path",
                     data.table=T,
                     stringsAsFactors = F)

# q1 - What per cent of Amazon's customers are female?
# 1. first method
sum(data_amazon$gender == "F")/ nrow(data_amazon)
# 2. use CrossTable
CrossTable(data_amazon$gender)

# q2 - Which 3 cities account for the largest % of Amazon's customers?
data_amazon[,.(number_of_customers=.N),by=.(city)][order(-number_of_customers)]

# q3 - Find avg total £ spent, avg total # of electronic product purchases, and avg number of months since last purchase
data_amazon[,.(mean_total = mean(total, na.rm=T),
               mean_electro = mean(electronics, na.rm=T),
               mean_last_purchase = mean(last, na.rm=T))]

# q4 - Calculate corr bet total spending on electronic products and their 
# total spending on non-electronic products. 
# Is the correlation statistically significant?
cor(data_amazon$electronics, data_amazon$nonelectronics)
cor.test(data_amazon$electronics, data_amazon$nonelectronics) # the p value is very small, so the coefficient is positive and significant!

# q5 - Use a regression as another way to determine whether customers' total spending on electronic 
# products and their total spending on non-electronic products is correlated. 
# Is the coefficient significant?
regression <- lm(data = data_amazon,
                 formula = electronics~nonelectronics)
summary(regression) # positive correlation and significant

# q6 - Which product categories have the most and least sales?
## extract the names of all variables
names_of_categories <- names(data_amazon)[9:15]
## we can use double dot to select cols named in a var
data_small <- data_amazon[,..names_of_categories]
## use sapply to compute the sum of sales for each category
sapply(data_small,sum) # sapply return vec or mat
## sort the order of sales in ascending order
sort(sapply(data_small,sum))

# q7 - Do males on average spend more on electronics than females?
data_amazon[,.(spending_electro = mean(electronics)),# since this dataset does not have missing values, we can leave out na.rm
            by = .(gender)]

# q8 - Is avg total spending on electronic products statistically different for males and females? 
t.test(data_amazon$electronics, data_amazon$gender=="F") # significantly different!

# q9 and for non-electronic products?
t.test(data_amazon$nonelectronics, data_amazon$gender=="F") # also significantly different!

# q10 - For both genders, find the total number and also the % subscribed to Amazon Prime.
data_amazon[,.(n_prime = sum(subscribe == "yes"),
               total_number_people = .N,
               percent_prime = sum(subscribe == "yes") /.N ),
            by = .(gender)]

# q11 - Which city has the most female prime members?
# use chain operation
data_amazon[,.(n_prime = sum(subscribe == "yes")),
            by = .(city,gender)][gender == "F"][order(-n_prime)]

# q12 - For both genders, determine the total number of purchases and the avg number of purchases by males vs females.
data_amazon[,.(total_number_of_purchase = sum(quantity),
               avg_number_of_purchase = sum(quantity) / .N),
            by = .(gender)]

# q13 - Determine the min, max, and avg number of months bet customers' first purchase and their most recent purchase.
# generate a new dataset with the months between first and last purchase
data_new <- data_amazon[,.(month_between = first - last)]
min(data_new$month_between)
mean(data_new$month_between)
max(data_new$month_between)

# q14 - What % of repeat customers (those with >=2 total purchases) subscribed to Amazon Prime?
# use chain operation
data_amazon[quantity >=2][,.(percent = sum(subscribe == "yes")/.N )]
