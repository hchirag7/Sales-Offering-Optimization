#### Packages and libraries ####

# install.packages("tidyverse")
# install.packages("tibble")

library("tidyverse")
library("tibble")

#### Read data into environment #####

# Save these .csv-files into your working directory:
# olist_order_items_dataset.csv
# olist_orders_dataset.csv
# olist_products_dataset.csv
# olist_sellers_dataset.csv
# product_category_name_translation.csv

order_items.df <- as_tibble(read_csv("olist_order_items_dataset.csv"))
orders.df <- as_tibble(read_csv("olist_orders_dataset.csv"))
products.df <- as_tibble(read_csv("olist_products_dataset.csv"))
sellers.df <- as_tibble(read_csv("olist_sellers_dataset.csv"))
name_translations.df <- as_tibble(read_csv("product_category_name_translation.csv"))

# Merge data
olist_combined.df <- merge(order_items.df, orders.df)
olist_combined.df <- merge(olist_combined.df, products.df)
olist_combined.df <- merge(olist_combined.df, sellers.df)

olist_combined.df <- as_tibble(olist_combined.df)
olist_combined.df <- olist_combined.df %>% select(order_id, product_id, seller_id,customer_id,
                                                  price, product_category_name, product_name_lenght,
                                                  order_delivered_customer_date, seller_city,
                                                  seller_state)
# to replace category names
#rename cols
names(olist_combined.df)[names(olist_combined.df) == "product_category_name"] <- "names"
colnames(name_translations.df) <- c("names", "value")
#blank column
olist_combined.df$product_category_name <- NA 
#re-ordering columns by name
olist_combined.df <- olist_combined.df[c("order_id", "product_id", "seller_id","customer_id",
                                         "price", "product_category_name", "names", "product_name_lenght",
                                         "order_delivered_customer_date", "seller_city",
                                         "seller_state")]
#populate translated values
olist_combined.df$product_category_name <- name_translations.df[match(olist_combined.df$names,
                                                                    name_translations.df$names),2]
olist_combined.df$product_category_name <- olist_combined.df$product_category_name$value

#remove spanish names
olist_combined.df$names <- NULL

# remove unnecessary vars
remove(order_items.df)
remove(orders.df)
remove(products.df)
remove(sellers.df)
remove(name_translations.df)

# Workable data info
summary(olist_combined.df)

#### Associations Mark5 ####
# approach: (trans: customer only, single transactions) df > csv > trans > rules
# relevant library
library(arules)
# column selection
olist_association.df <- olist_combined.df %>% select(customer_id, product_category_name)
# handling NA product categories and delivery dates
olist_association.df <- na.omit(olist_association.df) 
# sorting
olist_association.df <- olist_association.df[order(olist_association.df$customer_id),]
# Rename column headers for ease of use
colnames(olist_association.df) <- c("transactions", "itemList")
# remove order id column
#olist_association.df$transactions <- NULL
# check association dataset 
summary(olist_association.df)
# write a csv file
write.csv(olist_association.df,"Olist_ItemList.csv", row.names = TRUE)
# read trans
olist_itemlist.trans = read.transactions(file="Olist_ItemList.csv", rm.duplicates= TRUE,
                                        format="single",header = TRUE,sep=",",
                                        cols=c("transactions", "itemList"))
# check transactions
inspect(head(olist_itemlist.trans,n=10))
# rules
association_rules <- apriori(olist_itemlist.trans,
                                  parameter = list(sup = 0.00001, conf = 0.1,
                                                   target="rules", minlen = 2))
options(scipen=999) #disables showing values in e form
inspect(head(sort(association_rules, by = "lift"), n = 20))
# remove extra vars
remove(olist_association.df)
remove(olist_itemlist.trans)
# conclusion: Trial Success