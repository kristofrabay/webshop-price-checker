library(tidyverse)
library(data.table)
library(matrixStats)


# TODO joining method
# 1. previous file containing all data scraped previously
# 2. new data scraped as of Sys.Date()
#  add new columns to masterfile

# read in masterfile

mf <- readRDS("data/masterfile.RDS")

# get new file ready

data <- readRDS(paste0("data/data_products_", Sys.Date(), ".RDS")) 

to_drop <- c("product_secondary_categories", "sale_abs", "sale_flag", "sale_rel")
data <- data.table(data)
data[, (to_drop) := NULL]

names(data)[5] <- paste0('price_current_', Sys.Date())
names(data)[6] <- paste0('price_old_', Sys.Date())

# join mf with new data

mf <- merge(mf, data, 
            by = c("product_codes", "product_names", "product_main_categories", "product_sub_categories"), 
            all = T) 

View(mf)

saveRDS(mf, "data/masterfile.RDS")


# current and prev scraped date comparison
# TODO: prev won't necessarily be yesterday!

current_price_col <- paste0('price_current_', Sys.Date())
previous_price_col <- paste0('price_current_', Sys.Date() - 2)

# products not available any more
sold_out <- mf %>% 
  filter(is.na(get(current_price_col)) & !is.na(get(previous_price_col)))

sold_out %>% group_by(product_main_categories) %>% 
  summarize(count = n()) %>% ungroup() %>% arrange(desc(count)) %>% 
  mutate(total = sum(count),
         rel = count / total)

# new products
new <- mf %>% 
  filter(!is.na(get(current_price_col)) & is.na(get(previous_price_col)))

new %>% group_by(product_main_categories) %>% 
  summarize(count = n()) %>% ungroup() %>% arrange(desc(count)) %>% 
  mutate(total = sum(count),
         rel = count / total)


# net change

net_change <- nrow(new) - nrow(sold_out)

if (net_change >= 0) {
  print(paste0(net_change, " more products on website from last time"))
} else {
  print(paste0(abs(net_change), " less products on website from last time"))
}


### any price change of a certain product?

# from prev to current date

price_change <- mf %>% 
  filter(!is.na(get(current_price_col)) & !is.na(get(previous_price_col))) %>% 
  filter(get(current_price_col) != get(previous_price_col))
View(price_change)

# price variance overall

price_cols <- names(mf)[names(mf) %like% "price_current"]
mf$price_var <- sqrt(rowVars(as.matrix(mf[, ..price_cols])))
price_changes <- mf %>% filter(price_var != 0)
View(price_changes)
saveRDS(price_changes, "data/price_changes.RDS")
