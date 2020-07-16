library(tidyverse)
library(data.table)

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

mf <- merge(mf, data, by = c("product_codes", "product_names", "product_main_categories", "product_sub_categories"), all.x = T)

saveRDS(mf, "data/masterfile.RDS")
