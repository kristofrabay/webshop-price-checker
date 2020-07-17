library(tidyverse)
library(rvest)
library(data.table)
library(pbapply)
library(httr)

### get links to subcategories

main_link <- "https://www.mediamarkt.hu/?ref=logo_rh"

main_categories_link <- read_html(main_link) %>% 
  html_nodes(".ms-row__column:nth-child(3) .ms-link") %>% 
  html_attr("href") %>% 
  str_replace_all("//", "https://")


read_sub_category_links <- function(x) {
  
  return(list(read_html(x) %>% 
  html_nodes(".categories-flat-descendants a") %>% 
  html_attr("href") %>% 
  str_replace_all("/hu/cat", "https://www.mediamarkt.hu/hu/cat")))
  
}

sub_categories_links <- rbindlist(pblapply(main_categories_link, read_sub_category_links))

saveRDS(sub_categories_links, "data/links_categories.RDS")

rm(main_link, main_categories_link, sub_categories_links)


### get links to subcategories

links <- readRDS("data/links_categories.RDS")

products_links <- rbindlist(pblapply(links[[1]], function(link) {
  
  last_page <- read_html(link[[1]]) %>% html_node(".hellip+ li a") %>% html_text() %>% as.numeric()
  
  if (is.na(last_page)) {
    all_links_to_scrape <- paste0(link[[1]], "?searchParams=&sort=&view=PRODUCTLIST&page=", 1:4)
  } else {
    all_links_to_scrape <- paste0(link[[1]], "?searchParams=&sort=&view=PRODUCTLIST&page=", 1:last_page)
  }
  
  return(list(all_links_to_scrape))
  
}))

saveRDS(products_links, "data/links_products.RDS")


### get product info by going through every link's every page

links <- readRDS("data/links_products.RDS")


extract_product_data <- function(link) {
  
  if (as.integer(runif(1, 0, 100)) < 10) {
    page <- read_html(GET(link, add_headers('user-agent' = 'mozilla')))
  } else if (as.integer(runif(1, 0, 100)) < 20) {
    page <- read_html(GET(link, add_headers('user-agent' = 'chrome')))
  } else if (as.integer(runif(1, 0, 100)) < 30) {
    page <- read_html(GET(link, add_headers('user-agent' = 'edge')))
  } else if (as.integer(runif(1, 0, 100)) < 40) {
    page <- read_html(GET(link, add_headers('user-agent' = 'safari')))
  } else if (as.integer(runif(1, 0, 100)) < 50) {
    page <- read_html(GET(link, add_headers('user-agent' = 'express')))
  } else if (as.integer(runif(1, 0, 100)) < 60) {
    page <- read_html(GET(link, add_headers('user-agent' = 'opera')))
  } else if (as.integer(runif(1, 0, 100)) < 70) {
    page <- read_html(GET(link, add_headers('user-agent' = 'microsoft')))
  } else if (as.integer(runif(1, 0, 100)) < 80) {
    page <- read_html(GET(link, add_headers('user-agent' = 'apple')))
  } else if (as.integer(runif(1, 0, 100)) < 90) {
    page <- read_html(GET(link, add_headers('user-agent' = 'google')))
  } else {
    page <- read_html(GET(link, add_headers('user-agent' = 'facebook')))
  }
  
  product_names <- page %>% html_nodes("h2 a") %>% html_text(trim = T)
  
  if (!identical(product_names, character(0))) {
    
    product_main_categories <- page %>% html_nodes(".home+ li a") %>% html_text()
    product_secondary_categories <- page %>% html_nodes(".home~ li+ li a") %>% html_text()
    product_secondary_categories <- ifelse(identical(product_secondary_categories, character(0)), NA, product_secondary_categories)
    product_sub_categories <- page %>% html_nodes("h1") %>% html_text()
    product_codes <- page %>% html_nodes(".printonly") %>% html_text(trim = T) %>% str_remove_all("Cikkszám: ")
    product_prices <- page %>% html_nodes(".small") %>% html_text() %>% as.numeric()
    product_prices_old <- page %>% html_nodes('.infobox') %>% html_node(".price-old") %>% html_text() %>% str_replace_all("[\r\n\t]", "") %>% as.numeric()
    product_prices_old <- product_prices_old[c(T, F)]
    
    if (length(product_names) == length(product_prices)) {
      
      data <- data.frame(product_names = product_names,
                         product_main_categories = product_main_categories,
                         product_secondary_categories = product_secondary_categories,
                         product_sub_categories = product_sub_categories,
                        product_codes = product_codes,
                        product_prices = product_prices,
                        product_prices_old = product_prices_old,
                        sale_flag = ifelse(((product_prices_old != "NA") & (product_prices_old != product_prices)),1,0))
      
    }
    
  }
  
}

data <- rbindlist(pblapply(links[[1]], extract_product_data))

data <- distinct(data, product_codes, .keep_all = TRUE)

data$sale_abs <- data$product_prices - data$product_prices_old
data$sale_rel <- (data$product_prices - data$product_prices_old) / data$product_prices_old

data$product_sub_categories <- trimws(str_replace(data$product_sub_categories, " \\(.*\\)", ""))

saveRDS(data, paste0("data/data_products_", Sys.Date(), ".RDS")) 

