library(tidyverse)
library(rvest)
library(data.table)
library(pbapply)

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


extract_product_data <- function(x) {
  
  read_page <- read_html(x)
  
  product_names <- read_page %>% html_nodes("h2 a") %>% html_text(trim = T)
  
  if (!identical(product_names, character(0))) {
    
    product_codes <- read_page %>% html_nodes(".printonly") %>% html_text(trim = T) %>% str_remove_all("Cikkszám: ")
    product_prices <- read_page %>% html_nodes(".small") %>% html_text() %>% as.numeric()
    product_prices_old <- read_page %>% html_nodes('.infobox') %>% html_node(".price-old") %>% html_text() %>% str_replace_all("[\r\n\t]", "") %>% as.numeric()
    product_prices_old <- product_prices_old[c(T, F)]
    
    data <- data.frame(product_names = product_names,
                       product_codes = product_codes,
                       product_prices = product_prices,
                       product_prices_old = product_prices_old,
                       sale_flag = ifelse(((product_prices_old != "NA") & (product_prices_old != product_prices)),1,0))
    
  }
  
}

data <- rbindlist(pblapply(products_links[[1]], extract_product_data))
View(data)

saveRDS(data, "data/data_products.RDS")

# error
#Error in data.frame(product_names = product_names, product_codes = product_codes,  : 
  #                    arguments imply differing number of rows: 24, 23

# debug around 240-306