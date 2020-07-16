library(tidyverse)
library(scales)

data <- readRDS(paste0("data/data_products_", Sys.Date(), ".RDS")) 

dir.create(paste0("charts/", Sys.Date()))

table(data$product_main_categories)
table(data$product_secondary_categories)
table(data$product_sub_categories)

data %>% group_by(sale_flag) %>% summarize(count = n()) %>% mutate(sale_flag = ifelse(is.na(sale_flag), 0, 1), count_pct = count / sum(count)) %>% 
  ggplot(aes(sale_flag, count)) + 
  geom_col() +
  geom_text(aes(label = scales::percent(count_pct)), position = position_dodge(width = 1), vjust = -0.5) +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous() +
  labs(title = paste0('Akciós termékek aránya ', Sys.Date()),
       x = 'Akciós', y = 'Db') +
  theme_bw()

ggsave(paste0("charts/", Sys.Date(), "/on_sale.png"))

data %>% group_by(product_main_categories, sale_flag) %>% summarize(count = n()) %>% mutate(sale_flag = ifelse(is.na(sale_flag), 0, 1), count_pct = count / sum(count)) %>% 
  ggplot(aes(sale_flag, count_pct)) + 
  geom_col() +
  geom_text(aes(label = scales::percent(round(count_pct, digits = 3))), position = position_dodge(width = 1.5), vjust = -1/3) +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 5L), breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  facet_wrap(~product_main_categories) +
  labs(title = paste0('Akciós termékek aránya ', Sys.Date()),
       x = NULL, y = NULL) +
  theme_bw()

ggsave(paste0("charts/", Sys.Date(), "/on_sale_by_cat.png"))

data %>% ggplot(aes(product_main_categories, abs(sale_rel), color = product_main_categories, fill = product_main_categories)) + 
  geom_boxplot(alpha = 0.5, show.legend = F, width = 1/2) +
  labs(x = NULL, y = NULL, title = paste0('Akciók eloszlása ', Sys.Date())) +
  scale_y_continuous(labels = percent_format(accuracy = 5L), breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(paste0("charts/", Sys.Date(), "/on_sale_by_cat_distribution.png"))


data %>% ggplot(aes(product_prices)) + 
  geom_histogram(show.legend = F) +
  labs(x = NULL, y = NULL, title = paste0('Árak eloszlása ', Sys.Date())) +
  scale_y_continuous(label = comma) +
  scale_x_continuous(label = comma, limits = c(0, 250000)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~product_main_categories, scales = 'free')
  
ggsave(paste0("charts/", Sys.Date(), "/prices_by_cat_distribution.png"))

