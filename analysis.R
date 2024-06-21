
library(tidyverse)


full_cntry_list <-
  read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>%
  rename(iso2c = iso2,
         country = cntry) %>%
  sample_n(n()) 



linkie <- function(.x, cntry2) {
  download.file(glue::glue("https://github.com/favstats/meta_ad_reports/releases/download/{cntry2}-last_30_days/{.x}.rds"), destfile = "data.rds", quiet = T)
  
  fin <- read_rds("data.rds") %>% 
    mutate(cntry = cntry2)
  
  Sys.sleep(0.1)
  file.remove("data.rds")
  
  return(fin)
}

linkie2 <- possibly(linkie, otherwise = NULL, quiet = F)

beginn_month <- c("2024-06-01", "2024-05-01", "2024-04-01")

full_list <- expand_grid(beginn_month, iso2 = full_cntry_list$iso2c)

all_spends <- full_list %>% 
  # slice(1:10) %>% 
  split(1:nrow(.)) %>% 
  map_dfr_progress(~{
    linkie2(.x$beginn_month, .x$iso2)
  })

saveRDS(all_spends, "data/all_spends.rds")


all_spends %>% 
  distinct(page_id, .keep_all = T) %>% View()

all_spends %>% 
  distinct(page_id, cntry, .keep_all = T) %>% View()


all_spends %>% 
  distinct(page_id, cntry, .keep_all = T) %>% 
  count(page_id, page_name, sort = T) %>% View()


all_currencies <- c("2024-03-31", "2024-04-30", "2024-05-31") %>% 
  map_dfr_progress(~{
    
    conversios <- jsonlite::fromJSON(paste0("https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@", .x ,"/v1/currencies/usd.json")) %>% 
      pluck("usd") %>% 
      enframe() %>% 
      unnest(value) %>% 
      distinct(name, .keep_all = T) %>% 
      mutate(main_currency = str_to_upper(name)) %>% 
      filter(str_count(main_currency)==3) %>% 
      mutate(conversion_rate = value) %>% 
      select(-name, -value) %>% 
      mutate(date = .x)
    
    return(conversios)
    
  })

all_spends

thecurrent <- all_spends %>% 
  select(contains("amount_spent")) %>% 
  names() %>% 
  str_remove_all("amount_spent_") %>% 
  str_to_upper()


all_spends_clean <- all_spends %>% 
  select(page_id, contains("amount_spent"), cntry, date) %>% 
  gather(currency, amount_spent, -page_id, -cntry, -date) %>% 
  mutate(currency = str_remove(currency, "amount_spent_")) %>% 
  mutate(currency = str_to_upper(currency)) %>% 
  drop_na(amount_spent) %>% 
  mutate(amount_spent = parse_number(amount_spent)) %>% 
  mutate(amount_spent = ifelse(amount_spent == 100, 0.01, amount_spent))

saveRDS(all_spends_clean, file = "data/all_spends_clean.rds")
# 
# all_spends_clean %>% count(date)
# 
# elex <- all_spends_clean %>% 
#   # sample_n(20) %>%  
#   left_join(all_currencies %>% rename(currency = main_currency)) %>%
#   mutate(dollar_spend = amount_spent / conversion_rate) 
# 
elex <- all_spends %>%
  # sample_n(20) %>%
  left_join(all_currencies %>% rename(currency = main_currency)) %>%
  mutate(dollar_spend = amount_spent / conversion_rate)

saveRDS(elex, file = "data/all_spends_clean.rds")


# elex %>% 
#   group_by(page_id, cntry) %>% 
#   summarize(dollar_spend = sum(dollar_spend)) %>% 
#   ungroup() %>% 
#   arrange(desc(dollar_spend)) %>% 
#   add_count(page_id) %>% 
#   filter(n > 1)
# 
# 
# 
# # Load necessary libraries
# library(dplyr)
# 
# # Sample dataset (replace this with your actual data reading code)
# data <- data.frame(
#   advertiser = c("A", "A", "A", "B", "B", "C", "C", rep("D", 20)),
#   country = c("US", "UK", "CA", "US", "FR", "US", "NL", paste0("Country", 1:20)),
#   spending = c(200, 200, 200, 400, 100, 500, 0.01, rep(100, 20))
# )
# 
# # advertiser  nHHI
# # <chr>      <dbl>
# #   1 A          0    
# # 2 B          0.360
# # 3 C          1.00 
# # 4 D          0    
# ## ZZERROOOO IS GOOOD!
# 
# # Function to calculate the normalized HHI
# calculate_normalized_hhi <- function(spending) {
#   N <- length(spending)
#   HHI <- sum((spending / sum(spending))^2)
#   nHHI <- (HHI - 1/N) / (1 - 1/N)
#   if(nHHI < 0.00001){
#     nHHI <- 0
#   }
#   return(nHHI)
# }
# 
# # Calculate the normalized Herfindahl-Hirschman Index (nHHI) for each advertiser
# normalized_hhi_coefficients <- data %>%
#   group_by(advertiser) %>%
#   summarise(nHHI = calculate_normalized_hhi(spending))
# 
# # Print the results
# normalized_hhi_coefficients %>% 
#   mutate(nHHI = as.character(nHHI))
# 
# distrib_dat <- elex %>% 
#   group_by(page_id, cntry) %>% 
#   summarize(dollar_spend = sum(dollar_spend)) %>% 
#   ungroup() %>% 
#   group_by(page_id) %>% 
#   mutate(total_spend = sum(dollar_spend)) %>%
#   ungroup() %>% 
#   arrange(desc(dollar_spend)) %>% 
#   add_count(page_id) %>% 
#   filter(n > 1) %>% 
#   filter(total_spend >= 1000)
# 
# distrib_dat %>%
#   tidycomm::tab_percentiles(total_spend, levels = c(0.9))
# 
# distrib_dat %>%
#   tidycomm::tab_percentiles(n)
# # 
# 
# distrib_dat %>% 
#   filter(n > 10) %>% 
#   group_by(page_id) %>%
#   summarise(nHHI = calculate_normalized_hhi(dollar_spend)) %>% 
#   ungroup() %>% 
#   arrange(nHHI) %>% 
#   left_join(all_spends %>% distinct(page_id, page_name)) %>%
#   left_join(distrib_dat %>% select(page_id, total_spend) %>% distinct()) %>% 
#   View()
# 
# # linkie("2024-06-01", "BE")
# 
