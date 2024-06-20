



all_spends_clean <- all_spends %>% 
  select(page_name, contains("amount_spent"), cntry, date) %>% 
  gather(currency, amount_spent, -page_name, -cntry, -date) %>% 
  mutate(currency = str_remove(currency, "amount_spent_")) %>% 
  mutate(currency = str_to_upper(currency)) %>% 
  drop_na(amount_spent) %>% 
  mutate(amount_spent = parse_number(amount_spent)) %>% 
  mutate(amount_spent = ifelse(amount_spent == 100, 0.01, amount_spent))



all_spends_clean %>% count(date)

elex <- all_spends_clean %>% 
  # sample_n(20) %>%  
  left_join(all_currencies %>% rename(currency = main_currency)) %>%
  mutate(dollar_spend = amount_spent / conversion_rate) 


elex %>% 
  group_by(page_name, cntry) %>% 
  summarize(dollar_spend = sum(dollar_spend)) %>% 
  ungroup() %>% 
  arrange(desc(dollar_spend)) %>% 
  add_count(page_name) %>% 
  filter(n > 1)



# Load necessary libraries
library(dplyr)

# Sample dataset (replace this with your actual data reading code)
data <- data.frame(
  advertiser = c("A", "A", "A", "B", "B", "C", "C", rep("D", 20)),
  country = c("US", "UK", "CA", "US", "FR", "US", "NL", paste0("Country", 1:20)),
  spending = c(200, 200, 200, 400, 100, 500, 0.01, rep(100, 20))
)

# advertiser  nHHI
# <chr>      <dbl>
#   1 A          0    
# 2 B          0.360
# 3 C          1.00 
# 4 D          0    
## ZZERROOOO IS GOOOD!

# Function to calculate the normalized HHI
calculate_normalized_hhi <- function(spending) {
  N <- length(spending)
  HHI <- sum((spending / sum(spending))^2)
  nHHI <- (HHI - 1/N) / (1 - 1/N)
  if(nHHI < 0.00001){
    nHHI <- 0
  }
  return(nHHI)
}


distrib_dat <- elex %>% 
  group_by(page_name, cntry) %>% 
  summarize(dollar_spend = sum(dollar_spend)) %>% 
  ungroup() %>% 
  group_by(page_name) %>% 
  mutate(total_spend = sum(dollar_spend)) %>%
  ungroup() %>% 
  arrange(desc(dollar_spend)) %>% 
  add_count(page_name) %>% 
  filter(n > 1) %>% 
  filter(total_spend >= 1000)

distrib_dat %>%
  tidycomm::tab_percentiles(total_spend, levels = c(0.9))

distrib_dat %>%
  tidycomm::tab_percentiles(n)
# 

distrib_dat %>% 
  filter(n > 10) %>% 
  group_by(page_name) %>%
  summarise(nHHI = calculate_normalized_hhi(dollar_spend)) %>% 
  ungroup() %>% 
  arrange(nHHI) %>% 
  # left_join(all_spends %>% distinct(page_name, page_name)) %>%
  left_join(distrib_dat %>% select(page_name, total_spend) %>% distinct()) %>% 
  View()

# linkie("2024-06-01", "BE")
