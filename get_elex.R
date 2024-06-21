

library(tidyverse)


full_cntry_list <-
  read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>%
  rename(iso2c = iso2,
         country = cntry) %>%
  sample_n(n()) 

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


beginn_month <- c("2024-06-01", "2024-05-01", "2024-04-01")
full_list <- expand_grid(beginn_month, iso2 = full_cntry_list$iso2c)

topspenders <- openxlsx::read.xlsx("topspenders.xlsx")

targets <- full_list %>%
  # filter(cntry %in% eu_countries) %>% 
  split(1:nrow(.)) %>% 
  map_dfr_progress(~{
    # print()
    elex <- NULL
    try({
      elex <-
        arrow::read_parquet(
          paste0(
            "https://github.com/favstats/meta_ad_targeting/releases/download/",
            .x$iso2,
            "-last_",
            30,
            "_days/",
            .x$beginn_month,
            ".parquet"
          )
        )  %>%
        mutate(cntry = .x$iso2) %>%
        filter(is.na(no_data))  %>%
        left_join(all_currencies) %>%
        mutate(total_spend_formatted = ifelse(total_spend_formatted == 100, 0.01, total_spend_formatted)) %>%
        mutate(dollar_spend = total_spend_formatted / conversion_rate) %>% 
        filter(page_id %in% topspenders$page_id)
    })
    
    return(elex)
  })


saveRDS(targets, file = "data/election_dat30.rds")
