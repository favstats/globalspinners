

elex %>% 
  sample_n(100) %>% 
  group_split(cntry) %>% 
  # arrange(desc(dollar_spend)) %>%
  map_dfr(~{
    .x %>% tidycomm::tab_percentiles(dollar_spend)
  })
  
  slice(1:10) 

  
toppers <-  elex  %>% 
  filter(dollar_spend >= 100) %>% 
    # sample_n(100) %>% 
    group_by(cntry) %>% 
    summarize(dollar = quantile(dollar_spend, 0.99)) %>% 
    ungroup() %>% 
    arrange(desc(dollar))
    
  


  # filter(dollar_spend >= 100) %>% View()
  count(cntry, sort = T)
  
  
library(metatargetr)
  
  get_page_insights2 <- possibly(get_page_insights, otherwise = NULL, quiet = F)

page_info_dat <- elex %>% 
    left_join(toppers) %>% 
    filter(dollar_spend >= dollar) %>% 
    distinct(page_id) %>% 
    pull(page_id) %>% 
    map_dfr_progress(~{
     get_page_insights2(pageid=.x,
                                         # lang="en-GB", iso2c="US", 
                                         include_info=c("page_info"))
    })



page_info_dat2 <- elex %>% 
  left_join(toppers) %>% 
  filter(dollar_spend >= dollar) %>% 
  distinct(page_id) %>% 
  anti_join(page_info_dat %>% select(page_id)) %>% 
  pull(page_id) %>% 
  map_dfr_progress(~{
    get_page_insights2(pageid=.x,
                       # lang="en-GB", iso2c="US", 
                       include_info=c("page_info"))
  })


remainers <- elex %>% 
  left_join(toppers) %>% 
  filter(dollar_spend >= dollar) %>% 
  distinct(page_id) %>% 
  anti_join(page_info_dat %>% select(page_id) %>% 
              bind_rows(page_info_dat2 %>% select(page_id))) %>% 
  pull(page_id) 

page_info_dat3 <- readRDS("data-LK.rds")


page_info_dat %>% 
  bind_rows(page_info_dat2) %>% 
  bind_rows(page_info_dat3) %>% 
  as_tibble() %>% 
  count(page_category, sort = T) %>% View()


page_info_dat %>% 
  bind_rows(page_info_dat2) %>% 
  bind_rows(page_info_dat3) %>% 
  as_tibble() %>% 
  left_join(elex %>% group_by(page_id) %>% summarize(dollar_spend = sum(dollar_spend)) %>% distinct(page_id, dollar_spend)) %>% 
  group_by(page_category) %>% 
  summarize(dollar_spend = sum(dollar_spend)) %>% 
  ungroup() %>% 
  mutate(perc = dollar_spend/sum(dollar_spend)*100) %>%
  arrange(desc(perc)) %>% 
  View()



ggdat <- page_info_dat %>% 
  bind_rows(page_info_dat2) %>% 
  bind_rows(page_info_dat3) %>% 
  as_tibble() %>% 
  left_join(elex %>% group_by(cntry, page_id) %>% summarize(dollar_spend = sum(dollar_spend)) %>% distinct(page_id, dollar_spend)) %>% 
  group_by(page_category, cntry) %>% 
  summarize(dollar_spend = sum(dollar_spend),
            ntimes = n()) %>% 
  ungroup() %>% 
  group_by(cntry) %>% 
  mutate(perc = dollar_spend/sum(dollar_spend)*100) %>%
  arrange(desc(perc)) %>%
  filter(ntimes >= 5) %>% #View()
  slice(1:5) %>% 
  ungroup() %>% 
  mutate(page_category = fct_reorder(page_category, perc)) %>% 
  ggplot(aes(page_category, perc)) +
  geom_boxplot() +
  coord_flip()

ggsave("plot.png",ggdat, width = 8, height = 6, bg = "white", dpi = 300)
  # View()


beginn_month <- c("2024-06-01", "2024-05-01", "2024-04-01")
full_list <- expand_grid(beginn_month, iso2 = full_cntry_list$iso2c)



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
        mutate(dollar_spend = total_spend_formatted / conversion_rate)
    })
    
    return(elex)
  })
