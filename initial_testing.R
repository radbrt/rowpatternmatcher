# NOTE:
# THIS IS NOT UNIT TESTS - those haven't arrived yet...

msft <- read.csv('raw_data/MSFT.csv')
msft$ticker <- "MSFT"
amzn <- read.csv('raw_data/AMZN.csv')
amzn$ticker <- "AMZN"
aapl <- read.csv('raw_data/AAPL.csv')
aapl$ticker <- "AAPL"
stocks <- dplyr::bind_rows(msft, amzn, aapl)

tail(stocks)
names(stocks)
names(stocks) <- c("date", "open", "high", "low", "close", "adj_close", "volume", "ticker")

save(stocks, file = "data/stocks.RData")
rm(stocks)
load('data/stocks.RData')


library(tidyverse)


stocks %>% 
  filter(ticker=='MSFT') %>% 
  arrange(date) %>% 
  mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
  mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
  regex_row_matcher('([D]{4,})', ds) %>% 
  nrow()




# Eneste måten å forbedre må være en mutate+case_when som har omtrent signaturen under
# define_rows(mdf, nv, e+s>10 ~ 'whot', e+s>3 ~ 'nah')

# WILDCARD POC
stocks %>% 
  filter(ticker=='MSFT') %>% 
  arrange(date) %>% 
  mutate( defns = 
            case_when(
              adj_close>lag(adj_close) ~ 'UP',
              TRUE ~ 'DOWN'
            )
  ) %>% 
  match_rows(defns, " whatevz UP{4,}") %>% 
  nrow()





