# NOTE:
# THIS IS NOT UNIT TESTS - those haven't arrived yet...

msft <- read.csv('MSFT.csv')
names(msft)
names(msft) <- c("date", "open", "high", "low", "close", "adj_close", "volume")
library(tidyverse)


msft %>% 
  arrange(date) %>% 
  mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
  mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
  regex_row_matcher('([D]{4,})', ds) %>% 
  head(n=20)





# Eneste måten å forbedre må være en mutate+case_when som har omtrent signaturen under
# define_rows(mdf, nv, e+s>10 ~ 'whot', e+s>3 ~ 'nah')

msft %>% 
  arrange(date) %>% 
  mutate( defns = 
            case_when(
              adj_close>lag(adj_close) ~ 'UP',
              TRUE ~ 'DOWN'
            )
  ) %>% 
  match_rows(defns, "UP{4,}DOWN{1,}") %>% 
  head(n=20)
