msft <- read.csv('MSFT.csv')
names(msft)
names(msft) <- c("date", "open", "high", "low", "close", "adj_close", "volume")
library(tidyverse)

ranges_from_starts_lengths <- function(start, ends) {
  return(start:((ends)-1))
}

regex_row_matcher <- function(df, ptn, defs) {

  defstring <- paste(defs, collapse='')
  result <- gregexpr(ptn, defstring, perl=TRUE)
  cstarts <- attr(result[[1]], 'capture.start')
  cends <- attr(result[[1]], 'capture.start') + attr(result[[1]], 'capture.length')
  ranges <- map2(cstarts, cends, ranges_from_starts_lengths)
  arranges <- unique(unlist(ranges))
  return(df[arranges,])
}

msft %>% 
  arrange(date) %>% 
  mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
  mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
  regex_row_matcher('([D]{4,}[U]{2,})', .$ds) %>% 
  head(n=20)






