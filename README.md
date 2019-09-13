# Row pattern matching in R

Welcome, and sorry for the derangement.

## What is this?
This R package does row pattern matching with a tidyverse-compliant syntax. What is row pattern matching, you ask? The concept was first developed (as far as I know) by Oracle, and was included in the ANSI SQL:2016 standard. But so far very few databases support it - despite it being one of the most creative new features in a while. The basic idea is to let you filter a dataset not based conditions that can be satisfied by a single row alone, but by the pattern that the rows constitute.

## When is it useful?
The canonical example is stock prices: 
- Can you select all rows that are part of a 5-or-more day price surge? 
- Can you select the rows that are part of a prolonged W-pattern of repeating ups and downs?

For social scientists dealing with panel data, which follows interview subjects year after year, this lets you easily find things like repeated job changes, periods of strong income increases, or specific educational paths.

The package is not trying to copy the ANSI row pattern matching syntax, rather it is trying to copy the functionality and adapt it to the R/tidy programming patterns. Also, the implementation is fairly opportunistic. It leaves out functionality that is covered by tidyverse libraries, it includes some new features that came "for free" in the R environment, and it postpones implementing features that have a high cost/benefit ratio. All in all, it is likely that the library will never cover all features of row pattern matching in SQL.

## How do I use it?

Basically working with row pattern matching comes in two steps:
1. First you create a set of **definitions** for your rows. This can be stuff like, the close price of a stock being higher than in the previous row, or name of employer is different from the next row. Or maybe as simple as stock price over 100. For the time being, you can create up to 10 of these definition (for weird technical reasons). Definitions are basically just a new column, containing "nicknames" for your definitions. In the example of stock prices, nicknames can be things like *UP* or *DOWN*. These names are completely up to you, but should be intuitive. 
1. The definition column you created in the previous step is in turn the basis for the pattern you will look for. Defining a pattern can be done using pseudo-regex (or real regex, for advanced users), and will return only the rows that are part of the pattern you are looking for.

## Show me the code already!

OK, calm down. A basic example, using the stocks data that come with the package, can look like this:

```
stocks %>% 
  filter(ticker=='MSFT') %>% 
  arrange(date) %>% 
  mutate( defns = 
           case_when(
             adj_close>lag(adj_close) ~ 'UP',
             TRUE ~ 'DOWN'
           )
  ) %>% 
  match_rows(defns, "UP{4,} DOWN{3,}")
```

This looks for occurences of the Microsoft stock increasing at least four consecutive days, and then decreasing at least three consecutive days. Only the rows that are part of this "peak" will be included in the output. The resulting data frame will have 14 rows that constitute two different matches. A separate column, `match_number` denoting the match occurence is automatically added. In future releases the variable name can be specified by the user.

There is also an "advanced mode", which does not do any regex parsing and therefore allows for more advanced regex patterns to be specified. The downside of this though, is that the definitions created can only consist of a single character. This really shows that what happens under the hood is pure regex - and one of the simplifying tricks is to have a 1-to-1 mapping between the text string containing the definitions and the rows. Sorry for the inconvenience, this is duct-tape not magic.

```
stocks %>% 
               filter(ticker=='MSFT') %>% 
               arrange(date) %>% 
               mutate(ds = ifelse(adj_close>lag(adj_close), 'U', 'D')) %>% 
               mutate(ds = ifelse(is.na(ds), '0', ds)) %>% 
               regex_row_matcher('([D]{4,})', ds) 
```

## My interest is piqued, where can I learn more?

If you want to learn more about row pattern matching as implemented by Oracle, you can take a look at their documentation at https://docs.oracle.com/database/121/DWHSG/pattern.htm#DWHSG8956.
