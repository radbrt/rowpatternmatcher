#' Regex row matcher
#'
#' This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' @param df Sorted and grouped dataframe to filter
#' @param definitions Column containing the definition of rows
#' @param rx Perl-regex pattern to look for, based on the definitions created
#' @param match_name Optional column name for match-number. Defaults to NULL, meaning no column will be created.
#' @param keep_all_rows Boolean allows you to return all rows, uncluding nonmatching rows. 
#' Meaninless if match_name is not set. Default FALSE.
#' @keywords match_recognize
#' @export
#' @examples
#' \dontrun{
#' match_rows_raw(msft, change2, "([D]{4,})", mnum)
#' }
match_rows_raw <- function(df, definitions, rx, match_name=NULL, keep_all_rows=FALSE) {
  definitions <- rlang::enquo(definitions)
  match_name <- rlang::enquo(match_name)
  dplyr::group_modify(df, ~ match_partition_raw(df=.x, definitions, rx, match_name, keep_all_rows)) 
}



#' Conditional aggregation
#'
#' Aggregate a column where some condition is true - meant to be used inside tidy functions. 
#' This is not even a convenience function as it can be done simpler in native tidyverse (se examples).
#' The function exists in order to highlight a programming pattern.
#' @param fun The aggregation function to run
#' @param var The variable to run the function on
#' @param cond The condition to filter on
#' @keywords aggregation
#' @export
#' @examples
#' \dontrun{
#' mutate(msft, fincol = aggregate_where(mean, adj_close, change=="UP") )
#' 
#' # Equivalent to
#' mutate(msft, fincol = mean(adj_close[change=="UP"] ))
#' }
#' 
aggregate_where <- function(fun, var, cond) {
  fun(var[cond])
}



#' Row matcher
#'
#' This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' @param df Sorted and grouped dataframe to filter
#' @param definitions Column containing the definition of rows
#' @param rx Simple regex-like statement to filter for - quoted.
#' @param match_name Optional column name for match-number. Defaults to NULL, meaning no column will be created.
#' @param wildcards keywords (nicknames) in your pattern (`rx`) that should match any rows
#' @param keep_all_rows Boolean allows you to return all rows, uncluding nonmatching rows. 
#' Meaninless if match_name is not set. Default FALSE.
#' @keywords match_recognize
#' @export
#' @examples
#' \dontrun{
#' match_rows(msft, change, "UP{4,} DOWN{3,}", match_name=mnum)
#' }
match_rows <- function(df, definitions, rx, match_name=NULL, wildcards=NULL, keep_all_rows=FALSE) {
  definitions <- rlang::enquo(definitions)
  match_name <- rlang::enquo(match_name)
  
  #rxt <- "^Kw4   UP+    DOWN{3,}$"
  # Extract nicknames/definitions from pattern
  rx_name_ptn <- "[A-Za-z0-9]+(?![^{]*\\})"
  ptn_nicks <- as.character(stringr::str_match_all(rx, rx_name_ptn)[[1]])
  col_nicks <- as.character(dplyr::pull(df, !!definitions))
  all_nicks <- unique(c(col_nicks, ptn_nicks))
  all_nicks <- setdiff(all_nicks, wildcards)
  all_nicks <- rev(all_nicks[order(nchar(all_nicks), all_nicks)])
  #all_nicks <- setdiff(all_nicks, wildcards)
  if(length(all_nicks)>9) stop("There are more than 10 different definitions, we are not able to handle that yet...")
  
  #rx_names <- stringr::str_extract_all(rx, rx_name_ptn)[[1]]
  rx_parsed <- rx

  if (length(wildcards)>0) {
    for (w in wildcards) {
      rx_parsed <- stringr::str_replace_all(rx_parsed, w, '\\.')
    }
  }
  
  # reverse-sorted all-nicks to avoid potential overwrite
  for (a in all_nicks) {
    rx_parsed <- stringr::str_replace_all(rx_parsed, a, as.character(match(a, all_nicks)))
  }
  
  # spaces are great when writing pseudoregex, need to remove before parsing as real regex
  rx_parsed <- stringr::str_replace_all(rx_parsed, ' ', '')

  # match_partition_fast <- function(df, definitions, rx, all_nicks, match_name=NULL, keep_all_rows=FALSE)
  dplyr::group_modify(df, ~ match_partition(df=.x, definitions, rx_parsed, all_nicks, match_name, keep_all_rows)) 
}










