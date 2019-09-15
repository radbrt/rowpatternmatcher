#' Regex row matcher
#'
#' This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' @param df Sorted and grouped dataframe to filter
#' @param ptn Perl-regex pattern to look for, based on the definitions created
#' @param defs Column containing the definition of rows
#' @param match_name Optional column name for match-number, defaults to NULL.
#' @keywords match_recognize
#' @export
#' @examples
#' match_rows_raw(msft, change2, "([D]{4,})", mnum)
match_rows_raw <- function(df, definitions, rx, match_name=NULL) {
  definitions <- enquo(definitions)
  match_name <- enquo(match_name)
  group_modify(df, ~ match_partition_raw(df=.x, definitions, rx, match_name)) 
}


#' Row matcher
#'
#' This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' @param df Sorted and grouped dataframe to filter
#' @param definitions Column containing the definition of rows
#' @param rx Simple regex-like statement to filter for - quoted.
#' @param match_name Optional column name for match-number, defaults to NULL.
#' @keywords match_recognize
#' @export
#' @examples
#' match_rows(msft, change, "UP{4,} DOWN{3,}", match_name=mnum)
match_rows <- function(df, definitions, rx, match_name=NULL) {
  definitions <- enquo(definitions)
  match_name <- enquo(match_name)
  group_modify(df, ~ match_partition(df=.x, definitions, rx, match_name)) 
}




