#' Match ranges
#' DEPRECATED
#' This is a convenience function that recieves two integers (a, b), and returns the range a:b-1
#' @param start The start of the range
#' @param ends The end of the range, endexclusive
ranges_from_starts_lengths <- function(start, ends) {
  return(start:((ends)-1))
}

#' Match number
#'
#' This is an internal function that returns an array of match-numbers, in the case that 
#' there are several matches in one "string". 
#' @param matchlist Output of the mapped `ranges_from_starts_lengths` function
match_number <- function(matchlist) {
  cr <- c()
  for (i in 1:length(matchlist)) {
    cr <- c(cr, rep(i, length(matchlist[[i]])))
  }
  cr
}

#' Regex row matcher
#'
#' This function accepts a dataframe, regex-pattern and column name for definitions, and return matching rows
#' @param df Sorted and grouped dataframe to filter
#' @param ptn Perl-regex pattern to look for, based on the definitions created
#' @param defs Column containing the definition of rows
#' @keywords match_recognize
#' @export
#' @examples
#' regex_row_matcher(df, "([D]{4,})", .$ds)
regex_row_matcher <- function(df, ptn, defs) {
  edefs <- enquo(defs)
  defstring <- funs(paste(!!edefs, collapse=''))
  print(edefs)
  result <- gregexpr(ptn, defstring, perl=TRUE)
  cstarts <- attr(result[[1]], 'capture.start')
  cends <- attr(result[[1]], 'capture.start') + attr(result[[1]], 'capture.length')
  ranges <- map2(cstarts, cends, ~ .x:.y-1)
  print(ranges)
  match_numbers <- match_number(ranges)
  arranges <- unique(unlist(ranges))
  ret_df <- df[arranges,]
  ret_df$match_number <- match_numbers
  return(ret_df)
  
}






