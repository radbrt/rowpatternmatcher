#' Match ranges
#' DEPRECATED
#' This is a convenience function that recieves two integers (a, b), and returns the range a:b-1
#' @param start The start of the range
#' @param ends The end of the range, endexclusive
ranges_from_starts_lengths <- function(start, ends) {
  return(start:((ends)-1))
}

#' Ranges from pattern
#' returns row ranges e.g. c(1:4,13:17) based on string of definitions and regex-pattern 
#' @param defstring The collapsed column with definitions - one character per row
#' @param ptn The regex-pattern to find
row_ranges <- function(defstring, ptn) {
  result <- str_locate_all(defstring, ptn)[[1]]
  cstarts <- result[, 'start']
  cends <- result[, 'end']
  ranges <- map2(cstarts, cends, ~ .x:.y)
  ranges
}

#' Match number
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

#' Subset a dataframe based on list of possibly overlapping ranges of rows
#' @param df Data frame to subset
#' @param ranges List of possibly overlapping ranges of rows to select
subset_from_ranges <- function(df, ranges) {
  unique_row_numbers <- unique(unlist(ranges))
  df[unique_row_numbers,]
}
