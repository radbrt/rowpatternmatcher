

regex_pattern <- function(df, ptn, defs) {
  # returns rows that match pattern:
  #   -does: selects row in df (usually group_by partition) that matches pattern
  #   -needs: df, pattern (string with regex pattern), colname of df tht containts pattern (turn into string)
  defstring <- paste(df$defs, collapse='')
  result <- gregexpr(ptn, defstring, perl=TRUE)
  
  rmat <- attr(result[[1]], 'capture.start')
  ranges <- make_ranges(rmat)
  return(1:3)
}


make_ranges <- function(mat) {
  # TODO: assert ncol(mat)==2
  ranges = c()
  for(i in 1:nrow(mat)) {
    print(mat[i,])
    ranges <- c(ranges, mat[i,][1]:mat[i,][2])
  }
  return(ranges)
}