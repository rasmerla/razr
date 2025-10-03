
#' Title
#'
#' @param strings Strings to compare
#' @param max_dist How many characters that should minimum diff.
#' @param ignore_case Defaults to True.
#' @param costs Complicated ...

find_similar_strings <- function(
    strings,
    max_dist = 3,
    ignore_case = TRUE,
    costs = c(ins = 1, del = 1, sub = 1)
) {
  # strings: character vector of names / strings
  # max_dist: the maximum edit distance (number of insertions/deletions/substitutions) to consider “similar”
  # ignore_case: whether to lower-case before comparing
  # costs: list / named vector for insertion, deletion, substitution costs (passed to adist)

  if (ignore_case) {
    s_norm <- tolower(strings)
  } else {
    s_norm <- strings
  }

  # Compute full distance matrix
  dmat <- utils::adist(s_norm, s_norm,
                ignore.case = FALSE,
                costs = costs)

  n <- length(strings)
  result <- list()

  # For each string i, find strings j > i (to avoid duplicates) that are within max_dist
  for (i in seq_len(n)) {
    # Compare only j > i to avoid symmetric duplicates and self-distance 0
    for (j in seq((i+1), n)) {
      if (j > n) next
      d <- dmat[i, j]
      if (!is.na(d) && d <= max_dist) {
        result[[ length(result) + 1 ]] <- list(
          i = i, j = j,
          str_i = strings[i], str_j = strings[j],
          dist = d
        )
      }
    }
  }

  # Convert to data frame
  if (length(result) == 0) {
    return(data.frame(i = integer(0),
                      j = integer(0),
                      str_i = character(0),
                      str_j = character(0),
                      dist = numeric(0),
                      stringsAsFactors = FALSE))
  } else {
    df <- do.call(rbind, lapply(result, as.data.frame, stringsAsFactors = FALSE))
    return(df)
  }
}
