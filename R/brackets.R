#' Brackets
#'
#'Returns a consistent layout for authority without changing base data.
#' Deprecated:
#' Now does not change anything as the
#'  position of the brackets encode species status.
#'
#' @param authority.in authority as returned by UKSI
#'
#' @return text with brackets
#'
#'
#' @examples
#'
#' "Davies, 1979"
#'
#'
brackets <- function(authority.in) {

  authority.in <- gsub('\\(','',authority.in )
  authority.in <- gsub('\\)','',authority.in )
  authority.out <- paste0("(", authority.in, ")")


  return(authority.in)
}
