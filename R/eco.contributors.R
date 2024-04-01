# Get list of contributors

#' Contributors To Report
#'
#' @param my.data.ss Search data for names of contributors
#'
#' @return List of contributors and clipboard text.
#' @export eco.contributors
#'@importFrom magrittr "%>%"
#'
eco.contributors <- function(my.data.ss) {

  thanks.stack <- utils::stack( my.data.ss[,c("recorder.name","determiner")]) %>% unique()

  thanks.stack$values <- gsub("NULL",NA,thanks.stack$values)

  thanks.expanded <- thanks.stack %>% tidyr::separate_longer_delim( cols = "values",
                                                                    delim = stringr::regex("&|;|,"))

  thanks.expanded$values <- thanks.expanded$values %>% stringr::str_trim()

  thanks <-  thanks.expanded %>% unique()  %>% tidyr::drop_na()

  thanks.recorders <- thanks %>% filter(thanks$ind == "recorder.name")
  thanks.determiners <- thanks %>% filter(thanks$ind == "determiner")

  # Now make a thanks string
  recorder.text <- knitr::combine_words(thanks.recorders$values)
  determiner.text <-  knitr::combine_words(thanks.determiners$values)

  thanks.text <- paste("Thanks to ",recorder.text,
                       " for providing specimens. The assistance with determinations by ",
                       determiner.text,
                       " is gratefully acknowledged.",
                       sep = ""
  )
  clipr::write_clip(thanks.text)

  return(thanks)
}
