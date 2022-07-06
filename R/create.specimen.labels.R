#' A Function To Create Data Labels
#'
#' This function creates a skeleton file to be compiled with Latex
#'
#' @param data.ss.for.label Should contain datumEntity
#' @param file.for.output Name of file to save
#' @export create.specimen.labels
#'

create.specimen.labels <- function(data.ss.for.label, file.for.output = "label.tex"){

  label.data.raw <- data.ss.for.label

  cols.to.keep <-  c("datumEntity",
                     "record.date",
                     "site",
                     "grid.ref",
                     "recorder.name",
                     "method",
                     "Code")

  data.ss.for.label.short <- label.data.raw[,cols.to.keep ]



  #data.ss.for.label.short$site <-  sub("(\\w+\\s+\\w+).*", "\\1", data.ss.for.label.short$site)
  data.ss.for.label.short$site <- stringr::word(data.ss.for.label.short$site, start = 1,end =2)

  formatted.for.tex <- with(data.ss.for.label.short,
                            paste(sep="", "\\acard",
                                  "{",datumEntity,"}",
                                  "{",record.date,"}",
                                  "{",site,"}",
                                  "{",grid.ref,"}",
                                  "{",recorder.name,"}",
                                  "{",method,"}",
                                  "{",Code,"}",
                                  "\n")
  )



  cat(formatted.for.tex, file = file.for.output)

}
