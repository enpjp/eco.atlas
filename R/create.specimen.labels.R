#' A Function To Create Data Labels
#'
#' This function creates a skeleton file to be compiled with Latex
#'
#' @param data.ss.for.label Should contain datumEntity
#' @param file.for.output Name of file to save
#' @export create.specimen.labels
#'

create.specimen.labels <- function(data.ss.for.label, file.for.output = "label"){

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

  # Make as a one item list

  my.lists <- list( cards =  formatted.for.tex  )

# Now to create a Latex file

  data.model.path <- system.file("latex_template","specimen_label.Rmd",package = "eco.atlas")

  # Now create the template file
  my.template <- readr::read_file(data.model.path) %>%   as.character()

  my.rendered.output <-
    whisker::whisker.render(my.template, data = my.lists, strict = FALSE)

  rendered.path <- fs::path(file.for.output , ext = "tex")

  readr::write_file(my.rendered.output, rendered.path)

#  cat(formatted.for.tex, file = file.for.output)

}
