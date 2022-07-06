#' A Function To Create Data Labels
#'
#' This function creates a skeleton file to be compiled with Latex
#'
#' @param data.ss.for.label Should contain datumEntity, record.date, site,
#'   grid.ref,recorder.name, method, Code.
#' @param file.for.output Name of file to save. Do not add the extension.
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
  square.tdf.path <- system.file("latex_template","square.tdf",package = "eco.atlas")
  square.tdf <- readr::read_file(data.model.path) %>%   as.character()

  # Save the tdf file as latex needs it
  readr::write_file(square.tdf,  "square.tdf")


  my.rendered.output <-
    whisker::whisker.render(my.template, data = my.lists, strict = FALSE)

  # The multi line list inserts a comma for each row which needs to be removed.
  my.rendered.output.cleaned <-  gsub("\n,","\n", my.rendered.output )

  rendered.path <- fs::path(file.for.output , ext = "tex")

  readr::write_file(my.rendered.output.cleaned, rendered.path)

#  cat(formatted.for.tex, file = file.for.output)

}
