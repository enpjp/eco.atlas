#' A Function To Create Data Labels
#'
#' This function creates a skeleton file to be compiled with Latex
#'
#' @param data.ss.for.label Should contain datumEntity, record.date, site,
#'   grid.ref,recorder.name, method, Code.
#' @param file.for.output Name of file to save. Do not add the extension.
#' @param make.pdf Automatically build the PDF file.
#' @param small.label Logical: make small labels.
#' @param last.line.comment Logical: Use the comments field for the last line.
#' @export create.specimen.labels
#'
#'

create.specimen.labels <- function(data.ss.for.label,
                                   file.for.output = "label",
                                   make.pdf = TRUE,
                                   small.label = FALSE,
                                   last.line.comment = FALSE){

  # Sometimes "Code" is NA as not ID but an aggregate is recorded in taxon
  empty.codes <- is.na(
    suppressWarnings(as.numeric(data.ss.for.label$Code)
                     )
                    )

    data.ss.for.label$Code[empty.codes] <-
      data.ss.for.label$taxon[empty.codes]

if(last.line.comment){
   comments <- stringr::str_replace_all(data.ss.for.label$comments, "[^[:alpha:]]", " ")
   comments.squish <- stringr::str_squish(comments)
  data.ss.for.label$Code <- stringr::word(comments.squish, start = 1,end =2)
}


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
  if(small.label){
    data.model.path <- system.file("latex_template","small_specimen_label.Rmd",
                                   package = "eco.atlas")
  }else{
    data.model.path <- system.file("latex_template","specimen_label.Rmd",
                                   package = "eco.atlas")
  }


  # Now create the template file

  my.template <- readr::read_file(data.model.path) %>%   as.character()

  # Get the latex definition file
  if(small.label){
    square.tdf.path <- system.file("latex_template","small.square.tdf",
                                   package = "eco.atlas")
    square.tdf <- readr::read_file(square.tdf.path) %>%   as.character()
    # Save the tdf file as latex needs it to set the label size
    readr::write_file(square.tdf,  "smallsquare.tdf")


  }else{
    square.tdf.path <- system.file("latex_template","square.tdf",
                                   package = "eco.atlas")
    square.tdf <- readr::read_file(square.tdf.path) %>%   as.character()
    # Save the tdf file as latex needs it to set the label size
    readr::write_file(square.tdf,  "square.tdf")

  }




  my.rendered.output <-
    whisker::whisker.render(my.template, data = my.lists, strict = FALSE)

  # The multi line list inserts a comma for each row which needs to be removed.
  my.rendered.output.cleaned <-  gsub("\n,","\n", my.rendered.output )

  rendered.path <- fs::path(file.for.output , ext = "tex")

  readr::write_file(my.rendered.output.cleaned, rendered.path)

  # Finally run latex to build the PDF if option TRUE

  tools::texi2dvi(rendered.path, pdf = TRUE, clean = TRUE)

#  cat(formatted.for.tex, file = file.for.output)

}
