

#' new.record
#'
#'
#'@param index.reference A unique record identifier.
#'@param datum.attributes Named list of attributes to use in template
#'@param data.model.path Path to external template

#'
#' @return Nothing returned
#' @export new.record
#'
new.record <- function(index.reference,
                       datum.attributes,
                       data.model.path = NULL
                       ) {

  my.lists <- datum.attributes
  my.working.project <- rstudioapi::getActiveProject()

  my.working.directory <- fs::path(getwd())

  dir.create(index.reference)

  # Now create an images directory
  images.dir <- fs::path(my.working.directory,index.reference, "images")
  dir.create(images.dir)

  # Path to inst
  if(is.null(data.model.path)) {
    data.model.path <- system.file("model_template","data_model.Rmd",package = "eco.atlas")

  }

  # Now create the template file
 my.template <- readr::read_file(data.model.path) %>%   as.character()

 my.rendered.output <-
   whisker::whisker.render(my.template, data = my.lists, strict = FALSE)
 # Write the file
 rendered.path <- fs::path(my.working.directory,
                           index.reference,
                           index.reference, ext = "Rmd")
 readr::write_file(my.rendered.output, rendered.path)

  # usethis::with_project( my.working.project,
  #                        usethis::use_template(data_model,
  #                                              save_as = fs::path(my.working.directory,
  #                                                                 index.reference, ext = "Rmd"),
  #                                              index.reference = index.reference
  #                        )
  #                        # End use with project
  # )


}
