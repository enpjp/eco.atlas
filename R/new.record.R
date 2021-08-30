

#' new.record
#'
#'
#'@param index.reference A unique record identifier.
#'@param Order Record order.
#'
#'
#' @return Nothing returned
#' @export new.record
#'
new.record <- function(index.reference, Order = "Lepidoptera") {

  my.lists <- list(
    index.reference = index.reference,
    order = Order

    )

  my.working.project <- rstudioapi::getActiveProject()

  my.working.directory <- fs::path(getwd())

  dir.create(index.reference)

  # Now create an images directory
  images.dir <- fs::path(my.working.directory,index.reference, "images")
  dir.create(index.reference)

  # Path to inst
 data.model.path <- system.file("model_template","data_model.Rmd",package = "eco.atlas")
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
