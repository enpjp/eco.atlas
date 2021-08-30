

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
new.record <- function(index.reference,Order = "Lepidoptera") {

  my.working.project <- rstudioapi::getActiveProject()

  my.working.directory <- fs::path(getwd())

  dir.create(index.reference)
  # Now create an images directory
  images.dir <- fs::path(my.working.directory,"images")

  # Path to inst
 data_model <- system.file("model_template","data_model.Rmd",package = "eco.atlas")
  # Now create the template file
  usethis::with_project( my.working.project,
                         usethis::use_template(data_model,
                                               save_as = fs::path(my.working.directory,
                                                                  index.reference, ext = "Rmd"),
                                               reference = index.reference
                         )
                         # End use with project
  )


}
