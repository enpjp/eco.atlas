

#' new.record
#'
#'
#'@param index.reference A unique record identifier.
#'@param record.date ISO Date.
#'@param place.name Place name.
#'@param grid.ref Grid ref.
#'@param recorder.name Recorder Name.
#'@param record.notes Record notes.
#'@param determiner The determiner.
#'@param order Taxon order.
#'
#' @return Nothing returned
#' @export new.record
#'
new.record <- function(index.reference,
                       record.date = "Date",
                       place.name = "Place Name",
                       grid.ref = "Grid Reference",
                       recorder.name = "Recorder Name",
                       record.notes = "Record Notes",
                       determiner = "Determiner Name",
                       order = "Lepidoptera"
                       ) {

  my.lists <- list(
    index.reference = index.reference,
    record.date = record.date,
    place.name = place.name,
    grid.ref = grid.ref,
    recorder.name = recorder.name,
    record.notes = record.notes,
    determiner = determiner,
    order = order

    )

  my.working.project <- rstudioapi::getActiveProject()

  my.working.directory <- fs::path(getwd())

  dir.create(index.reference)

  # Now create an images directory
  images.dir <- fs::path(my.working.directory,index.reference, "images")
  dir.create(images.dir)

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
