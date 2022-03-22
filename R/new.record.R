

#'new.record
#'
#'
#'@param datum.attributes Named list of attributes to use in template. must
#'  begin with index.reference a unique record identifier.
#'@param data.model.path Path to external template
#'@param overwrite Should existing data be overwritten?
#'
#' @return Nothing returned
#' @export new.record
#'
new.record <- function(
                       datum.attributes,
                       data.model.path = NULL,
                       overwrite = FALSE
                       ) {
  index.reference <- datum.attributes$index.reference
  my.lists <- datum.attributes
  my.working.project <- rstudioapi::getActiveProject()

  my.working.directory <- fs::path(getwd())

  # Only overwrite if flag true

   does.dir.exist <- fs::dir_exists(index.reference)

   if(does.dir.exist){
      if(overwrite){
        create.the.directory <- TRUE
      }else{create.the.directory <- FALSE}

   }else{

     create.the.directory <- TRUE
   }

   if(create.the.directory) {




          dir.create(index.reference)

          # Now create an images directory
          images.dir <- fs::path(my.working.directory,index.reference, "images","dissection")
          dir.create(images.dir, recursive = TRUE)



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
   }
  # usethis::with_project( my.working.project,
  #                        usethis::use_template(data_model,
  #                                              save_as = fs::path(my.working.directory,
  #                                                                 index.reference, ext = "Rmd"),
  #                                              index.reference = index.reference
  #                        )
  #                        # End use with project
  # )


}
