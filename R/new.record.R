

#'new.record
#'
#'
#'@param datum.attributes Named list of attributes to use in template. must
#'  begin with index.reference a unique record identifier.
#'@param data.model.path Path to external template
#'@param overwrite Should existing data be overwritten?
#'@param record.path Path to save records
#'@param sub.dir Group records into subdirectory
#'
#' @return Nothing returned
#' @export new.record
#'
new.record <- function(
                       datum.attributes,
                       data.model.path = NULL,
                       overwrite = FALSE,
                       record.path,
                       sub.dir = NULL
                       ) {
  index.reference <- datum.attributes$index.reference
  my.lists <- datum.attributes
 # my.working.project <- rstudioapi::getActiveProject()

  my.working.directory <- record.path

  if(!is.null(sub.dir)){

    my.working.directory <- fs::path(my.working.directory, sub.dir)
  }

  # Only overwrite if flag true

   does.dir.exist <- fs::dir_exists(
     fs::path(my.working.directory , index.reference))

   if(does.dir.exist){
      if(overwrite){
        create.the.directory <- TRUE
      }else{create.the.directory <- FALSE}

   }else{

     create.the.directory <- TRUE
   }

   if(create.the.directory) {




          dir.create(fs::path(my.working.directory , index.reference))

          # Now create an images directory
          images.dir <- fs::path(my.working.directory,
                                 index.reference,
                                 "images",
                                 "dissection")
          fs::dir_create(images.dir, recurse = TRUE)
          # create a placeholder for git
          readr::write_file( "Images here." ,
                             fs::path( images.dir,
                                       "image_placeholder",
                                       ext = "txt"

            ) )


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


   if(overwrite) {




     fs::dir_create(fs::path(my.working.directory, index.reference))

     # Now create an images directory
     images.dir <- fs::path(my.working.directory,
                            index.reference,
                            "images",
                            "dissection")
     fs::dir_create(images.dir, recurse = TRUE)
     # create a placeholder for git
     readr::write_file( "Images here." ,
                        fs::path( images.dir,
                                  "image_placeholder",
                                  ext = "txt"

                        ) )


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


   # Now copy any resource files
   resource.path <- dirname(data.model.path)
   resource.dir <- fs::path(resource.path, "resources")
   destination.path <- fs::path(my.working.directory,
                                index.reference,
                                "resources")
   if (dir.exists(resource.dir)) {
     fs::dir_create(destination.path, recurse = TRUE)
     fs::dir_copy(resource.dir,
                  destination.path, overwrite = TRUE)
   }



}
