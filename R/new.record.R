

#' new.record
#'
#' @return Nothing returned
#' @export new.record
#'
new.record <- function(index.reference) {

  my.working.project <- rstudioapi::getActiveProject()

  my.working.directory <- fs::path(getwd())

  dir.create(index.reference)
  # Now create an images directory
  images.dir <- fs::path(my.working.directory,"images")

  # Now create the template file
  usethis::with_project( my.working.project,
                         usethis::use_template("rmarkdown-template.Rmd",
                                               save_as = fs::path(template.path.name,
                                                                  "skeleton", ext = "Rmd"),
                                               package = this.package.name
                         )
                         # End use with project
  )


}
