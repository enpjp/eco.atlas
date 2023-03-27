#' Jpegs To Report Format
#'
#' Resize and rename jpeg files to jpg with 1000px height.
#'
#' @param jpg.path Path to directory with jpg files relative to project
#'   directory
#'
#' @return nothing to return
#' @export jpegs.to.report.format
#'
jpegs.to.report.format <- function(jpg.path = "_records"){

  # Read all jpeg files
  # Create a path to the book versions
  path.to.jpeg.data <- fs::path(jpg.path)

  # Find files to rename

  files.to.rename <- list.files(
    path.to.jpeg.data,
    pattern = ".JP?G$|.jpeg$", # Make a suitable filter.
    full.names = TRUE,
    recursive = TRUE)

  rename.files.to <- fs::path_ext_set(files.to.rename, ext = "jpg")

  file.rename(from = files.to.rename, to = rename.files.to )

  # Now read all the files and rename them syntactically

  my.jpeg.files <- list.files(
    path.to.jpeg.data,
    pattern = ".jp?g$", # Make a suitable filter.
    full.names = TRUE,
    recursive = TRUE)

  bad.name <- basename(my.jpeg.files)
  bad.name.no.ext         <-  fs::path_ext_remove(bad.name)
  nice.name <-  janitor::make_clean_names(bad.name.no.ext)

 path.name <- dirname(my.jpeg.files)

 nice.path.name <- fs::path( path.name, nice.name, ext = "jpg")



  file.rename(from = my.jpeg.files, to = nice.path.name )

# And read again to resize
  my.jpeg.files <- list.files(
    path.to.jpeg.data,
    pattern = ".jp?g$", # Make a suitable filter.
    full.names = TRUE,
    recursive = TRUE)

  my.jpeg.info  <-  magick::image_info( magick::image_read(my.jpeg.files)) %>% as.data.frame()

  jpeg.rows.to.change <- which(my.jpeg.info$height > 1000 )

  jpeg.files.to.change <- my.jpeg.files[jpeg.rows.to.change]

  imagemagick.files <- magick::image_read(jpeg.files.to.change)

  #imagemagick.files.resized <- image_resize(imagemagick.files, geometry_size_pixels(height = 1000))

  imagemagick.files.resized <- magick::image_resize(imagemagick.files, "x1000")

  # Image write does not behave correctly as a vector
  if(length(jpeg.files.to.change) > 0) {
       for (my.new.image in 1:length(jpeg.files.to.change)) {

         magick::image_write(
       imagemagick.files.resized[my.new.image],
        path = jpeg.files.to.change[my.new.image]
      )
      }

  }






}
