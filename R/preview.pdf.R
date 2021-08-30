
#' Preview.pdf
#'
#' Generates a preview of the currently active bookdown chapter and compresses
#' the PDF file.
#'
#'
#'
#' @return Nothing returned
#' @export preview.pdf
#'
#'
preview.pdf <- function() {
# Preview
# Current source active file directory
current.pane <- dirname(rstudioapi::getSourceEditorContext()$path)
#current.pane <- getwd()
setwd(current.pane)
#where.am.i <- rstudioapi::getSourceEditorContext()
# Find the base directory
directory.base.name <- basename(current.pane)
current.chapter.Rmd <- fs::path(directory.base.name, ext = "Rmd")

# Make sure that jpegs are the correct size.

my.jpeg.files <- list.files(
  current.pane,
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


# Build the PDF
output.pdf <- fs::path(directory.base.name, ext = "pdf")
bookdown::preview_chapter(current.chapter.Rmd,output_file = output.pdf)
# Now compact the pdf
tools::compactPDF(output.pdf,qpdf = "qpdf", gs_cmd = "gs", gs_quality = "printer")
setwd(rprojroot::find_rstudio_root_file())
}
