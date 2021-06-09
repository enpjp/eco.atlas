
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
#where.am.i <- rstudioapi::getSourceEditorContext()
# Find the base directory
directory.base.name <- basename(current.pane)
current.chapter.Rmd <- fs::path(directory.base.name, ext = "Rmd")
output.pdf <- fs::path(directory.base.name, ext = "pdf")


setwd(current.pane)
bookdown::preview_chapter(current.chapter.Rmd,output_file = output.pdf)
# Now compact the pdf
tools::compactPDF(output.pdf,qpdf = "qpdf", gs_cmd = "gs", gs_quality = "ebook")
setwd(rprojroot::find_rstudio_root_file())
}
