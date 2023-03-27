#' Compress PDF Files
#'
#' Compress PDF files built with markdown.
#'
#' @param project.path Usually the project directory.
#' @param is.recursive Recursively search for PDFs if TRUE.
#'
#' @return Nothing
#' @export compress.pdf.files
#'
#'
compress.pdf.files <- function(project.path, is.recursive = FALSE){

  my.pdf.report <- list.files(
    project.path,
    pattern = ".pdf$", # Make a suitable filter. Use the dot for a wildcard. note the use of the $ to match the files extension.
    full.names = TRUE,
    recursive = is.recursive)



  tools::compactPDF(my.pdf.report, qpdf = "qpdf", gs_cmd = "gs", gs_quality = "printer")



}
