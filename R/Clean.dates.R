#' Clean.Dates
#'
#' Converts dates in inconsistent formats into a consistent format.
#' It now uses string functions.
#' @param x A vector of dates
#'
#' @return A multicolumn vector of the same length as x with the following values:
#'
#' "YYMMDD", "week", "YYYY", "DD", "MM", "MMDD", "DOY"
#'
#' @export Clean.dates
#'
#'
#'
#'
Clean.dates <- function(x) {
  f.data <- as.data.frame(x)
  # Add a column name
  colnames(f.data) <- c("Date") %>% as.character()
  # Rather than use subfunctions we use a series of custom regex to check for
  # basic date syntax. Once the rows containing valid dataes are found they are
  # converted to dates The resulting output is the same length as the input
  # vector so the user has a choice on how to handle the invalid date rows.

  # Deal with date ranges of the form "August 2012 to September 2012". Assume all
  # dates of this type start on the first of the month.

  # First find which rows have "to".
  # This works better if we look for spaces in front of the year.
  f.data$isadate <- grepl(" \\d{4}",f.data$Date )
 # f.data$isadate <- grepl(" to ",f.data$Date )
  which.rows <- which(f.data$isadate)

  # we now need to pull out the month and year.
  date.words <- stringr::str_extract_all(f.data[which.rows,"Date"],
                                         stringr::boundary("word")) %>% unlist()

  my.month <- date.words[1] # get the month

  my.year <- date.words[2] # get the year

  my_months_index <- match(my.month,month.name) # as a number
  # str_pad(string, 2, side = "left", pad = "0")
  my_months_number <- stringr::str_pad(my_months_index, 2, side = "left", pad = "0")

  # Make into a date
  my.new.date <- paste(my.year,my_months_number,"01", sep = "-" )

  # now paste into f.data
  f.data[which.rows,"Date"] <- my.new.date

  # all should now be good from here.

  # Test for dates dd-mm-yyyy
  f.data$isadate <- grepl("\\d{2}-\\d{2}-\\d{4}",f.data$Date )
  which.rows <- which(f.data$isadate)
  # We now know which dates are true and which are false.
  f.data[which.rows,"date_format_YYYYMMDD"] <-
    strftime(as.POSIXlt(as.Date(f.data[which.rows,"Date"], origin="1904-01-01", format = "%d-%m-%Y")),
             format="%Y-%m-%d")
  # We need to do this for each date pattern in the data.

  # Test for dates dd/mm/yyyy
  f.data$isadate <- grepl("\\d{2}/\\d{2}/\\d{4}",f.data$Date )
  which.rows <- which(f.data$isadate)
  # We now know which dates are true and which are false.
  f.data[which.rows,"date_format_YYYYMMDD"] <-
    strftime(as.POSIXlt(as.Date(f.data[which.rows,"Date"], origin="1904-01-01", format = "%d/%m/%Y")),
             format="%Y-%m-%d")

  # Test for dates yyyy-mm-dd
  f.data$isadate <- grepl("\\d{4}-\\d{2}-\\d{2}",f.data$Date )
  which.rows <- which(f.data$isadate)
  # We now know which dates are true and which are false.
  f.data[which.rows,"date_format_YYYYMMDD"] <-
    strftime(as.POSIXlt(as.Date(f.data[which.rows,"Date"], origin="1904-01-01", format = "%Y-%m-%d")),
             format="%Y-%m-%d")

  # Test for dates yyyy/mm/dd
  f.data$isadate <- grepl("\\d{4}/\\d{2}/\\d{2}",f.data$Date )
  which.rows <- which(f.data$isadate)
  # We now know which dates are true and which are false.
  f.data[which.rows,"date_format_YYYYMMDD"] <-
    strftime(as.POSIXlt(as.Date(f.data[which.rows,"Date"], origin="1904-01-01", format = "%Y/%m/%d")),
             format="%Y-%m-%d")

  # Test for Excel dates
  f.data$isadate <- grepl("\\d{5}",f.data$Date )
  which.rows <- which(f.data$isadate)
  # We now know which dates are true and which are false.


  f.data[which.rows,"date_format_YYYYMMDD"] <-
    strftime(as.POSIXlt(as.Date(as.numeric(as.character(f.data[which.rows,"Date"])), origin = "1899-12-30",format = "%Y-%m-%d" )),
             format="%Y-%m-%d")

  # # Deal with date ranges of the form "August 2012 to September 2012". Assume all
  # # dates of this type start on the first of the month.
  #
  # # First find which rows have "to"
  # f.data$isadate <- grepl(" to ",f.data$Date )
  # which.rows <- which(f.data$isadate)
  #
  # # we now need to pull out the month and year.
  # date.words <- stringr::str_extract_all(f.data[which.rows,"Date"], stringr::boundary("word"))
  #


  # Now try to transform all the dates and break out the year, month and week numbers
  #f.data$YYMMDD <-format(as.Date(x, origin="1904-01-01", format = "%d/%m/%Y"), "%Y/%m/%d")
  f.data$YYMMDD <- f.data$date_format_YYYYMMDD
  f.data$week <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%W")
  f.data$YYYY <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%Y")
  f.data$DD <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%d")
  f.data$MM <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%m")
  f.data$MMDD <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%m%d")
  f.data$DOY  <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%j")
  # Remove remaining NA dates by testing where we can work out a year

  #f.data <- subset(f.data, !is.na(f.data$YYYY)) %>% as.data.frame

  # And finally drop the columns we do not need by selecting those that are useful
  #f.data <- subset(f.data, select=c("Date","YYMMDD","week","YYYY","DD","MM", "MMDD","DOY"))
  f.data <- f.data[,c("YYMMDD","week","YYYY","DD","MM", "MMDD","DOY")]

}

