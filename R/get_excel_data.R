#'Get Excel Data
#'
#'This function discovers Survey data in a directory (data-raw by default) and
#'processess it by extracting a consistent series of columns and placing them in
#'a data-clean directory. This function may be used in conjucntion with other
#'related functions to extract data from other sources.
#'
#'@param data_raw_directory Source of the raw data as a list of files.
#'@param columns_list A list of column mappings.
#'
#'@return A count of the number of files read. Output saved into data-clean
#'  directory.
#'
#'@export get_excel_data
#'
#'
#'
get_excel_data <- function(data_raw_directory="data-source", columns_list) {
  my.columns <- columns_list
  standard.col.names <- columns_list[[1]]
  temp.path <- data_raw_directory

  temp.list <- list.files(temp.path, pattern = "\\.xlsx$|.xls$",
                          full.names=TRUE, recursive = TRUE)
  # The trouble with Excel is that it creates temporary files that it leaves
  # behind, so they are not exactly temporary. It is hard to make a single
  # pattern to exclude these files so we will do it in two stages.

  temp.list.tilde <- which(!grepl("\\~", temp.list)) # Mark which rows to keep.
  temp.list <- temp.list[temp.list.tilde]


  temp.list.path <- temp.list

  # Mollusc_data <- read_excel("inst/dev/utils.template.network.inst.dev/data-raw/Mollusc data.xlsx")



  # If the number of excel files is greater than 0
  if(length(temp.list > 0)) {

    for(i in 1:length(temp.list)) {
      my.file <- temp.list.path[[i]]

      ### check for multiple sheets ####
      temp.excel.sheets <-
        readxl::excel_sheets(my.file )
      #colnames(temp.excel.sheets) <- c("sheet.name")
      temp.excel.sheets[] <- lapply(temp.excel.sheets, as.character)

      for(sheet.index in 1:length(temp.excel.sheets) ) {
        sheet.name <- temp.excel.sheets[[sheet.index]]
        # Read the files but set all columns to text
        temp.my.data.excel <- readxl::read_excel(my.file, col_types = "text", sheet = sheet.name) %>%
          as.data.frame()

        # Try and read clean dates
        my.clean.dates <- utils.template.network::Clean.dates(
          temp.my.data.excel$Date) %>%
          as.data.frame()
        # Which rows are clean
        rows.with.dates <- grepl("\\d{4}-\\d{2}-\\d{2}",
                                 my.clean.dates$YYMMDD )

        # Append the clean cloumns
        my.clean.dates$Valid.dates <- rows.with.dates
        temp.my.data.excel <- cbind(temp.my.data.excel,my.clean.dates )
        #
        #         # Now to add location columns
        #         colnames(temp.my.data.excel)[which(colnames(
        #           temp.my.data.excel) == "Latitude..WGS84.")] <- c("Lat")
        #         colnames(temp.my.data.excel)[which(colnames(
        #           temp.my.data.excel) == "Longitude..WGS84.")] <- c("Long")
        #
        #         # Create a time stamp in POSIX format
        # temp.time.split <- stringr::str_split(temp.my.data.excel$Timestamp, " ")
        # temp.time.split.df <- matrix(
        #   unlist(temp.time.split), ncol=2, byrow=TRUE) %>% as.data.frame()
        # colnames(temp.time.split.df) <- c("Date", "Time")
        # temp.my.data.excel$Time <- temp.time.split.df$Time
        # temp.my.data.excel$Date <- temp.time.split.df$Date
        nice.file.name <- make.names(my.file)
        nice.sheet.name <- make.names(sheet.name)
        # Add the file as source
        temp.my.data.excel$source <- paste(nice.file.name,"Sheet",nice.sheet.name, sep = ".")



        ################ Get the columns ##############
        # Intialise
        my.file.type <- c("unknown")
        test.temp.my.file <- temp.my.data.excel

        # Length of target list
        length.target <- length(my.columns[[1]])

        # Work through the list
        for(i in 1:length(my.columns)) {
          working.list <- my.columns[[i]]
          length.intersect <-length( intersect(colnames(test.temp.my.file),
                                               working.list))

          # Create an index we can use for transformations
          if( length.intersect == length.target && my.file.type == "unknown" ) {
            my.file.type <- paste("Type ",i, sep ="")
            test.my.file.cols <- test.temp.my.file[,working.list]
            # Rename the columns as per one
            colnames(test.my.file.cols) <- my.columns[[1]]

            test.temp.my.file <- test.my.file.cols

            # Now make a record of the transformation in the source column
            test.temp.my.file$source <- paste(test.temp.my.file$source,
                                              my.file.type, sep = "," )
          } # end add source index
          ############

          # Now test if the data OK
          save.directory <- "data-NA"

          if(!my.file.type == "unknown") {save.directory <- "data-sl"}

          # We will now save each file in data-clean or data-NA
          temp.nice.name <- paste(nice.file.name,nice.sheet.name, sep = "." )
          temp.nice.name.rds <- paste(temp.nice.name, ".rds", sep="")
          temp.data.to.save.path <- fs::path(save.directory, temp.nice.name.rds)

          saveRDS(test.temp.my.file, file = temp.data.to.save.path)

        } # End if
        ###############









                } # end sheets
                } # end check columns
              # Now add any specific the transformations










  } # End if files to transform

  number.of.files <- length(temp.list)
  return(number.of.files)
  }# End function




