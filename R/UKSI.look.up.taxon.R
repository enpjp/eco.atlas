
#' Look Up Taxon From UKSI
#'
#' @param taxon character: species to lookup One at a time
#'
#' @return tibble
#' @export UKSI.look.up.taxon
#'
#'@importFrom magrittr "%>%"
#'@importFrom dplyr "select"
#'@importFrom dplyr "filter"
#'@importFrom dplyr "arrange"
#'@importFrom dplyr "desc"
#'@importFrom utils "head"
#'@importFrom stringr "str_sort"
#'
UKSI.look.up.taxon <- function(taxon) {

  NAMES <- UKSI.names
  TAXA <- UKSI.taxa

  # which.rows <- which(UKSI$TAXON_NAME == taxon)
  All.names.out <- tibble::tibble(
    requested.taxon = taxon,
    informalGroup = "unknown",
    TVK = "unknown",
    Order = "unknown",
    Family = "unknown",
    Genus = "unknown",
    recommended_species = "unknown",
    authority = "unknown",
    vernacular = "unknown",
    SORT_ORDER = "unknown"

  )

  #
  # output.row <- UKSI[which.rows,]

  if(taxon %in% NAMES$TAXON_NAME) {

              try(   {
                 # TVK.out <- TAXA %>% select(TAXON_VERSION_KEY,TAXON_NAME, SORT_ORDER) %>% filter(TAXON_NAME %in% taxon )
                  # TVK.out <- NAMES %>% select(TAXON_VERSION_KEY,
                  #                             TAXON_NAME,
                  #                             RECOMMENDED_TAXON_VERSION_KEY)
                  cols.to.use <- c("TAXON_VERSION_KEY",
                                   "TAXON_NAME",
                                   "RECOMMENDED_TAXON_VERSION_KEY",
                                   "NAME_STATUS",
                                   "NAME_FORM",
                                   "INFORMAL_GROUP")


                  NAMES.cols <- NAMES[,cols.to.use]

                  TVK.out <- NAMES.cols[NAMES.cols$TAXON_NAME == taxon,   ]

                  # Select recommended well formed names

               #   TVK.out <- TVK.out[TVK.out$NAME_STATUS == "R", ]
                #  TVK.out <- TVK.out[TVK.out$NAME_FORM == "W", ]



                 # %>% filter(TAXON_NAME   %in% taxon )

                  TVK.to.use <- TVK.out$RECOMMENDED_TAXON_VERSION_KEY %>%
                    unique() %>% stringr::str_sort(numeric = TRUE,
                                                   decreasing = FALSE) %>%
                    head(1)


             #    en.names.out <- NAMES %>% select(RECOMMENDED_TAXON_VERSION_KEY,TAXON_NAME,NAME_FORM, LANGUAGE) %>%
             #      filter(RECOMMENDED_TAXON_VERSION_KEY %in%  TVK.to.use, LANGUAGE == "en")

             #    NAMES.select <- select(NAMES, RECOMMENDED_TAXON_VERSION_KEY,TAXON_NAME,NAME_FORM, LANGUAGE)
            #     en.names.out <-filter(NAMES.select, RECOMMENDED_TAXON_VERSION_KEY %in%  TVK.to.use, LANGUAGE == "en")

                 cols.to.use <- c("INFORMAL_GROUP",
                                  "RECOMMENDED_TAXON_VERSION_KEY",
                                  "TAXON_NAME",
                                  "RECOMMENDED_SCIENTIFIC_NAME",
                                  "NAME_STATUS",
                                  "NAME_TYPE",
                                  "NAME_FORM",
                                  "LANGUAGE")

                 NAMES.cols <- NAMES[,cols.to.use]

                 en.names.out.tvk <- NAMES.cols[NAMES.cols$RECOMMENDED_TAXON_VERSION_KEY == TVK.to.use, ]
                 en.names.out <- en.names.out.tvk[en.names.out.tvk$LANGUAGE == "en", ]


                 en.names.out <- en.names.out[en.names.out$NAME_STATUS == "R", ]
                  en.names.out$NAME_FORM <- factor( en.names.out$NAME_FORM, c("W", "I", "R","S")  )
                  en.names.out$levels <- as.numeric(en.names.out$NAME_FORM)
                  en.names.out <- en.names.out %>% arrange(levels) %>% head(n=1)


             #     la.names.out <- NAMES %>% select(RECOMMENDED_TAXON_VERSION_KEY,TAXON_NAME,NAME_FORM, LANGUAGE) %>%
              #      filter(RECOMMENDED_TAXON_VERSION_KEY %in%  TVK.to.use, LANGUAGE == "la")
               #   NAMES.select <-  select(NAMES, RECOMMENDED_TAXON_VERSION_KEY,TAXON_NAME,NAME_FORM, LANGUAGE)
              #    la.names.out <- filter(NAMES.select ,RECOMMENDED_TAXON_VERSION_KEY %in%  TVK.to.use, LANGUAGE == "la")

                  cols.to.use <- c("INFORMAL_GROUP",
                                    "RECOMMENDED_TAXON_VERSION_KEY",
                                   "TAXON_NAME",
                                   "RECOMMENDED_SCIENTIFIC_NAME",
                                   "RECOMMENDED_NAME_AUTHORITY",
                                   "NAME_STATUS",
                                   "NAME_TYPE",
                                   "NAME_FORM",
                                   "LANGUAGE"
                  )

                  NAMES.cols <- NAMES[,cols.to.use]

                  NAMES.select.tvk <- NAMES.cols[NAMES.cols$RECOMMENDED_TAXON_VERSION_KEY == TVK.to.use, ]
                  la.names.out <- NAMES.select.tvk[NAMES.select.tvk$LANGUAGE == "la", ]


               #   la.names.out <- NAMES.select.tvk[NAMES.select.tvk$NAME_STATUS == "R", ]
                  la.names.out$NAME_FORM <- factor( la.names.out$NAME_FORM, c("W", "I", "R","S")  )
                  la.names.out$levels <- as.numeric(la.names.out$NAME_FORM)
                  la.names.out <- la.names.out %>% arrange(levels) %>% head(1)

                  # Latin Name
                  # la.output.df <- tibble::tibble(TVK =  TVK.to.use,
                  #                        taxon_name =  la.names.out$TAXON_NAME,
                  #                        NAME_FORM_la = la.names.out$NAME_FORM,
                  #                        name_level_la = la.names.out$levels) %>% arrange(name_level_la) %>% head(n = 1)
                  #
                  la.output.tibble <-  tibble::tibble(TVK =  TVK.to.use,
                                                      taxon_name =  la.names.out$RECOMMENDED_SCIENTIFIC_NAME,
                                                      taxon_authority = la.names.out$RECOMMENDED_NAME_AUTHORITY,
                                                      NAME_FORM_la = la.names.out$NAME_FORM,
                                                      name_level_la = la.names.out$levels)
                  la.output.arranged <- arrange(la.output.tibble, "name_level_la")
                  la.output.df <- head(la.output.arranged, n=1)


                  # Vernacular Name
                  # en.output.df <- tibble::tibble(
                  #   vernacular =  en.names.out$TAXON_NAME,
                  #   NAME_FORM_en = en.names.out$NAME_FORM,
                  #   name_level_en = en.names.out$levels) %>% arrange(name_level_en) %>% head(n = 1)
            #  UKSI.names %>% select( all_of(cols.to.use) ) %>% filter( TAXON_NAME == "Acleris schalleriana", NAME_STATUS == "R", NAME_FORM == "W")
                  en.output.tibble     <-  tibble::tibble(
                    vernacular =  " ",
                    NAME_FORM_en = " ",
                    name_level_en = " ")

                  if(nrow(en.names.out) > 0){
                    en.output.tibble     <-  tibble::tibble(
                      informalGroup = en.names.out$INFORMAL_GROUP,
                      vernacular =  en.names.out$TAXON_NAME,
                      NAME_FORM_en = en.names.out$NAME_FORM,
                      name_level_en = en.names.out$levels)

                    en.output.arranged <-  arrange(en.output.tibble, "name_level_en") %>% head(n = 1)

                    en.output.tibble <- head(en.output.arranged, n=1)
                  }



                  # Walk up the taxonomy

                  taxonomy.cols <-    taxonomy.parent(la.output.df$taxon_name)
                  # drop the Species name if it is there as we already have it

                  taxonomy.ladder <- taxonomy.cols[, !names(taxonomy.cols) == "Species"]

                  # Construct a tibble for output
                  TAXON_AUTHORITY_clean  <-   brackets(la.names.out$RECOMMENDED_NAME_AUTHORITY)

                  if(la.output.df$taxon_name == en.output.tibble$vernacular) {

                    vernacular.to.use <- " "
                  }else{
                    vernacular.to.use <- en.output.tibble$vernacular

                  }

                  if("informalGroup" %in% colnames(en.output.tibble)){

                    informal.Group <- en.output.tibble$informalGroup
                  }else{
                    informal.Group <- "Not assigned"
                  }

                  All.names.out <- tibble::tibble(
                    requested.taxon = taxon,
                    informalGroup = informal.Group,
                    TVK =  TVK.to.use,
                    recommended_species = la.output.df$taxon_name,
                    authority = TAXON_AUTHORITY_clean,
                    vernacular = vernacular.to.use


                  ) %>% head(n=1)

                  All.names.out <- cbind(All.names.out, taxonomy.ladder )

                }

              )   # End try
}


# Nake sure that there are no ampersands
  data.cleaned <- data.frame(lapply(All.names.out, function(x) {
    gsub("&", "and", x)
  }))

  return( data.cleaned)
}
