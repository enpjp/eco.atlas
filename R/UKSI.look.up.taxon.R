
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
#'@importFrom utils "head"
#'
UKSI.look.up.taxon <- function(taxon) {

  NAMES <- UKSI.names
  TAXA <- UKSI.taxa

  # which.rows <- which(UKSI$TAXON_NAME == taxon)
  All.names.out <- tibble::tibble(
    requested.taxon = taxon,
    TVK = "unknown",
    order = "unknown",
    family = "unknown",
    genus = "unknown",
    recommended_species = "unknown",
    authority = "unknown",
    vernacular = "unknown",
    SORT_ORDER = "unknown"

  )

  #
  # output.row <- UKSI[which.rows,]

  try(
    {
     # TVK.out <- TAXA %>% select(TAXON_VERSION_KEY,TAXON_NAME, SORT_ORDER) %>% filter(TAXON_NAME %in% taxon )
      # TVK.out <- NAMES %>% select(NBN_TAXON_VERSION_KEY,
      #                             TAXON_NAME,
      #                             NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME)
      cols.to.use <- c("NBN_TAXON_VERSION_KEY",
                       "TAXON_NAME",
                       "NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME")

      NAMES.cols <- NAMES[,cols.to.use]

      TVK.out <- NAMES.cols[NAMES.cols$TAXON_NAME == taxon,   ]


     # %>% filter(TAXON_NAME   %in% taxon )

      TVK.to.use <- TVK.out$NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME %>% unique()


 #    en.names.out <- NAMES %>% select(NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME,TAXON_NAME,NAME_FORM, LANGUAGE) %>%
 #      filter(NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME %in%  TVK.to.use, LANGUAGE == "en")

 #    NAMES.select <- select(NAMES, NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME,TAXON_NAME,NAME_FORM, LANGUAGE)
#     en.names.out <-filter(NAMES.select, NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME %in%  TVK.to.use, LANGUAGE == "en")

     cols.to.use <- c(
                      "NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME",
                      "TAXON_NAME",
                      "NAME_FORM",
                      "LANGUAGE")

     NAMES.cols <- NAMES[,cols.to.use]

     en.names.out.tvk <- NAMES.cols[NAMES.cols$NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME == TVK.to.use, ]
     en.names.out <- en.names.out.tvk[en.names.out.tvk$LANGUAGE == "en", ]



      en.names.out$NAME_FORM <- factor( en.names.out$NAME_FORM, c("W", "I", "R","S")  )
      en.names.out$levels <- as.numeric(en.names.out$NAME_FORM)
      en.names.out <- en.names.out %>% arrange(levels) %>% head(n=1)


 #     la.names.out <- NAMES %>% select(NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME,TAXON_NAME,NAME_FORM, LANGUAGE) %>%
  #      filter(NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME %in%  TVK.to.use, LANGUAGE == "la")
   #   NAMES.select <-  select(NAMES, NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME,TAXON_NAME,NAME_FORM, LANGUAGE)
  #    la.names.out <- filter(NAMES.select ,NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME %in%  TVK.to.use, LANGUAGE == "la")

      cols.to.use <- c("NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME",
                       "TAXON_NAME",
                       "TAXON_AUTHORITY",
                       "NAME_FORM",
                       "LANGUAGE"
      )

      NAMES.cols <- NAMES[,cols.to.use]

      NAMES.select.tvk <- NAMES.cols[NAMES.cols$NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME == TVK.to.use, ]
      la.names.out <- NAMES.select.tvk[NAMES.select.tvk$LANGUAGE == "la", ]



      la.names.out$NAME_FORM <- factor( la.names.out$NAME_FORM, c("W", "I", "R","S")  )
      la.names.out$levels <- as.numeric(la.names.out$NAME_FORM)
 #     la.names.out <- la.names.out %>% arrange(levels)

      # Latin Name
      # la.output.df <- tibble::tibble(TVK =  TVK.to.use,
      #                        taxon_name =  la.names.out$TAXON_NAME,
      #                        NAME_FORM_la = la.names.out$NAME_FORM,
      #                        name_level_la = la.names.out$levels) %>% arrange(name_level_la) %>% head(n = 1)
      #
      la.output.tibble <-  tibble::tibble(TVK =  TVK.to.use,
                                          taxon_name =  la.names.out$TAXON_NAME,
                                          taxon_authority = la.names.out$TAXON_AUTHORITY,
                                          NAME_FORM_la = la.names.out$NAME_FORM,
                                          name_level_la = la.names.out$levels)
      la.output.arranged <- arrange(la.output.tibble, "name_level_la")
      la.output.df <- head(la.output.arranged, n=1)


      # Vernacular Name
      # en.output.df <- tibble::tibble(
      #   vernacular =  en.names.out$TAXON_NAME,
      #   NAME_FORM_en = en.names.out$NAME_FORM,
      #   name_level_en = en.names.out$levels) %>% arrange(name_level_en) %>% head(n = 1)

      en.output.tibble     <-  tibble::tibble(
        vernacular =  " ",
        NAME_FORM_en = " ",
        name_level_en = " ")

      if(nrow(en.names.out) > 0){
        en.output.tibble     <-  tibble::tibble(
          vernacular =  en.names.out$TAXON_NAME,
          NAME_FORM_en = en.names.out$NAME_FORM,
          name_level_en = en.names.out$levels)

        en.output.arranged <-  arrange(en.output.tibble, "name_level_en") %>% head(n = 1)

        en.output.df <- head(en.output.arranged, n=1)
      }


      # Walk up the taxonomy

      taxonomy.cols <-    taxonomy.parent(TVK.to.use)
      # drop the Species name if it is there as we already have it

      taxonomy.ladder <- taxonomy.cols[, !names(taxonomy.cols) == "Species"]

      # Construct a tibble for output
      TAXON_AUTHORITY_clean  <-   brackets(la.names.out$TAXON_AUTHORITY)

      All.names.out <- tibble::tibble(
        requested.taxon = taxon,
        TVK =  TVK.to.use,
        recommended_species = la.output.df$taxon_name,
        authority = TAXON_AUTHORITY_clean,
        vernacular = en.output.tibble$vernacular

      ) %>% head(n=1)

      All.names.out <- cbind(All.names.out, taxonomy.ladder )

    }

  )




  return( All.names.out)
}
