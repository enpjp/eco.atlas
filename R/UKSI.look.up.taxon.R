
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
   #   en.names.out <- en.names.out %>% arrange(levels)


 #     la.names.out <- NAMES %>% select(NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME,TAXON_NAME,NAME_FORM, LANGUAGE) %>%
  #      filter(NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME %in%  TVK.to.use, LANGUAGE == "la")
   #   NAMES.select <-  select(NAMES, NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME,TAXON_NAME,NAME_FORM, LANGUAGE)
  #    la.names.out <- filter(NAMES.select ,NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME %in%  TVK.to.use, LANGUAGE == "la")

      cols.to.use <- c("NBN_TAXON_VERSION_KEY_FOR_RECOMMENDED_NAME",
                       "TAXON_NAME",
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
                        vernacular =  en.names.out$TAXON_NAME,
                         NAME_FORM_en = en.names.out$NAME_FORM,
                        name_level_en = en.names.out$levels)

      en.output.arranged <-  arrange(en.output.tibble, name_level_en) %>% head(n = 1)

      en.output.df <- head(en.output.arranged, n=1)



      # Family
      # fa.name.TVK <-   UKSI.taxa %>% select(PARENT_KEY, TAXON_VERSION_KEY) %>%
      #   filter(TAXON_VERSION_KEY ==  TVK.to.use) %>% head(n = 1)
 #     UKSI.taxa.selected <- select(UKSI.taxa,   "PARENT_KEY", "TAXON_VERSION_KEY")
 #     UKSI.taxa.filtered <-  filter(UKSI.taxa.selected,  TAXON_VERSION_KEY ==  TVK.to.use)
 #     fa.name.TVK <- head(UKSI.taxa.filtered, n=1)

      cols.to.use <- c("PARENT_KEY",
                       "TAXON_VERSION_KEY"
      )

      UKSI.taxa.selected <- UKSI.taxa[,cols.to.use]
      UKSI.taxa.filtered <- UKSI.taxa.selected[
        UKSI.taxa.selected$TAXON_VERSION_KEY == TVK.to.use,]
      fa.name.TVK <- head(UKSI.taxa.filtered, n=1)



      genus.out <- UKSI.taxa %>% select(TAXON_VERSION_KEY, RANK, TAXON_NAME, ORGANISM_KEY, PARENT_KEY) %>%
        filter(ORGANISM_KEY == fa.name.TVK$PARENT_KEY) %>% head(n = 1)

     cols.to.use <- c("TAXON_VERSION_KEY",
                      "RANK",
                     "TAXON_NAME",
                     "ORGANISM_KEY",
                     "PARENT_KEY")

       UKSI.taxa <- UKSI.taxa[,cols.to.use]
       genus.out <- UKSI.taxa[UKSI.taxa$ORGANISM_KEY == fa.name.TVK$PARENT_KEY,]



      family.out <- UKSI.taxa %>% select(TAXON_VERSION_KEY, RANK, TAXON_NAME, ORGANISM_KEY, PARENT_KEY) %>%
        filter(ORGANISM_KEY == genus.out$PARENT_KEY) %>% head(n = 1)

      order.out <- UKSI.taxa %>% select(TAXON_VERSION_KEY, RANK, TAXON_NAME, ORGANISM_KEY, PARENT_KEY) %>%
        filter(ORGANISM_KEY == family.out$PARENT_KEY) %>% head(n = 1)

      sort_order <- UKSI.taxa %>% select(TAXON_VERSION_KEY,TAXON_AUTHORITY, SORT_ORDER) %>%
        filter(TAXON_VERSION_KEY ==  TVK.to.use) %>% head(n = 1)

      # all.names.out <- bind_rows(en.names.out,la.names.out)
      #
      # all.names.out$NAME_FORM <- factor( all.names.out$NAME_FORM, c("W", "I", "R","S")       )
      #
      # all.names.out$levels <- as.numeric(all.names.out$NAME_FORM)

      # All.names.out <- cbind(la.output.df, en.output.df)

      # Construct a tibble for output

      All.names.out <- tibble::tibble(
        requested.taxon = taxon,
        TVK =  TVK.to.use,
        order = order.out$TAXON_NAME,
        family = family.out$TAXON_NAME,
        genus = genus.out$TAXON_NAME,
        recommended_species = la.output.df$taxon_name,
        authority = sort_order$TAXON_AUTHORITY,
        vernacular = en.output.df$vernacular,
        SORT_ORDER = sort_order$SORT_ORDER

      )

    }

  )




  return( All.names.out)
}
