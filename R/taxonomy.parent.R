
taxonomy.parent <- function(taxon) {

 # NAMES <- UKSI.names
  TAXA <- UKSI.taxa

  cols.to.use <- c("TAXON_VERSION_KEY",
                   "RANK",
                   "TAXON_NAME",
                   "SORT_ORDER",
                   "ORGANISM_KEY",
                   "PARENT_KEY")

  UKSI.taxa.cols <- TAXA[,cols.to.use]
  parent.out <- UKSI.taxa.cols[
    UKSI.taxa.cols$TAXON_NAME == taxon,] %>%
    arrange(desc("ORGANISM_KEY")) %>%
    head(n = 1)

  out.cols <- data.frame (parent.out$TAXON_NAME )
 colnames(out.cols) <- parent.out$RANK

 out.sort <- data.frame (SORT_ORDER = parent.out$SORT_ORDER )

 #out.cols <- cbind( out.cols, out.sort)

# Now add a while loop to climb all the way up the taxonomy tree.
 # Add a counter to stop after 20 steps anyway.

 while(!is.na(parent.out$PARENT_KEY)) {

   UKSI.taxa.cols <- TAXA[,cols.to.use]
   parent.out <- UKSI.taxa.cols[
     UKSI.taxa.cols$ORGANISM_KEY == parent.out$PARENT_KEY,]

   out.cols.recursive <- data.frame (parent.out$TAXON_NAME )
   colnames(out.cols.recursive)[1] <- parent.out$RANK

   out.cols <- cbind(out.cols, out.cols.recursive)
 }

 out.cols <- cbind( out.cols, out.sort)
  return(out.cols)
}
