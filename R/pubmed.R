
################################################################################
#' @title Extracts abstracts and titles from PUBMED from a list of selected PMIDs
#' @description \code{getPubmedAbstracts} extracts a list of abstracts from Pubmed for specific PMIDs.
#' @param pmids A list of Pubmed IDs
#' @return A dataframe with three columns: PMID, Title, and Abstract
#' @details This function requires the package "RISmed".
#' @examples
#' ## getting Pubmed abstracts
#' pmids <- c("7545544", "9342365")
#' abstracts <- getPubmedAbstracts(pmids)
#' abstracts[1,]
################################################################################
getPubmedAbstracts <- function(pmids) {
  # Load required package
  require(RISmed, quietly = TRUE)

  # get information from Pubmed articles included in the list of
  message("Retrieving Pubmed abstracts...")
  records = RISmed::EUtilsGet(unique(pmids))

  # Create a data frame with PMID, Title and Abstract
  df <- data.frame('PMID' = as.numeric(RISmed::PMID(records)),
                   'Title' = RISmed::ArticleTitle(records),
                   'Abstract'= RISmed::AbstractText(records),
                   stringsAsFactors = FALSE)

  # Order data frame by PMIDs
  df <- df[order(df$PMID),]

  # Return data frame with abstracts extracted from Pubmed
  df
}
