
################################################################################
#' @title Find Pubtator information on Pubmed abstracts
#' @description \code{getPubtatorMatches} finds selected Pubtator information
#' that occurs at least twice in the title and in the abstract.
#' @param abstracts A data frame with a list of Pubmed abstracts, including:
#' PMID, Title and Abstract
#' @param info Check what PubTator information to use for list of symbols,
#' including genes, diseases, mutations, chemicals and species
#' @return The function "getPubtatorMatches" returns a data frame with four columns:
#' PMID, symbols, title, and sentences. If the regex encounters an error, it will
#' return the string "*** REGEX ERROR ***".
#' The column "symbols" represent the Pubtator info found in the astract or title.
#' The symbols are separated by a pipe "|".
#' The column "title" will show the title only if there are at least 2 symbols in it.
#' The colum "sentences" will show all sentences from the abstract that have at
#' least two occurrences of any symbol listed in the column "symbols". The sentences are separated by a pipe "|".
#' @details
#' This function requires the packages "pubmed.mineR" and "plyr".
#' @examples
#' ## getting Pubtator matches from abstracts extracted from Pubmed
#' pubtator <- getPubtatorMatches(abstracts,
#'                                info = c("genes"))
#' pubtator[1,]
################################################################################
getPubtatorMatches <- function(abstracts,
                               info = c("Genes", "Diseases", "Mutations", "Chemicals", "Species")) {

  #-------------------------------------------------
  # Function to perform regular expression to a sentence
  findInSentences <- function(sentence, symbols) {
    tryCatch({
      # match patterns and returns sentences that have at least two matches within the same sentence
      ifelse(!is.null(symbols) & length(which(gregexpr(paste(tolower(symbols), collapse = "|"), tolower(sentence))[[1]] > 0)) > 1, sentence[[1]], "")
    },error = function(e) {
      # if the regular expression has an error, return the following erro message
      "*** REGEX ERROR ***"
    })
  }

  #-------------------------------------------------
  # Load required package
  require(pubmed.mineR, quietly = TRUE) && require(plyr, quietly = TRUE)

  # Loop through each abstract
  df <- plyr::ddply(abstracts, .(PMID), function(abstract) {

    # extract PubTator information from selected abstract
    pubtator_output <- pubmed.mineR::pubtator_function(abstract["PMID"])

    # Check what PubTator information to use for list of symbols.
    # The user can define any or all types: Genes, Diseases, Mutations, Chemicals and Species
    symbols <- c()
    for(pubType in c("Genes", "Diseases", "Mutations", "Chemicals", "Species")) {
      if(tolower(pubType) %in% tolower(info) & !is.null(pubtator_output[pubType][[1]])){
        symbols <- c(symbols, pubtator_output[pubType][[1]])
      }
    }

    # Separate the abstract in different sentences
    sentences <- pubmed.mineR::SentenceToken(abstract["Abstract"])

    # Loop throught the sentences and find matches of symbols
    sent <- plyr::laply(sentences, findInSentences, symbols = symbols)

    # Remove sentence with no matches
    sent <- sent[sent!=""]

    # find matches in the title of the abstract
    title <- findInSentences(abstract["Title"], symbols)

    # Create a data frame with all symbols found, corresponding title and sentences
    data.frame(symbols=paste(symbols, collapse = "|"), title=title, sentences=paste(sent, collapse = "|"))
  }, .progress = "text")

  # Order data frame by PMIDs
  df <- df[order(df$PMID),]

  # 0 = no match, no symbols found by Pubtator, or REGEX ERROR in the sentences
  # 1 = Match at least 2 symbols in one sentence/title
  df$match <- ifelse(df$symbols == "" | df$sentences == "" | grepl("REGEX ERROR", df$sentences), 0, 1)

  # Return data frame with Pubtator matches
  df
}
