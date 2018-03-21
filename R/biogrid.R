
################################################################################
#' @title Get latest version of BioGRID
#' @description \code{getLatestBiogridData} downloads the latest BioGRID file
#' (BIOGRID-ALL-LATEST.tab2.zip) available at \url{https://thebiogrid.org/download.php}
#' @return A data frame with the latest BioGRID content
#' @details This function requires the package "data.table".
#' @examples
#' ## Downloading the latest BioGRID data
#' biogrid <- getLatestBiogridData()
################################################################################
getLatestBiogridData <- function() {
  require(data.table, quietly = TRUE)

  # Download BioGRID ZIP File
  url <- "https://thebiogrid.org/downloads/archives/Latest%20Release/BIOGRID-ALL-LATEST.tab2.zip"
  temp <- tempfile()
  download.file(url, temp)

  # Unzip and Read Biogrid content
  df <- data.table::fread(unzip(temp),
                          header = TRUE,
                          sep = "\t",
                          stringsAsFactors = FALSE,
                          data.table = FALSE,
                          colClasses = "character")
  colnames(df) <- make.names(colnames(df))

  # Return data frame with BioGRID content
  df
}


################################################################################
#' @title Extracts unique Biogrid Symbols
#' @description \code{getBiogridSymbols} returns a list of unique BioGRID symbols
#' @param biogrid A data frame with the BioGRID data
#' @return A data frame with one column with the unique BioGRID symbols.
#' @details
#' The symbols are extracted from the columns: \code{Official.Symbol.Interactor.A},
#' \code{Official.Symbol.Interactor.B}, \code{Synonyms.Interactor.A}, and
#' \code{Synonyms.Interactor.B}.
#' @examples
#' biogrid_symbols <- getBiogridSymbols(biogrid)
################################################################################
getBiogridSymbols <- function(biogrid) {

  # Get unique symbols from Interactors A and B and Synonyms.Interactors A and B
  biogrid_symbols <- unique(c(
    unique(biogrid$Official.Symbol.Interactor.A),
    unique(biogrid$Official.Symbol.Interactor.B),
    unlist(lapply(biogrid$Synonyms.Interactor.A, function(x) {unlist(strsplit(x, "\\|"))})),
    unlist(lapply(biogrid$Synonyms.Interactor.B, function(x) {unlist(strsplit(x, "\\|"))}))
  ))

  # return a data frame with BioGRID symbols
  as.data.frame(biogrid_symbols, stringsAsFactors = FALSE)
}
