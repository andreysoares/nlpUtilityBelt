\name{getCompleteSymbols}
\alias{getCompleteSymbols}
\title{Protein-Protein Interaction Identifier Functions}
\description{
The function "getCompleteSymbols" extracts all symbols and symbol synonyms from a data frame and finds their gene names. The function returns a data frame with a single column that lists all unique symbols, symbol synonyms, and gene names.
}
\usage{
The function takes 
getCompleteSymbols(data)
}
\arguments{
  \item{data}{a data frame that contains contains columns of proteins and protein synonyms}
}
\value{
The function "getCompleteSymbols" returns a data frame with a single column that lists all unique symbols, symbol synonyms, and gene names.
}

\details{
This function requires the 'org.Hs.eg.db' library and the \code{\link{getPatternList}} function.
}
\examples{
\dontrun{

## get a single list of all symbols and names from a data frame
symbols <- getCompleteSymbols(data)

}

}