\name{getFilteredRegex}
\alias{getFilteredRegex}
\title{Protein-Protein Interaction Identifier Functions}
\description{
The function "getFilteredRegex" identifies the regular expression pattern for a given input list. Additional filtering is applied to the result to remove non-alpha numeric characters and characters less than length three.
}
\usage{
getFilteredRegex(list)
}
\arguments{
  \item{list}{a character vector}
}
\value{
The function "getFilteredRegex" returns a large character vector.
}

\details{
This function requires the \code{\link{getPatternList}} function.
}
\examples{
\dontrun{

## create a list of filtered symbols
filtered_symbols <- getFilteredRegex(symbol_list)

}

}