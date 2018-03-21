\name{getInteractionMatches}
\alias{getInteractionMatches}
\title{Protein-Protein Interaction Identifier Functions}
\description{
The function "getInteractionMatches" searches among each sentence that contains two proteins using a user provided list of keywords for matches. If a match is found, the sentences containing the keywords and the matched keywords are returned as a nested list.
}
\usage{
getInteractionMatches(data, keywords)
}
\arguments{
  \item{data}{a character vector of sentences separated by '|'}
  \item{keywords}{a list of keywords to identify PPIs}
}
\value{
The function "getInteractionMatches" returns a nested list with two lists: sentences containing keywords, keywords. If matched sentences do not contain any of the provided keywords the cell will contain "No keywords found in sentence" and or "No keywords". The nested list will contain the following sub-lists:
  \item{sents}{Sentences from abstract containing gene A, gene B, and one or more of the keywords, separated by '|'}
  \item{kws}{Matched keywords from int_sentences, separated by '|'}
}

\details{
This function requires the packages "quanteda".
}
\examples{
\dontrun{

## identifiying sentences contiaing specific keywords
interaction_info <- getInteractionMatches(sentences c('interaction, association, binding'))

}

}