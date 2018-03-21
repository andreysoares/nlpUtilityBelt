\name{getPPIs}
\alias{getPPIs}
\title{Find Protein-Protein Interactions}
\description{
The function "getPPIs" identifies proteins in PubMed titles and abstracts and if a match is found returns matching information as data frame. The function searches among each sentence that contains two proteins using a user provided list of keywords for matches. If a match is found, the sentences containing the keywords and the matched keywords are appended to data frame.
}
\usage{
getPPIs(data, regex, getInteractionMatches, keywords)
}
\arguments{
  \item{data}{a data frame containing PubMed Ids, gene A and B symbols, synonyms list (each symbol separated by '|'), gene A and B names, and article title and abstract}
  \item{patterns}{a large character vector of symbols and names}
  \item{getInteractionMatches}{See specific function documentation}
  \item{keywords}{a list of keywords to identify PPIs}
}
\value{
The function "getPPIs" returns a data frame with seven columns: pmid, proteins, title, sentence, int_sentences, int_keywords, and match. If matched sentences do not contain any of the provided keywords the cell will contain "No keywords found in sentence" and or "No keywords". The data frame will contain the following columns:
  \item{pmid}{PubMed IDs}
  \item{proteins}{a list of protein symbols, synonyms, and gene names separated by '|'}
  \item{title}{PubMed article title}
  \item{sentences}{Sentences from abstract containing both gene A and gene B}
  \item{int_sentences}{Sentences from abstract containing gene A, gene B, and one or more of the keywords, separated by '|'}
  \item{int_keywords}{Matched keywords from int_sentences, separated by '|'}
  \item{match}{'0' or '1' to indicate articles where gene A and B where identified}
}

\details{
This function requires the packages "svMisc" and "stringr". It also uses the \code{\link{getInteractionMacthes}} function.
}
\examples{
\dontrun{

## getting protein-protein interaction matches from titles and abstracts extracted from PubMed
found_PPIs <- getPPIs(biogrid_data, patterns, getInteractionMatches, c('interaction, association, binding'))

## full example of how this function can be used
# load biogrid data
biogrid = getLatestBiogridData()

# filter data to inlcude only certain types of interactions and organisms - human + colocalization
biogrid_human_data <- biogrid[ which(biogrid$Organism.Interactor.A == 9606 & biogrid$Organism.Interactor.B == 9606),]
biogrid_human_coloc <- biogrid_human_data[ which(biogrid_human_data$Experimental.System == "Co-localization"),]

# remove uneeded columns
biogrid_human_coloc <- biogrid_human_coloc[,c("Entrez.Gene.Interactor.A",
                                             "Entrez.Gene.Interactor.B",
                                             "Official.Symbol.Interactor.A",
                                             "Official.Symbol.Interactor.B",
                                             "Synonyms.Interactor.A",
                                             "Synonyms.Interactor.B",
                                             "Pubmed.ID")]

# add gene name to filtered biogrid data
symbols <- unlist(c(unique(biogrid_human_coloc$Entrez.Gene.Interactor.A), 
                   c(unique(biogrid_human_coloc$Entrez.Gene.Interactor.B))), recursive=F)

gene_name <- aggregate(GENENAME ~ ENTREZID, data = select(org.Hs.eg.db, symbols, "GENENAME", "ENTREZID"), FUN = unique)

# combine information for rows with same PubMed ID
# gene A
final_merged_biogrid_data <- merge(biogrid_human_coloc, 
                                  gene_name, 
                                  by.x = "Entrez.Gene.Interactor.A", by.y = "ENTREZID", 
                                  all.x = TRUE)
# rename column
colnames(final_merged_biogrid_data)['GENENAME'] <- 'GENENAME_A'

# gene B
final_merged_biogrid_data = merge(final_merged_biogrid_data, 
                                  gene_name, 
                                  by.x = "Entrez.Gene.Interactor.B", by.y = "ENTREZID", 
                                  all.x = TRUE)
# rename column
colnames(final_merged_biogrid_data)['GENENAME'] <- 'GENENAME_B'

# Query PubMed - Given a set of PubMed ids for analysis, we retrieve the titles and abstracts
pmids <- unlist(lapply(as.matrix(unique(final_merged_biogrid_data$Pubmed.ID)), function(x)as.character(x)))

# retrieve pubmed abstracts and titles for pmids in data set
pubmed_results <- getPubmedAbstracts(biogrid$Pubmed.ID)

# combine results with final data
merged_biogrid_pubmed_results <- merge(final_merged_biogrid_data, 
                                      pubmed_results, 
                                      by.x = "Pubmed.ID", by.y = "PMID", all = TRUE)

# only keep complete rows
merged_biogrid_pubmed_results$Abstract[merged_biogrid_pubmed_results$Abstract==''] <- NA
merged_biogrid_pubmed_results <- merged_biogrid_pubmed_results[complete.cases(merged_biogrid_pubmed_results$Abstract),]

# get a list of all symbols and gene names
symbols = getCompleteSymbols(merged_biogrid_pubmed_results)

# get regex patterns
patterns = getFilteredRegex(symbols)

# interaction keywords
keywords <- c("bind", 
              "interact",
              "associate",
              "regulation",
              "bound",
              "localize",
              "stimulation",
              "regulate",
              "effect",
              "target",
              "component",
              "member",
              "mediate")

# loop over abstracts - get subset of the data for testing
tokens <- getPPIs(merged_biogrid_pubmed_results, patterns, getInteractionMatches, keywords)

# write out sentences
write.table(tokens, "Mapped_proteins.txt", quote  = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE)

}

}