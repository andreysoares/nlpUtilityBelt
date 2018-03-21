##
## Sample use of the Package nlpUtilityBelt
## Authors: Tiffany Callahan and Andrey Soares
##


################################################################################
# Install and Load package

# FROM BITBUCKET
install.packages("devtools")
library(devtools)

# If public repository
install_github("andreysoares/nlpUtilityBelt")

# Load library
library(nlpUtilityBelt)


################################################################################
## PUBTATOR
biogrid <- nlpUtilityBelt::getLatestBiogridData()

# (Optional) Filter by physical interactions for human proteins
biogrid <- subset(biogrid, biogrid$Experimental.System.Type == "physical" &
                    biogrid$Organism.Interactor.A == "9606" & biogrid$Organism.Interactor.B == "9606")

# Get list of Biogrid symbols
biogrid_symbols <- nlpUtilityBelt::getBiogridSymbols(biogrid)
head(biogrid_symbols)

# List of PMIDs
pmids <- c("9006895", "7545544", "9342365") # or use all ids from biogrid with "biogrid$Pubmed.ID"

# Retrieve Abstracts for selected PMIDs
abstracts <- nlpUtilityBelt::getPubmedAbstracts(pmids)

# View sample abstract
abstracts[abstracts$PMID=="9006895",]

# Find proteins and select sentences and titles with at least 2 matches
pubtator <- nlpUtilityBelt::getPubtatorMatches(abstracts,
                                        info = c("Genes"))
pubtator[pubtator$PMID=="9006895",]

################################################################################
## REGEX

# List of symbols to define the corresponding regular expression
symbols <- c("p53", "SEK-1", "ABP-280", "HBGF-7", "CD8", "Hsp90", "Tango9", "RpL29", "RanBP3", "ATPsyn-Cf6")

# Get patterns for each symbol
patternList <- nlpUtilityBelt::getPatternList(symbols,
                                              skipSinglePattern = TRUE,
                                              res = "individual",
                                              rm.duplicated = TRUE,
                                              ignore.case = TRUE,
                                              word.boundary = FALSE)
patternList

## getting patterns with ranges.
patternRanges <- nlpUtilityBelt::setPatternRange(patternList,
                                          colname = "symbols")
patternRanges

################################################################################
## NLP

abs <- abstracts[abstracts$PMID=="9006895",]
abs

sentences <- nlpUtilityBelt::sentenceTokenizer(abs$Abstract, type = "Quanteda")
pos <- 1 # select sentence number to analyze
sentences[pos]

tokens <- nlpUtilityBelt::wordTokenizer(sentences[pos], type = "openNLP")
tokens

postags <- nlpUtilityBelt::tagPOS(sentences[pos])
postags

selTokens <- nlpUtilityBelt::getTokens(postags, c("NN"))
selTokens

matches <- nlpUtilityBelt::matchTokens(patternList, selTokens, fixed = TRUE)
matches

verbTokens <- nlpUtilityBelt::getTokens(postags, c("VBZ", "VBP", "VBD", "VBN"))
verbTokens

ptext <- nlpUtilityBelt::parseSentence(sentences[pos])
ptext

nlpUtilityBelt::parse2graph(ptext,
                     margin=-0.15, asp=0.5,
                     vertex.color=NA, vertex.frame.color=NA,
                     vertex.label.font=1.5, vertex.label.cex=.8,
                     edge.width=.5, edge.color='black', edge.arrow.size=0)


# create a feature matrix of word occurrence - removing stop words and punctuation
feature_matrix = quanteda::dfm(quanteda::corpus(as.character(abs)),
                               remove = stopwords("english"),
                               remove_punct = TRUE)

quanteda::topfeatures(feature_matrix) #print top features

# plot a word cloud - word size related to frequency of occurrence in text
set.seed(100)
textplot_wordcloud(feature_matrix,
                   min.freq = 1,
                   random.order = FALSE,
                   rot.per = .25,
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))




