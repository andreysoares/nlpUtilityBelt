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
## BIOGRID
biogrid <- getLatestBiogridData()

# (Optional) Filter by physical interactions for human proteins
# biogrid_sub <- subset(biogrid, biogrid$Experimental.System == "Co-localization" &
#                     biogrid$Organism.Interactor.A == "9606" & biogrid$Organism.Interactor.B == "9606")

biogrid_sub <- subset(biogrid, biogrid$Organism.Interactor.A == "7227" & biogrid$Organism.Interactor.B == "7227")

# Get list of Biogrid symbols (includes synonyms)
biogrid_symbols <- nlpUtilityBelt::getBiogridSymbols(biogrid_sub)

# List of PMIDs
pmids <- c(biogrid_sub$Pubmed.ID)

# Retrieve Abstracts for selected PMIDs
pubmed_results <- nlpUtilityBelt::getPubmedAbstracts(pmids)

# remove rows with missing data - this fixed crashing issue
pubmed_results$Abstract[pubmed_results$Abstract==''] <- NA
pubmed_results$Title[pubmed_results$Title==''] <- NA
pubmed_results=pubmed_results[complete.cases(pubmed_results$Abstract),]
pubmed_results=pubmed_results[complete.cases(pubmed_results$Title),]

# replace '{, }' and other symbols
pubmed_results = as.data.frame(sapply(pubmed_results,gsub,pattern='[\\{, \\}]',replacement=' '))


################################################################################
## PROTEIN-PROTEIN INTERACTION
# get regex patterns
regex <- nlpUtilityBelt::getPatternList(biogrid_symbols$biogrid_symbols,
                                           skipSinglePattern = FALSE,
                                           res = 'individual',
                                           rm.duplicated = FALSE,
                                           ignore.case = TRUE)

#### NOTE: I need the version that does not include boundaries and need the ignore.case is still backwards

# interaction keywords
keywords= c("bind",
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

# identify protein-protein interactions
PPI_results = getPPIs(pubmed_results, regex, nlpUtilityBelt::getInteractionMatches, keywords)

# write out sentences
write.table(PPI_results, "7227_PPI_BIOGRID_results.txt", quote  = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE)
