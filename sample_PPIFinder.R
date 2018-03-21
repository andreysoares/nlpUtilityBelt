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
biogrid <- nlpUtilityBelt::getLatestBiogridData()

# (Optional) Filter by physical interactions for human proteins
biogrid_sub <- subset(biogrid, biogrid$Experimental.System == "Co-localization" &
                    biogrid$Organism.Interactor.A == "9606" & biogrid$Organism.Interactor.B == "9606")

# Get list of Biogrid symbols (includes synonyms)
biogrid_symbols <- nlpUtilityBelt::getBiogridSymbols(biogrid_sub)

# biogrid_symbols_names <- nlpUtilityBelt::getCompleteSymbols(biogrid_symbols, "human")
#### NOTE: add this back when the get PPIs function is working

head(biogrid_symbols); tail(biogrid_symbols)

# List of PMIDs
pmids <- c(biogrid_sub$Pubmed.ID)

# Retrieve Abstracts for selected PMIDs
pubmed_results <- nlpUtilityBelt::getPubmedAbstracts(pmids)

# remove rows with missing data - this fixed crashing issue
pubmed_results$Abstract[pubmed_results$Abstract==''] <- NA
# pubmed_results$Title[pubmed_results$Title==''] <- NA
pubmed_results=pubmed_results[complete.cases(pubmed_results$Abstract),]
# pubmed_results=pubmed_results[complete.cases(pubmed_results$Title),]


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

# sample data
smp_size <- floor(0.90 * nrow(pubmed_results))
# set the seed to make your partition reproductible
set.seed(123)
training_samp <- sample(seq_len(nrow(pubmed_results)), size = smp_size)

# subset data
train <- pubmed_results[training_samp, ]
test <- pubmed_results[-training_samp, ]

# identify protein-protein interactions
data = pubmed_results

PPI_results = nlpUtilityBelt::getPPIs(data, regex, nlpUtilityBelt::getInteractionMatches, keywords)

# write out sentences
write.table(PPI_results, "PPI_BIOGRID_results.txt", quote  = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE)
