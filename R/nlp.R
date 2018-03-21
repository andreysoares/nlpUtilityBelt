
################################################################################
#' @title Split text into an array of sentences
#' @description 
#' @param text 
#' @param type 
#' @return 
#' @details
#' @examples 
#' 
################################################################################
sentenceTokenizer <- function(text, type = "Quanteda") {
  gc()
  if(tolower(type) == "qdap") {
    require(qdap, quietly = TRUE)
    sent_detect_nlp(text)
  } else if(tolower(type) == "quanteda") {
    require(quanteda, quietly = TRUE)
    sentences <- tokens(text, what = "sentence")
    attr(sentences, "types")
  } else if(tolower(type) == "opennlp") {
    require(openNLP, quietly = TRUE)
    s <- as.String(text)
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    annotator <- annotate(s, list(sent_token_annotator))
    s[annotator]
  } else if(tolower(type) == "regex") {
    require("NLP", quietly = TRUE)
    s <- as.String(text)
    sent_tokenizer(s)
    sent_token_annotator <- Simple_Sent_Token_Annotator(function(s) {
      m <- gregexpr("[^[:space:]][^.]*\\.", s)[[1L]]
      Span(m, m + attr(m, "match.length") - 1L)
    })
    a1 <- annotate(s, sent_token_annotator)
    s[a1]
  } else if(tolower(type) == "tokenizers"){
    require(tokenizers, quietly = TRUE)
    tokenize_sentences(text)[[1]]
  } 
  
}


################################################################################
#' @title generate an array with tokens
#' @description 
#' @param text 
#' @param type 
#' @return 
#' @details
#' @examples 
#' 
################################################################################
wordTokenizer <- function(text, type = "openNLP") {
  gc()
  if(tolower(type) == "qdap") {
    require(qdap, quietly = TRUE)
    qdap::word_split(text)
  } else if(tolower(type) == "quanteda") {
    require(quanteda, quietly = TRUE)
    quanteda::tokens(text, what = c('word'))
  } else if(tolower(type) == "opennlp"){
    require(openNLP, quietly = TRUE) & require(NLP, quietly = TRUE)
    s <- as.String(text)
    annotSentence <- Annotation(1L, "sentence", 1L, nchar(s))
    annotWord <- annotate(s, Maxent_Word_Token_Annotator(), annotSentence)
    s[annotWord[annotWord$type == "word"]]
  } else if(tolower(type) == "tokenizers") {
    require(tokenizers, quietly = TRUE)
    tokenize_words(text, lowercase = FALSE)[[1]]
  } 
  
}


################################################################################
#' @title Define POS Tags for a sentence
#' @description 
#' @param text 
#' @return 
#' @details
#' @examples 
#' 
################################################################################
tagPOS <-  function(text, ...) {
  gc()
  require(openNLP, quietly = TRUE) && require(NLP, quietly = TRUE)
  s <- as.String(text)
  annotSentence <- Annotation(1L, "sentence", 1L, nchar(s))
  annotWord <- annotate(s, Maxent_Word_Token_Annotator(), annotSentence)
  annotPOS <- annotate(s, Maxent_POS_Tag_Annotator(), annotWord)
  POS <- annotPOS[annotPOS$type == "word"]
  POStags <- unlist(lapply(POS$features, `[[`, "POS"))
  POStext <- s[POS]
  POStagged <- paste(sprintf("%s/%s", POStext, POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags, POStext = POStext)
}

################################################################################
#' @title Define POS Tags for a sentence - option 2
#' @description 
#' @param text 
#' @return 
#' @details
#' @examples 
#' 
################################################################################
extractPOS = function(x, thisPOSregex) {
  require(stringr) && require(NLP) && require(openNLP)
  x = as.String(as.character(x))
  wordAnnotation = annotate(x, list(openNLP::Maxent_Sent_Token_Annotator(), openNLP::Maxent_Word_Token_Annotator()))
  POSAnnotation = annotate(x, openNLP::Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords = subset(POSAnnotation, type == "word")
  tags = sapply(POSwords$features, '[[', "POS")
  thisPOSindex = grep(thisPOSregex, tags)
  tokenizedAndTagged = sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
  untokenizedAndTagged = paste(tokenizedAndTagged, collapse = " ")
  
  res = strsplit(as.character(untokenizedAndTagged ), " ")
  names(res) = 'POS_tags'
  return(res)
}


################################################################################
#' @title Parse sentence with POS tags
#' @description 
#' @param text
#' @return 
#' @details
#' @examples 
#' 
################################################################################
parseSentence <-  function(text, ...) {
  gc()
  require(openNLP, quietly = TRUE)
  s <- as.String(text)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  parse_annotator <- Parse_Annotator()
  annotator <- annotate(s, list(sent_token_annotator, word_token_annotator))
  p <- parse_annotator(s, annotator)
  sapply(p$features, `[[`, "parse")
}


################################################################################
#' @title Make a graph from Tree_parse result
#' @description Source: \url{http://stackoverflow.com/questions/33473107/visualize-parse-tree-structure/33536291}
#' @param ptext 
#' @param leaf.color 
#' @param label.color 
#' @param title 
#' @param cex.main 
#' @return 
#' @details
#' @examples 
#' 
################################################################################
parse2graph <- function(ptext, leaf.color='chartreuse4', label.color='blue4',
                        title=NULL, cex.main=.9, ...) {
  require(NLP, quietly = TRUE) && require(igraph, quietly = TRUE)

  ## Replace words with unique versions
  ms <- gregexpr("[^() ]+", ptext)                                      # just ignoring spaces and brackets?
  words <- regmatches(ptext, ms)[[1]]                                   # just words
  regmatches(ptext, ms) <- list(paste0(words, paste("#$#",seq.int(length(words)), sep="")))  # add id to words

  ## Going to construct an edgelist and pass that to igraph
  ## allocate here since we know the size (number of nodes - 1) and -1 more to exclude 'TOP'
  edgelist <- matrix('', nrow=length(words)-2, ncol=2)

  ## Function to fill in edgelist in place
  edgemaker <- (function() {
    i <- 0                                      # row counter

    g <- function(node) {                       # the recursive function
      if (inherits(node, "Tree")) {             # only recurse subtrees
        if ((val <- node$value) != 'TOP#$#1') {    # skip 'TOP' node (added '1' above)
          for (child in node$children) {
            childval <- ifelse(inherits(child, "Tree"), child$value, child)
            i <<- i+1
            edgelist[i,1:2] <<- c(val, childval)
          }
        }
        invisible(lapply(node$children, g))
      }
    }

  })()

  ## Create the edgelist from the parse tree
  edgemaker(Tree_parse(ptext))

  ## Make the graph, add options for coloring leaves separately
  g <- graph_from_edgelist(edgelist)
  vertex_attr(g, 'label.color') <- label.color  # non-leaf colors
  vertex_attr(g, 'label.color', V(g)[!degree(g, mode='out')]) <- leaf.color
  V(g)$label <- lapply(V(g)$name, function(x){substring(x, 1, gregexpr(pattern ='\\#\\$\\#', x)[[1]]-1)}) # remove the numbers for labels
  plot(g, layout=layout_as_tree, ...)
  if (!missing(title))
    title(title, cex.main=cex.main)
}


################################################################################
#' @title discover regular expressions from string
#' @description 
#' @param postag
#' @param tags
#' @return 
#' @details
#' @examples 
#' 
################################################################################
getTokens <- function(postag, tags = c("NN")) {
  tokens <- data.frame(pos=NA, token=NA)
  count <- 1
  for(i in 1:length(postag$POStags)) {
    if(postag$POStags[i] %in% tags) {
      tokens[count, "pos"] <- i
      tokens[count, "token"] <- postag$POStext[i]
      count <- count + 1
    }
  }
  tokens
}


################################################################################
#' @title Match tokens
#' @description 
#' @param patternList 
#' @param selTokens 
#' @param fixed 
#' @param window
#' @return 
#' @details
#' @examples 
#' 
################################################################################
matchTokens <- function(patternList, selTokens, fixed = FALSE, window = 500) {
  start <- 1
  ite <- ceiling(nrow(patternList) / window)
  
  mat <- data.frame()
  count <- 1
  for(i in 1:ite) {
    
    if(i == ite) {
      end <- ceiling(i*window-(i*window-nrow(patternList)))
    } else {
      end <- ceiling(i*window)
    }
    regexResult <- regexpr(pattern=paste(patternList$patterns[start:end ], collapse = "|"), text = selTokens$token)
    selTokens$matchsize <- attr(regexResult,"match.length")
    matches <- which(apply(selTokens, 1, function(x) {
      (ifelse(fixed, nchar(x[2]) == as.numeric(x[3]), TRUE) & as.numeric(x[3]) > 0)}))
    mat <- rbind(mat, selTokens[matches, ])
    
    start <- start + window
    count <- count + 1
  }
  unique(mat[order(mat$pos), -3])
}

