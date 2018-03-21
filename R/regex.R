
################################################################################
#' @title Discover regular expressions from string
#' @description \code{getPatternList} define regular expressions from symbols.
#' For example, the symbol "SEK-1" will generate the regular expression
#' "[a-zA-Z]\{3\}[\-][0-9]\{1\}".
#' @param symbols  a list of symbols to discover regular expressions
#' @param skipSinglePattern  a logical value to select multiple patterns
#' @param res  type of regex to be created: generic, individual or restricted.
#' Generic will use the plus sign to represent several occurences
#' (ex: [a-zA-Z]+[0-9]+), the Individual option will use the specific number of
#' occurrences (ex: [a-zA-Z]\{2\}[0-9]\{3\}), and the restricted option will use
#' the minimum and maximum occurences for a type of pattern (ex:
#' [a-zA-Z]\{2-4\}[0-9]\{1-\}).
#' @param rm.duplicated  remove duplicated regular expressions
#' @param ignore.case whether to differentiate lower/upper cases in the patterns. 
#' If TRUE, a pattern containing lower and upper case letters will have different expressions,
#' one for lower case (ex: [a-z]) and one for upper case (ex: [A-Z]).
#' If FALSE, a patter with lower and upper case letters will have a combined expression such as
#' [a-zA-Z].
#' @param word.boundary logical value to inform whether to surround the regular expressions with 
#' the word boundary character. This will prevent partial matches.
#' @return A dataframe with list of patterns for the symbols informed
#' @details The following characters are handled by this function:
#' " ", "_", ".", "-", "@", ":", "#", "/", "*", "&", "?", ";", "$".
#' This function requires the package "plyr".
#' @examples
#' ## getting Patterns List
#' symbols <- c("p53", "ABP-280", "Hsp90")
#' patternsList <- getPatternList(symbols,
#'                                skipSinglePattern = TRUE,
#'                                res = "individual",
#'                                rm.duplicated = TRUE,
#'                                ignore.case = TRUE,
#'                                word.boundary = FALSE)
#' patternsList
################################################################################
getPatternList <- function(symbols,
                           skipSinglePattern = TRUE,
                           res = "individual",
                           rm.duplicated = FALSE, 
                           ignore.case = FALSE,
                           word.boundary = FALSE) {

  #-------------------------------------------------
  # Return the type of character of a single string
  typeCharacter <- function(char) {
    
    if(ignore.case == TRUE & grepl("[a-zA-Z]", char)) {
      type <- "alpha"
    } else if(ignore.case == FALSE & grepl("[a-z]", char)) {
      type <- "alphalower"
    } else if(ignore.case == FALSE & grepl("[A-Z]", char)) {
      type <- "alphaupper"
    } else if(grepl("[0-9]", char)) {
      type <- "numeric"
    } else if(char == " ") {
      type <- "space"
    } else if(char == "_"){
      type <- "underline"
    } else if(char == ".") {
      type <- "dot"
    } else if(char == "-") {
      type <- "dash"
    } else if(char == "@") {
      type <- "at"
    } else if(char == ":") {
      type <- "colon"
    } else if(char == "#") {
      type <- "pound"
    } else if(char == "/") {
      type <- "slash"
    } else if(char == "*") {
      type <- "asterix"
    } else if(char == "&") {
      type <- "ampersand"
    } else if(char == "?") {
      type <- "question"
    } else if(char == ";") {
      type <- "semi-colon"
    } else if(char == "$") {
      type <- "dollar"
    } else if(char == "(") {
      type <- "openParenthesis"
    } else if(char == ")") {
      type <- "closeParenthesis"
    } else if(char == "[") {
      type <- "openSquareBraket"
    } else if(char == "]") {
      type <- "closeSquareBraket"
    } else if(char == "{") {
      type <- "openCurlyBraket"
    } else if(char == "}") {
      type <- "closeCurlyBraket"
    } else if(grepl("\\W", char)) {
      type <- "character"
    } else {
      type <- "other"
      print(char)
    }
    type
  }

  #-------------------------------------------------
  # Return the regular expression of a string
  getRegex <- function(x, res = "individual") {
    regex <- ""
    for(pat in x) {
      s <- strsplit(pat, "\\|")
      
      if(ignore.case == TRUE & s[[1]][1] == "alpha") {
        regex <- paste(regex, as.character("[a-zA-Z]"), sep = "")
      } else if(ignore.case == FALSE & s[[1]][1] == "alphalower") {
        regex <- paste(regex, as.character("[a-z]"), sep = "")
      } else if(ignore.case == FALSE & s[[1]][1] == "alphaupper") {
        regex <- paste(regex, as.character("[A-Z]"), sep = "")
      } else if(s[[1]][1] == "numeric") {
        regex <- paste(regex, as.character("[0-9]"), sep = "")
      } else if(s[[1]][1] == "character") {
        regex <- paste(regex, as.character("[\\W]"), sep = "")
      } else if(s[[1]][1] == "space") {
        regex <- paste(regex, as.character("[\\S]"), sep = "")
      } else if(s[[1]][1] == "underline") {
        regex <- paste(regex, as.character("[\\_]"), sep = "")
      } else if(s[[1]][1] == "dot") {
        regex <- paste(regex, as.character("[\\.]"), sep = "")
      } else if(s[[1]][1] == "dash") {
        regex <- paste(regex, as.character("[\\-]"), sep = "")
      } else if(s[[1]][1] == "at") {
        regex <- paste(regex, as.character("[\\@]"), sep = "")
      } else if(s[[1]][1] == "colon") {
        regex <- paste(regex, as.character("[\\:]"), sep = "")
      } else if(s[[1]][1] == "pound") {
        regex <- paste(regex, as.character("[\\#]"), sep = "")
      } else if(s[[1]][1] == "slash") {
        regex <- paste(regex, as.character("[\\/]"), sep = "")
      } else if(s[[1]][1] == "asterix") {
        regex <- paste(regex, as.character("[\\*]"), sep = "")
      } else if(s[[1]][1] == "ampersand") {
        regex <- paste(regex, as.character("[\\&]"), sep = "")
      } else if(s[[1]][1] == "question") {
        regex <- paste(regex, as.character("[\\?]"), sep = "")
      } else if(s[[1]][1] == "semi-colon") {
        regex <- paste(regex, as.character("[\\;]"), sep = "")
      } else if(s[[1]][1] == "dollar") {
        regex <- paste(regex, as.character("[\\$]"), sep = "")
      } else if(s[[1]][1] == "openParenthesis") {
        regex <- paste(regex, as.character("[\\(]"), sep = "")
      } else if(s[[1]][1] == "closeParenthesis") {
        regex <- paste(regex, as.character("[\\)]"), sep = "")
      } else if(s[[1]][1] == "openSquareBraket") {
        regex <- paste(regex, as.character("[\\[]"), sep = "")
      } else if(s[[1]][1] == "closeSquareBraket") {
        regex <- paste(regex, as.character("[\\]]"), sep = "")
      } else if(s[[1]][1] == "openCurlyBraket") {
        regex <- paste(regex, as.character("[\\{]"), sep = "")
      } else if(s[[1]][1] == "closeCurlyBraket") {
        regex <- paste(regex, as.character("[\\}]"), sep = "")
      }

      if(res == "generic") {
        if(length(x) > 1 & (s[[1]][1] %in% c("alpha", "alphalower", "alphaupper", "numeric"))){
          regex <- paste(regex, "+", sep = "")
        }
      } else if(res %in% c("individual","restricted")){
        regex <- paste(regex, sprintf("{%s}", s[[1]][2]), sep = "")
      }
    }
    
    # surround the regex with word boundary character (\b)
    if(word.boundary == TRUE) {
      sprintf("\\b%s\\b", regex)
    } else {
      regex
    }
  }

  #-------------------------------------------------
  require(plyr, quietly = TRUE)

  patterns <- plyr::laply(symbols, function(symbol){
    pattern <- list()
    count <- 1
    current <- typeCharacter(substring(symbol,1, 1))
    seq <- 0
    characters <- strsplit(symbol, "")[[1]]

    for(j in 1:length(characters)) {
      type <- typeCharacter(characters[j])

      if(type != current) {
        pattern[count] <- paste(current, seq, sep = "|")
        count <- count + 1
        current <- type
        seq <- 1
      } else {
        seq <- seq + 1
      }

    }
    pattern[count] <- paste(current, seq, sep = "|")
    count <- count + 1

    # Check if needs to skip single patterns
    if(skipSinglePattern == FALSE | (skipSinglePattern == TRUE & length(pattern) > 1)) {
      return(getRegex(pattern, res = res))
    } else {
      return(NA)
    }
  }, .progress = "text")

  # create a data frame with symbols and their corresponding patterns
  df <- data.frame(symbols, patterns, stringsAsFactors = FALSE)

  # remove NAs, which is the singlePatterns
  df <- df[!is.na(df[,2]),]

  # remove duplicated regular expressions
  if(rm.duplicated == TRUE) {
    df <- df[!duplicated(df[,2]),]
  }

  # Combine patterns if restriced
  if(res == "restricted") {
    df <- setPatternRange(df)
  }

  # return a data frame with a list of symbols and patterns
  df
}


################################################################################
#' @title Set range for similar regular expressions
#' @description \code{setPatternRange} returns a data frame with the list of
#' restriced patterns.
#' @param pl  A list of patterns
#' @param colname  The name of the column containing the symbols
#' @return a dataframe with patterns converted to restrict format.
#' The restricted option combines the number of patterns using the min and max
#' number of occurrencens. For example: [a-zA-Z]{2-3}
#' @examples
#' ## getting patterns with ranges
#' patternRanges <- getPatternsRange(patternsList)
#' patternRanges[1,]
################################################################################
setPatternRange <- function(pl,
                            colname = "symbols") {

  #-------------------------------------------------
  getNumSeqs <- function(x) {
    find <- gregexpr("\\{\\d+\\}",x)[[1]]
    value <- c()
    for(j in 1:length(find)) {
      pos1 <- find[j]
      len <- attr(find,"match.length")[j]
      value <- c(value, as.numeric(substring(x, pos1+1, pos1+len-2)))
    }
    value
  }

  #-------------------------------------------------
  patternList <- data.frame(NA, NA)
  colnames(patternList) <- c(colname, "patterns")
  tot <- 1
  rx <- data.frame(pl[order(gsub(pattern = "\\{\\d+\\}", replacement = "#$#", x = pl$patterns), pl$symbols),],
                   stringsAsFactors = FALSE)
  rx$patterns <- as.character(rx$patterns)
  seq <- list()
  count <- 1
  current <- rx$patterns[1]
  currentSymbol <- rx[1, colname]
  seq[[count]] <- getNumSeqs(current)
  count <- count + 1

  pb <- txtProgressBar(min = 1, max = nrow(rx), style = 3)
  for(i in 2:nrow(rx)) {
    pat <- rx$patterns[i]
    patSymbol <- rx[i, colname]
    if(gsub(pattern = "\\{\\d+\\}", replacement = "#$#", x = current) ==
       gsub(pattern = "\\{\\d+\\}", replacement = "#$#", x = pat)) {

      seq[[count]] <- getNumSeqs(pat)
      count <- count + 1
    } else {
      seq <- t(data.frame(seq))

      find <- gregexpr("\\{\\d+\\}",current)[[1]]
      find
      for(j in length(find):1)  {
        pos1 <- find[j]
        len <- attr(find,"match.length")[j]
        min <- min(seq[,j])
        max <- max(seq[,j])
        current <- paste(substring(current, 1, pos1),
                         substring(current, pos1+len-1, nchar(current)),
                         sep=ifelse(min==max,as.character(min), paste(min, max, sep = ",")))
      }
      patternList[tot, colname] <- as.character(currentSymbol)
      patternList[tot,"patterns"] <- current
      tot <- tot + 1

      seq <- list()
      count <- 1
      current <- pat
      currentSymbol <- patSymbol
      seq[[count]] <- getNumSeqs(current)
      count <- count + 1
    }
    setTxtProgressBar(pb, i)
  }
  patternList[tot, colname] <- as.character(currentSymbol)
  patternList[tot,"patterns"] <- current

  close(pb)

  as.data.frame(patternList)
}
