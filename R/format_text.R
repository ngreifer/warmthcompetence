#' A function to format text
#'
#' @description Contains functions that are used by the main functions of the \code{warmthcompetence} package for text processing.
#' @import plyr, magrittr
#' @param text character A vector of texts
#' @param ID character A vector of IDs
#' @details Some features depend Spacyr which must be installed seperately in Python.
#' @return Tibbles that are used by the main functions of the \code{warmthcompetence} package


#mit <- readxl::read_excel("data/MIT Interview Transcriptions 20200625.xlsx", sheet = 'Keeping')


# Pulling out the words after cleaning
words_clean <- function(text, ID){
  allData <- tibble::tibble(text, ID)
  allData$text_clean <- tm::stripWhitespace(tm::removePunctuation(qdap::replace_symbol(qdap::replace_abbreviation(qdap::replace_contraction(qdap::clean(text))))))
  tidy_norms_clean <- allData %>% dplyr::select(text_clean, ID)  %>%
    tidytext::unnest_tokens("word", text_clean, to_lower = FALSE)
  return(tidy_norms_clean)
}

##Creating spacy tables
spacy_tbl <- function(text, ID){
  tbl <- tibble::as_tibble(
    data.frame(doc_id = ID, text = tolower(text), stringsAsFactors = F))
  spacy_tbl <- spacyr::spacy_parse(tbl, tag = TRUE, dependency = TRUE, nounphrase = TRUE)
  return(spacy_tbl)
}

