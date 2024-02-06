#' A function to format text
#'
#' @description Contains functions that are used by the main functions of the \code{warmthcompetence} package for text processing.
#' @param text character A vector of texts
#' @param ID character A vector of IDs
#' @details Some features depend Spacyr which must be installed seperately in Python.
#' @return Tibbles that are used by the main functions of the \code{warmthcompetence} package
#' @keywords internal
#'
words_clean <- function(text, ID) {

  allData <- dplyr::tibble(ID)
  allData$text_clean <- tm::stripWhitespace(tm::removePunctuation(qdap::replace_symbol(qdap::replace_abbreviation(qdap::replace_contraction(qdap::clean(text))))))
  allData$text_clean <- tolower(allData$text_clean)

  tidytext::unnest_tokens(allData[c("text_clean", "ID")],
                          output = "word",
                          input = .data$text_clean,
                          to_lower = FALSE)
}
