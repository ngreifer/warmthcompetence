#' A function to format text
#'
#' @description Contains functions that are used by the main functions of the \pkg{warmthcompetence} package for text processing.
#' @param text character A vector of texts
#' @param ID character A vector of IDs
#' @details Some features depend on Spacyr which must be installed separately in Python.
#' @return Tibbles that are used by the main functions of the \pkg{warmthcompetence} package
#' @keywords internal
#'
words_clean <- function(text, ID) {

  allData <- dplyr::tibble(ID)

  allData$text_clean <- qdap::clean(text) |>
    qdap::replace_contraction() |>
    qdap::replace_abbreviation() |>
    qdap::replace_symbol() |>
    tm::removePunctuation() |>
    tm::stripWhitespace() |>
    tolower()

  tidytext::unnest_tokens(allData[c("text_clean", "ID")],
                          output = "word",
                          input = .data$text_clean,
                          to_lower = FALSE)
}
