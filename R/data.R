#' Single Word Norms
#'
#' Single word norms
#'
#' @format A tibble of 10,758 words with 13 attributes
#' @source \url{https://doomlab.shinyapps.io/single_words/}
#'
"single_words_dic"

#' Mental Verbs
#'
#' list of mental verbs
#'
#' @format A list of 28 words
#'
"mental_verbs"

#' Education Words
#'
#' list of education words
#'
#' @format A list of 25 words
#'
"education_words"

#' Warmth and Competence Ratings
#'
#' list of words rated by judges on warmth and competence
#'
#' @format A list of 1711 words with two attributes
#'
"W_C_ratings"

#' Vignette Data
#'
#' Sample data from a study that can be used to test and explore the package. In this study, participants were asked to
#' present themselves in either a warm or competent manner. Then, three judges blind to participant condition coded
#' the introductions for warmth and competence.
#'
#' @format A dataframe with 393 rows and 11 columns
#'
"vignette_data"

#' Example Data
#'
#' 40 random bios from the vignette data. 20 bios were randomly selected from the competence condition and
#' 20 bios were randomly selected from the warmth condition.
#'
#' @format A dataframe with 40 rows and 11 columns
#'
"example_data"

#' Warmth Model
#'
#' A pre-trained enet regression model to assess warmth perceptions
#'
#'
"warmth_enet_final_reduced"

#' Competence Model
#'
#' A pre-trained enet regression model to assess competence perceptions
#'
#'
"competence_enet_final_reduced"
