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
#' list of mental verbs (author developed)
#'
#' @format A list of 28 words
#'
"mental_verbs"

#' Exploration Words
#'
#' list of exploration words
#'
#' @format A list of 9 words
#' @source Uotila et al. (2009)
#'
"explore_words"

#' Competence Adjectives
#'
#' list of competence adjectives
#'
#' @format A list of 44 words
#' @source Payne et al. (2011)
#'
"Courage_words"

#' Warmth Orientation
#'
#' list of words that suggest warmth orientation
#'
#' @format A list of 20 words
#' @source Payne et al. (2011)
#'
"Warmth_words"

#' Finance words
#'
#' list of words related to finance
#'
#' @format A list of 137 words
#' @source Matsumoto et al. (2011)
#'
"finance_words"

#' Employment language
#'
#' list of words related to employment (Author Developed)
#'
#' @format A list of 23 words
#'
"Employ_words"

#' Social Orientation
#'
#' list of words that suggest social orientation
#'
#' @format A list of 66 words
#' @source Moss et al. (2016)
#'
"social_words"

#' Psycholingustic Aspects
#'
#' list of words assessed for familiarity, AoA, concreteness and imagery
#'
#' @format A list of 85,942 words with four attributes
#' @source Paetzold & Specia (2016)
#'
"psy_ling_dic"

#' Single Word Norms
#'
#' list of words assessed for valence, arousal, dominance, concreteness and imagery
#'
#' @format A list of 17,350 words with four attributes
#' @source Bestgen & Vincze (2012)
#'
"norms_dic"

#' Language Complexity
#'
#' list of words assessed for AoA and Dunno
#'
#' @format A list of 31,105 words with two attributes
#' @source Kuperman et al. (2012)
#'
"AoA_dic"

#' Negative Tone
#'
#' list of words related to negative tone
#'
#' @format A list of 85 words
#' @source Henry (2008)
#'
"tone_neg_words"

#' Prevention words
#'
#' list of words associated with prevention
#'
#' @format A list of 25 words
#' @source Gamache et al. (2014)
#'
"Prevention_words"


#' Future Orientated Language
#'
#' list of words associated with future orientation
#'
#' @format A list of 34 words
#' @source Matsumoto et al. (2011)
#'
"forward_words"


#' Warmth and Competence Ratings
#'
#' list of words rated by judges on warmth and competence (Author Developed)
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
#' A pre-trained enet regression model to assess warmth perceptions using 29 features
#'
#'
"warmth_enet_model"

#' Competence Model
#'
#' A pre-trained enet regression model to assess competence perceptions using 29 features
#'
#'
"competence_enet_model"
