#' Warmth Detector
#'
#' @description Assesses warmth perceptions in self-presentational natural language.
#'     This function is one of the main two functions of the  \code{warmthcompetence} package.
#'     It takes an N-length vector of self-presentational text documents and N-length vector of document IDs and returns a warmth perception score that represents how much warmth
#'     others attribute the individual who wrote the self-presentational text.
#'     The function also contains a metrics argument that enables users to also return the raw features used to assess warmth perceptions.
#' @param text character A vector of texts, each of which will be assessed for warmth.
#' @param ID character A vector of IDs that will be used to identify the warmth scores.
#' @param metrics character An argument that allows users to decide what metrics to return. Users can return the warmth scores (metrics = "scores"),
#'     the features that underlie the warmth scores (metrics = "features"), or both the warmth scores and the features (metrics = "all).
#'     The default choice is to return the warmth scores.
#' @details Some features depend Spacyr which must be installed seperately in Python.
#' @returns The default is to return a data.frame with each row containing the document identifier and the warmth score.
#'     Users can also customize what is returned through the metrics argument. If metrics = "features", then a dataframe of warmth features will be
#'     returned where each document is represented by a row. If metrics = "all", then both the warmth scores and features will be returned in a data.frame.
#'
#' @references
#' Benoit K, Watanabe K, Wang H, Nulty P, Obeng A, Müller S, Matsuo A (2018). “quanteda: An R package for the quantitative analysis of textual data.” Journal of Open Source Software, 3(30), 774. doi: 10.21105/joss.00774, https://quanteda.io.
#' Buchanan, E. M., Valentine, K. D., & Maxwell, N. P. (2018). LAB: Linguistic Annotated Bibliography - Shiny Application. Retrieved from http://aggieerin.com/shiny/lab_table.
#' Rinker, T. W. (2018). lexicon: Lexicon Data version 1.2.1. http://github.com/trinker/lexicon
#' Rinker, T. W. (2019). sentimentr: Calculate Text Polarity Sentiment version 2.7.1. http://github.com/trinker/sentimentr
#' Yeomans, M., Kantor, A. & Tingley, D. (2018). Detecting Politeness in Natural Language. The R Journal, 10(2), 489-502.
#'
#'
#'@examples
#'data("example_data")
#'
#'warmth_scores <- warmth(example_data$bio, metrics = "all")
#'
#'example_data$warmth_predictions <- warmth_scores$warmth_predictions
#'warmth_model1 <- lm(RA_warm_AVG  ~ warmth_predictions, data = example_data)
#'summary(warmth_model1)
#'
#'
#'@export
warmth <- function(text, ID = NULL, metrics = "scores") {

  if (!is.character(text)) {
    stop("`text` must be a character vector of texts.", call. = FALSE)
  }

  if (!all(grepl("[[:alpha:]]", text))) {
    stop("Some entries in your dataset do not contain any words. Please remove those entries and try again.",
         call. = FALSE)
  }

  if (!is.character(metrics) || length(metrics) != 1) {
    stop("`metrics` must be a string.", call. = FALSE)
  }
  metrics <- tolower(metrics)
  metrics <- match.arg(metrics, c("scores", "features", "all"))

  if (is.null(ID)) {
    ID <- as.character(seq_along(text))
  }
  else {
    ID <- as.character(ID)
  }

  #word count and text objects
  df <- data.frame(text, ID)
  df$WC <- vapply(df$text, ngram::wordcount, numeric(1L))
  tbl <- dplyr::tibble(doc_id = ID, text = text)
  try <- spacyr::spacy_parse(tbl, tag = TRUE, dependency = TRUE, nounphrase = TRUE,
                             entity = FALSE)
  tidy_norms_clean <- words_clean(text, ID)
  df_corpus <- quanteda::corpus(text, docnames = ID)

  ## Word Lists by Tidy
  tidy_norms_clean$Courage_words <- 0
  tidy_norms_clean$Warmth_words <- 0
  tidy_norms_clean$finance_words <- 0
  tidy_norms_clean$strong_words <- 0

  tidy_norms_clean$Courage_words[tolower(tidy_norms_clean$word) %in% Courage_words$WORDS] <- 1
  tidy_norms_clean$Warmth_words[tolower(tidy_norms_clean$word) %in% Warmth_words$WORDS] <- 1
  tidy_norms_clean$finance_words[tolower(tidy_norms_clean$word) %in% finance_words$WORDS] <- 1
  tidy_norms_clean$strong_words[tolower(tidy_norms_clean$word) %in% qdapDictionaries::strong.words] <- 1

  tidy_words_scores <- dplyr::summarize(
    tidy_norms_clean,
    Courage_words = sum(.data$Courage_words, na.rm = TRUE),
    Warmth_words = sum(.data$Warmth_words, na.rm = TRUE),
    finance_words = sum(.data$finance_words, na.rm = TRUE),
    strong_words = sum(.data$strong_words, na.rm = TRUE),
    .by = .data$ID
  )

  df <- dplyr::left_join(df, tidy_words_scores,
                         by = c("ID" = "ID"))
  df$Courage_words <- df$Courage_words / df$WC
  df$Warmth_words <- df$Warmth_words / df$WC
  df$finance_words <- df$finance_words / df$WC
  df$strong_words <- df$strong_words / df$WC

  ## Word List by chunk
  df$explore_words <- 0
  df$employ_words <- 0
  df$social_words <- 0
  df$mental_verbs <- 0

  for (i in seq_len(nrow(df))) {
    for (j in explore_words$WORDS) {
      if (grepl(j, tolower(df$text[i]), ignore.case = TRUE, perl = TRUE))
        df$explore_words[i] <- df$explore_words[i] + 1
    }

    for (j in employ_words$WORDS) {
      if (grepl(j, tolower(df$text[i]), ignore.case = TRUE, perl = TRUE))
        df$employ_words[i] <- df$employ_words[i] + 1
    }

    for (j in social_words$WORDS) {
      if (grepl(j, tolower(df$text[i]), perl = TRUE, ignore.case = TRUE))
        df$social_words[i] <- df$social_words[i] + 1
    }

    for (j in mental_verbs$WORDS) {
      if (grepl(j, tolower(df$text[i]), ignore.case = TRUE, perl = TRUE))
        df$mental_verbs[i] <- df$mental_verbs[i] + 1
    }
  }

  df$explore_words <- df$explore_words / df$WC
  df$employ_words <- df$employ_words / df$WC
  df$social_words <- df$social_words / df$WC
  df$mental_verbs <- df$mental_verbs / df$WC

  #Psycholingustic features
  psy_ling_df <- dplyr::inner_join(tidy_norms_clean, psy_ling_dic,
                                   by = c("word" = "Symbol"))

  psy_ling_scores <- dplyr::summarize(
    psy_ling_df,
    AoA = sum(.data$AoA, na.rm = TRUE),
    Image = sum(.data$Imagery, na.rm = TRUE),
    .by = .data$ID
  )

  df <- dplyr::left_join(df, psy_ling_scores,
                         by = c("ID" = "ID"))
  df$AoA <- df$AoA / df$WC
  df$Image <- df$Image / df$WC


  ##Discourse Markers
  polysemic <- qdapDictionaries::discourse.markers.alemany$marker[qdapDictionaries::discourse.markers.alemany$type == "polysemic"]
  tidy_norms_clean$polysemic <- ifelse(tidy_norms_clean$word %in% polysemic, 1, 0)

  discourse_scores <- dplyr::summarize(
    tidy_norms_clean,
    polysemic = sum(.data$polysemic, na.rm = TRUE),
    .by = .data$ID
  )

  ref <- data.frame(ID = df$ID,
                    polysemic2 = 0)

  polysemic_splits <- strsplit(polysemic, "\\s+")
  for (j in which(lengths(polysemic_splits) > 1)) {
    for (i in seq_len(nrow(df))) {
      if (grepl(polysemic[j], tolower(df$text[i]), ignore.case = TRUE))
        ref$polysemic2[i] <- ref$polysemic2[i] + 1
    }
  }

  discourse_scores <- dplyr::inner_join(ref, discourse_scores,
                                        by = c("ID" = "ID"))
  discourse_scores$polysemic_ALL <- discourse_scores$polysemic + discourse_scores$polysemic2
  discourse_scores_short <- discourse_scores[c("ID",  "polysemic_ALL")]
  df <- dplyr::left_join(df, discourse_scores_short,
                         by = c("ID" = "ID"))
  df$polysemic_ALL <- df$polysemic_ALL / df$WC

  ##LabMT
  labMT_values <- dplyr::inner_join(tidy_norms_clean, qdapDictionaries::labMT,
                                    by = c("word" = "word"))

  labMT_values <- dplyr::summarize(
    labMT_values,
    happiness_rank = sum(.data$happiness_rank, na.rm = TRUE),
    .by = .data$ID
  )

  df <- dplyr::left_join(df, labMT_values,
                         by = c("ID" = "ID"))
  df$happiness_rank <- df$happiness_rank / df$WC


  #Readability
  readability <- quanteda.textstats::textstat_readability(df_corpus, measure = "Linsear.Write")

  df <- dplyr::left_join(df, readability,
                         by = c("ID" = "document"))

  #politeness
  df_politeness <- politeness::politeness(df$text, parser = "spacy",
                                          drop_blank = FALSE,
                                          metric = "average")
  df$Swearing <- {
    if (is.null(df_politeness$Swearing)) 0
    else df_politeness$Swearing
  }

  df$Hello.x <- {
    if (is.null(df_politeness$Hello)) 0
    else df_politeness$Hello
  }

  spacy_new2A <- dplyr::summarize(
    try,
    pre_UH_adv2_subj = sum(.data$tag == 'UH' & .data$token_id < .data$token_id[.data$dep_rel == "nsubj"][1]) / dplyr::n(),
    post_PRP_adv1_subj = sum(.data$tag == 'PRP' & .data$token_id > .data$token_id[.data$dep_rel == "nsubj"][1]) / sum(.data$tag == 'PRP'),
    post_adj1_ROOT = sum(.data$pos == 'ADJ' & .data$token_id > .data$token_id[.data$dep_rel == "ROOT"][1]) / sum(.data$pos == 'ADJ'),
    post_NNS_NOUN1_subj = sum(.data$tag == 'NNS' & .data$token_id > .data$token_id[.data$dep_rel == "nsubj"][1]) / sum(.data$pos == 'NOUN'),
    VB_VERB = sum(.data$tag == 'VB') / sum(.data$pos == 'VERB'),
    .by = c(.data$doc_id, .data$sentence_id)
  )

  spacy_new2B <- dplyr::summarize(
    spacy_new2A,
    dplyr::across(.data$pre_UH_adv2_subj:.data$VB_VERB, mean, na.rm = TRUE),
    .by = .data$doc_id
  )

  df <- dplyr::left_join(df, spacy_new2B,
                         by = c("ID" = "doc_id"))

  #message level spacy features
  spacy_counts2 <- dplyr::summarize(
    try,
    poss = sum(.data$dep_rel == 'poss'),
    VBG = sum(.data$tag == 'VBG'),
    .by = .data$doc_id
  )

  df <- dplyr::left_join(df, spacy_counts2,
                         by = c("ID" = "doc_id"))
  df$poss <- df$poss / df$WC
  df$VBG.x <- df$VBG / df$WC

  # Emotion
  emotion <- suppressWarnings(sentimentr::emotion_by(df$text, emotion_dt = lexicon::hash_nrc_emotions,
                                                      valence_shifters_dt = lexicon::hash_valence_shifters,
                                                      drop.unused.emotions = FALSE, un.as.negation = TRUE,
                                                      n.before = 5, n.after = 2))
  emotion$ID <- ID[emotion$element_id]

  emotion_wide <- tidyr::pivot_wider(emotion[c("ID", "emotion_type", "ave_emotion")],
                                     names_from = .data$emotion_type,
                                     values_from = .data$ave_emotion)

  emotion_wide$anger_difference <- emotion_wide$anger - emotion_wide$anger_negated
  emotion_wide$joy_difference <- emotion_wide$joy - emotion_wide$joy

  df <- dplyr::left_join(df, emotion_wide[c("ID", "disgust_negated", "anger_difference", "joy_difference")],
                         by = c("ID" = "ID"))

  #Norms
  single_df <- dplyr::inner_join(tidy_norms_clean, single_words_dic,
                                 by = c("word" = "Symbol"))

  single_scores <- dplyr::summarize(
    single_df,
    HAL = sum(.data$HAL, na.rm = TRUE),
    .by = .data$ID
  )

  df <- dplyr::left_join(df, single_scores,
                         by = c("ID" = "ID"))

  norms_dic <- norms_dic[c("Symbol", "Concreteness")]
  norms_df <- dplyr::inner_join(tidy_norms_clean, norms_dic,
                                by = c("word" = "Symbol"))

  norms_scores <- dplyr::summarize(
    norms_df,
    Concreteness = sum(.data$Concreteness,  na.rm = TRUE),
    .by = .data$ID
  )

  df <- dplyr::left_join(df, norms_scores,
                         by = c("ID" = "ID"))
  df$Concreteness <- df$Concreteness / df$WC

  ##Warmth Codings
  W_C_df <- dplyr::inner_join(tidy_norms_clean,
                              W_C_ratings,
                              by = c("word" = "Word"),
                              relationship = "many-to-many")

  Positive_Warm <- W_C_df[W_C_df[['Warmth Rating']] == '1',]
  names(Positive_Warm)[names(Positive_Warm) == 'Warmth Rating'] <- 'Warmth_Rating'

  Positive_Warm_Scores <- dplyr::summarize(
    Positive_Warm,
    Positive_Warm = sum(.data$Warmth_Rating, na.rm = TRUE),
    .by = .data$ID
  )

  df <- dplyr::left_join(df, Positive_Warm_Scores,
                         by = c("ID" = "ID"))
  df$Positive_Warm <- df$Positive_Warm / df$WC

  # diversity index
  df_short <- df[grepl("[[:alpha:]]", df$text),]
  diversity <- qdap::diversity(df_short$text, grouping.var = df_short$ID)
  collision <- diversity[c("ID", "collision")]
  df <- dplyr::left_join(df, collision,
                         by = c("ID" = "ID"))


  # Running pre-trained model
  warmth_features <- df[c("Courage_words", "explore_words",
                          "polysemic_ALL", "Warmth_words",
                           "Linsear.Write", "Swearing",
                           "pre_UH_adv2_subj", "post_PRP_adv1_subj", "post_adj1_ROOT",
                           "finance_words", "VB_VERB", "anger_difference",
                           "employ_words", "happiness_rank", "social_words",
                           "strong_words", "mental_verbs", "Image",
                           "post_NNS_NOUN1_subj", "HAL", "Hello.x", "poss",
                           "Positive_Warm", "VBG.x", "disgust_negated",
                           "Concreteness", "joy_difference", "AoA", "collision")]

  for (i in seq_along(warmth_features)) {
    warmth_features[[i]][!is.finite(warmth_features[[i]])] <- 0
  }

  suppressWarnings(preprocessParams1 <- caret::preProcess(warmth_features, method = c("center", "scale")))
  warmth_features1 <- stats::predict(preprocessParams1, newdata = warmth_features)

  out <- data.frame(ID = df$ID)

  if (metrics %in% c("scores", "all")) {
    out$warmth_predictions <- raster::predict(warmth_enet_model, warmth_features1)
  }

  if (metrics %in% c("features", "all")) {
    colnames(warmth_features1) <- c("Competence_Adjectives", "Exploration_Words",
                                    "Polysemic_Discourse_Markers", "Warmth_Orientation",
                                    "Readability", "Swearing",
                                    "Interjections_Before_Subject",
                                    "Personal_Pronouns_After_Subject",
                                    "Adjectives_After_Main_Verb", "Finance_Words",
                                    "Base_Form_Verbs", "Anger_Emotion",
                                    "Employment_Language", "Happiness_Emotion",
                                    "Social_Orientation", "Strength_Language",
                                    "Mental_Verbs", "Imagery",
                                    "Plural_Nouns_After_Subject", "Free_Association_Norms",
                                    "Hello", "Possession_Modifers", "Warmth_Language",
                                    "Present_Participle_Verbs", "Negated_Disgust",
                                    "Concreteness", "Joy_Emotion", "Word_Complexity",
                                    "Lexical_Diversity")

    out <- cbind(out, as.data.frame(warmth_features1))
  }

  out
}
