#' Competence Detector
#'
#' @description Assesses competence perceptions in self-presentational natural language.
#'     This function is one of the main two functions of the \code{warmthcompetence} package.
#'     It takes an N-length vector of self-presentational text documents and N-length vector of document IDs and returns a competence perception score that represents how much competence
#'     others attribute the individual who wrote the self-presentational text.
#'     The function also contains a metrics argument that enables users to also return the raw features used to assess competence perceptions.
#' @param text character A vector of texts, each of which will be assessed for competence.
#' @param ID character A vector of IDs that will be used to identify the competence scores.
#' @param metrics character An argument that allows users to decide what metrics to return. Users can return the competence scores (metrics = "scores"),
#'     the features that underlie the competence scores (metrics = "features"), or both the competence scores and the features (metrics = "all).
#'     The default choice is to return the competence scores.
#' @details Some features depend Spacyr which must be installed separately in Python.
#' @returns The default is to return a data.frame with each row containing the document identifier and the competence score.
#'     Users can also customize what is returned through the metrics argument. If metrics = "features", then a dataframe of competence features will be
#'     returned where each document is represented by a row. If metrics = "all", then both the competence scores and features will be returned in a data.frame.
#'
#' @references
#' Benoit K, Watanabe K, Wang H, Nulty P, Obeng A, Müller S, Matsuo A (2018). “quanteda: An R package for the quantitative analysis of textual data.” Journal of Open Source Software, 3(30), 774. doi: 10.21105/joss.00774, https://quanteda.io.
#' Buchanan, E. M., Valentine, K. D., & Maxwell, N. P. (2018). LAB: Linguistic Annotated Bibliography - Shiny Application. Retrieved from http://aggieerin.com/shiny/lab_table.
#' Rinker, T. W. (2018). lexicon: Lexicon Data version 1.2.1. http://github.com/trinker/lexicon
#' Rinker, T. W. (2019). sentimentr: Calculate Text Polarity Sentiment version 2.7.1. http://github.com/trinker/sentimentr
#' Yeomans, M., Kantor, A. & Tingley, D. (2018). Detecting Politeness in Natural Language. The R Journal, 10(2), 489-502.
#'
#' @examples
#' data("example_data")
#'
#' competence_scores <- competence(example_data$bio, metrics = "all")
#'
#' example_data$competence_predictions <- competence_scores$competence_predictions
#' competence_model1 <- lm(RA_comp_AVG ~ competence_predictions, data = example_data)
#' summary(competence_model1)
#

#' @export
competence <- function(text, ID = NULL, metrics = "scores") {

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
  try <- spacyr::spacy_parse(tbl, tag = TRUE, dependency = TRUE, nounphrase = TRUE, entity = FALSE)
  tidy_norms_clean <- words_clean(text, ID)
  df_corpus <- quanteda::corpus(text, docnames = ID)

  df_dfm <- quanteda::dfm(
    quanteda::tokens(df_corpus),
    tolower = TRUE,
    select = NULL,
    remove = NULL,
    dictionary = NULL,
    thesaurus = NULL)


  #politeness
  df_politeness <- politeness::politeness(text, parser = "spacy",
                                          drop_blank = TRUE, metric = "average")

  df$First.Person.Single <- {
    if (is.null(df_politeness$First.Person.Single)) 0
    else df_politeness$First.Person.Single
  }

  df$Negative.Emotion <- {
    if (is.null(df_politeness$Negative.Emotion)) 0
    else df_politeness$Negative.Emotion
  }

  ## Word Lists by Tidy
  tidy_norms_clean$tone_neg_words <- 0
  tidy_norms_clean$Prevention_words <- 0
  tidy_norms_clean$forward_words <- 0

  bundle_1 <- c("as", "an", "while", "in")
  tidy_norms_clean$bundle_1 <- 0

  tidy_norms_clean$tone_neg_words[tolower(tidy_norms_clean$word) %in% tone_neg_words$WORDS] <- 1
  tidy_norms_clean$Prevention_words[tolower(tidy_norms_clean$word) %in% Prevention_words$WORDS] <- 1
  tidy_norms_clean$forward_words[tolower(tidy_norms_clean$word) %in% forward_words$WORDS] <- 1
  tidy_norms_clean$bundle_1[tolower(tidy_norms_clean$word) %in% bundle_1] <- 1

  tidy_words_scores <- dplyr::summarize(
    tidy_norms_clean,
    tone_neg_words = sum(.data$tone_neg_words),
    Prevention_words = sum(.data$Prevention_words),
    forward_words = sum(.data$forward_words),
    bundle_1C = sum(.data$bundle_1) / nrow(tidy_norms_clean),
    .by = .data$ID
  )

  df <- dplyr::left_join(df, tidy_words_scores, by = c("ID" = "ID"))

  ##purposefully not controlling prevention by WC
  df$tone_neg_words <- df$tone_neg_words / df$WC
  df$forward_words <- df$forward_words / df$WC

  spacy_counts2 <- dplyr::summarize(
    try,
    advmod = sum(.data$dep_rel == 'advmod'),
    punct = sum(.data$dep_rel == 'punct'),
    TO = sum(.data$tag == 'TO'),
    ADP = sum(.data$pos == 'ADP'),
    ROOT = sum(.data$dep_rel == 'ROOT'),
    agency_target2 = sum(.data$tag == 'PRP' & .data$dep_rel %in% c('nsubj', 'csubj', 'csubjpass', 'nsubjpass')) / sum(.data$tag == 'PRP'),
    agency_target200 = sum(.data$token %in% c('i', 'i\'m', 'me', 'mine') &
                             .data$dep_rel %in% c('nsubj', 'csubj', 'csubjpass', 'nsubjpass')) / sum(.data$tag == 'PRP'),
    .by = .data$doc_id
  )

  df <- dplyr::left_join(df, spacy_counts2, by = c("ID" = "doc_id"))
  df$advmod <- df$advmod / df$WC
  df$punct <- df$punct / df$WC
  df$TO <- df$TO / df$WC
  df$ADP <- df$ADP/ df$WC
  df$ROOT <- df$ROOT / df$WC

  #!
  #sentence level spacy features
  # suppressWarnings(spacy_new2A <- plyr::ddply(try, .(doc_id, sentence_id), plyr::summarize,
  #                                             post_JJS_ADJ2_subj = (length(token_id[tag == 'JJS' & token_id > token_id[dep_rel == "nsubj"]])/ length(token_id)),
  #                                             pre_NOUN2_ROOT = (length(token_id[pos == 'NOUN' & token_id < token_id[dep_rel == "ROOT"]])/ length(token_id))
  # ))

  ## Need to address warnings
  ## Use `tt <- try[try$doc_id == "1" & try$sentence_id == 4,]` to examine
  spacy_new2A <- dplyr::summarize(
    try,
    post_JJS_ADJ2_subj = sum(.data$tag == 'JJS' & .data$token_id > .data$token_id[.data$dep_rel == "nsubj"]) / dplyr::n(),
    pre_NOUN2_ROOT = sum(.data$pos == 'NOUN' & .data$token_id < .data$token_id[.data$dep_rel == "ROOT"]) / dplyr::n(),
    .by = c(.data$doc_id, .data$sentence_id)
  )

  spacy_new2B <- dplyr::summarize(
    spacy_new2A,
    dplyr::across(.data$post_JJS_ADJ2_subj:.data$pre_NOUN2_ROOT, mean, na.rm = TRUE),
    .by = .data$doc_id
  )

  df <- dplyr::left_join(df, spacy_new2B, by = c("ID" = "doc_id"))

  ##Competence Codings
  W_C_df <- dplyr::inner_join(tidy_norms_clean,
                              W_C_ratings,
                              by = c("word" = "Word"),
                              relationship = "many-to-many")

  Positive_Comp <- W_C_df[W_C_df[['Competence Rating']] == '1',]
  names(Positive_Comp)[names(Positive_Comp) == 'Competence Rating'] <- 'Competence_Rating'

  Positive_Comp_Scores <- dplyr::summarize(
    Positive_Comp,
    Positive_Comp = sum(.data$Competence_Rating, na.rm = TRUE),
    .by = .data$ID
  )

  df <- dplyr::left_join(df, Positive_Comp_Scores, by = c("ID" = "ID"))
  df$Positive_Comp <- df$Positive_Comp / df$WC

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

  df <- dplyr::left_join(df, emotion_wide[c("ID", "surprise_negated", "anger_difference")],
                         by = c("ID" = "ID"))

  ##Norms
  AoA_df <- dplyr::inner_join(tidy_norms_clean, AoA_dic,
                              by = c("word" = "Symbol"))

  AoA_scores <- dplyr::summarize(
    AoA_df,
    AoA_Rating = sum(.data$AoA_Rating, na.rm = TRUE),
    .by = .data$ID
  )

  df <- dplyr::left_join(df, AoA_scores,
                         by = c("ID" = "ID"))
  df$AoA_Rating <- df$AoA_Rating / df$WC

  single_df <- dplyr::inner_join(tidy_norms_clean, single_words_dic,
                                 by = c("word" = "Symbol"))

  single_scores <- dplyr::summarize(
    single_df,
    Ortho = sum(.data$Ortho, na.rm = TRUE),
    .by = .data$ID
  )

  df <- dplyr::left_join(df, single_scores,
                         by = c("ID" = "ID"))
  df$Ortho <- df$Ortho / df$WC

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

  ##Discourse Markers
  vague <- qdapDictionaries::discourse.markers.alemany$marker[qdapDictionaries::discourse.markers.alemany$type == "vague"]
  tidy_norms_clean$vague <- ifelse(tidy_norms_clean$word %in% vague, 1, 0)

  discourse_scores <- dplyr::summarize(
    tidy_norms_clean,
    vague = sum(.data$vague, na.rm = TRUE),
    .by = .data$ID
  )

  ref <- data.frame(ID = df$ID,
                    vague2 = 0)

  vague_splits <- strsplit(vague, "\\s+")
  for (j in which(lengths(vague_splits) > 1)) {
    for (i in seq_len(nrow(df))) {
      if (grepl(vague[j], tolower(df$text[i]), ignore.case = TRUE))
        ref$vague2[i] <- ref$vague2[i] + 1
    }
  }

  discourse_scores <- dplyr::inner_join(ref, discourse_scores,
                                        by = c("ID" = "ID"))
  discourse_scores$vague_ALL <- discourse_scores$vague + discourse_scores$vague2
  discourse_scores_short <- discourse_scores[c("ID",  "vague_ALL")]
  df <- dplyr::left_join(df, discourse_scores_short,
                         by = c("ID" = "ID"))
  df$vague_ALL <- df$vague_ALL / df$WC

  #Power
  temp_power <- qdapDictionaries::key.power
  names(temp_power)[2] <- "y_power"
  key_power <- dplyr::inner_join(tidy_norms_clean, temp_power,
                                 by = c("word" = "x"))

  key_power <- dplyr::summarize(
    key_power,
    key_power = sum(.data$y_power, na.rm = TRUE),
    .by = c(.data$ID, .data$y_power)
  )
  positive_power <- key_power[key_power$y_power == '1', c("ID", "key_power")]
  names(positive_power)[2] <- "positive_power"

  df <- dplyr::left_join(df, positive_power,
                         by = c("ID" = "ID"))
  df$positive_power <- df$positive_power / df$WC

  # Sentiment
  sentiment <-  suppressWarnings(sentimentr::sentiment_by(df$text))
  df$ave_sentiment <- sentiment$ave_sentiment

  #Readability
  readability <- quanteda.textstats::textstat_readability(df_corpus, measure = c("Coleman.C2", "Fucks"),
                                                          remove_hyphens = TRUE, intermediate = FALSE)

  df <- dplyr::left_join(df, readability,
                         by = c("ID" = "document"))

  # Lexical Diversity
  df_short <- df[grepl("[[:alpha:]]", df$text),]
  diversity <- qdap::diversity(df_short$text, grouping.var = df_short$ID)
  shannon <- diversity[c("ID", "shannon")]
  df <- dplyr::left_join(df, shannon,
                         by = c("ID" = "ID"))

  lexdiv <- quanteda.textstats::textstat_lexdiv(df_dfm,
                                                measure = "D",
                                                remove_numbers = TRUE,
                                                remove_punct = TRUE,
                                                remove_symbols = TRUE,
                                                remove_hyphens = FALSE,
                                                log.base = 10,
                                                MATTR_window = 100L,
                                                MSTTR_segment = 100L)
  df <- dplyr::left_join(df, lexdiv,
                         by = c("ID" = "document"))

  #TFIDF-based Bundle
  message_dfm_tfidf<- quanteda::dfm_tfidf(df_dfm)
  bundle_2 <- c("years", "year", "name")
  feat <- quanteda::dfm_select(message_dfm_tfidf, bundle_2)
  feat2 <- quanteda::convert(feat, to = 'data.frame')

  for (i in c("years", "year", "name")) {
    if (!i %in% names(feat2)) feat2[[i]] <- 0
  }

  feat2$bundle_2 <- rowMeans(feat2[bundle_2])
  df <- dplyr::left_join(df, feat2[c("doc_id", "bundle_2")],
                         by = c('ID' = 'doc_id'))


  # Running pre-trained model
  competence_features <- df[c("tone_neg_words","First.Person.Single","agency_target2",
                              "Prevention_words","advmod","surprise_negated","punct",
                              "AoA_Rating","post_JJS_ADJ2_subj","pre_NOUN2_ROOT","TO",
                              "happiness_rank","ADP","Ortho","vague_ALL","forward_words",
                              "Positive_Comp","positive_power","agency_target200",
                              "ave_sentiment","anger_difference","ROOT","Coleman.C2",
                              "Negative.Emotion","D","shannon","Fucks","bundle_1C",
                              "bundle_2")]

  competence_features <- raster::as.matrix(competence_features)
  competence_features[!is.finite(competence_features)] <- 0

  suppressWarnings(preprocessParams1 <- caret::preProcess(competence_features, method = c("center", "scale")))
  competence_features1 <- stats::predict(preprocessParams1, competence_features)

  out <- data.frame(ID = df$ID)

  if (metrics %in% c("scores", "all")) {
    out$competence_predictions <- raster::predict(competence_enet_model, competence_features1)
  }

  if (metrics %in% c("features", "all")) {
    colnames(competence_features1) <- c("Negative_Tone","First_Person_Singular_Pronouns",
                                        "Personal_Pronouns_as_Subjects","Prevention_Words",
                                        "Adverbial_Modifiers","Negated_Surprise","Punctuation",
                                        "Word_Complexity","Superlative_Adjectives_After_Subject",
                                        "Nouns_Before_Main_Verb","Infinitival_To",
                                        "Happiness_Emotion","Adpositions","Orthographic_Neighborhood",
                                        "Vague_Discourse_Markers","Future_Oriented_Language",
                                        "Competence_Language",
                                        "Power_Language","First_Person_as_Subject","Average_Sentiment",
                                        "Anger_Emotion","Sentence_Length","Readability_Measure1",
                                        "Negative_Emotion","Lexical_Diversity_Measure1",
                                        "Lexical_Diversity_Measure2","Readability_Measure2","Adverbs",
                                        "Simple_Introductions")

    out <- cbind(out, as.data.frame(competence_features1))
  }

  out
}
