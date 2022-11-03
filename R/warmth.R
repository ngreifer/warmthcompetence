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
#' @return The default is to return a data.frame with each row containing the document identifier and the warmth score.
#'     Users can also customize what is returned through the metrics argument. If metrics = "features", then a dataframe of warmth features will be
#'     returned where each document is represented by a row. If metrics = "all", then both the warmth scores and features will be returned in a data.frame.
#'
#' @importFrom plyr .
#' @importFrom magrittr %>%
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
warmth <- function(text, ID=NULL, metrics = c("scores", "features", "all")){
  if(is.null(ID)){
    ID=as.character(1:length(text))
  }
  #For CRAN check


  #word count and text objects
  df <- data.frame(text, ID)
  df$WC <- apply(df %>% dplyr::select(text), 1, ngram::wordcount)
  tbl <- tibble::as_tibble(
    data.frame(doc_id = ID, text = text, stringsAsFactors = F)
  )
  try <- spacyr::spacy_parse(tbl, tag = TRUE, dependency = TRUE, nounphrase = TRUE,entity=FALSE)
  tidy_norms_clean <- words_clean(text, ID)

  df_corpus <- quanteda::corpus(df$text, docnames = df$ID)

  ## Word Lists by Tidy
  tidy_norms_clean$Courage_words <- 0
  tidy_norms_clean$Warmth_words <- 0
  tidy_norms_clean$finance_words <- 0
  tidy_norms_clean$strong_words <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tolower(tidy_norms_clean$word[i]) %in% Courage_words$WORDS)
      tidy_norms_clean$Courage_words[i] =  1
    if (tolower(tidy_norms_clean$word[i]) %in% Warmth_words$WORDS)
      tidy_norms_clean$Warmth_words[i] =  1
    if (tolower(tidy_norms_clean$word[i]) %in% finance_words$WORDS)
      tidy_norms_clean$finance_words[i] =  1
    if (tolower(tidy_norms_clean$word[i]) %in% qdapDictionaries::strong.words)
      tidy_norms_clean$strong_words[i] =  1}
  tidy_words_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                                   Courage_words = sum(Courage_words, na.rm = TRUE),
                                   Warmth_words = sum(Warmth_words, na.rm = TRUE),
                                   finance_words = sum(finance_words, na.rm = TRUE),
                                   strong_words = sum(strong_words, na.rm = TRUE))
  df <- dplyr::left_join(df, tidy_words_scores, by = c("ID" = "ID"))
  df$Courage_words <- df$Courage_words/ df$WC
  df$Warmth_words <- df$Warmth_words/ df$WC
  df$finance_words <- df$finance_words/ df$WC
  df$strong_words <- df$strong_words/ df$WC

  ## Word List by chunk
  df$explore_words <- 0
  df$employ_words <- 0
  df$social_words <- 0
  df$mental_verbs <- 0
  for (i in 1:nrow(df)) {
    for (j in 1:length(explore_words$WORDS)) {
      if (grepl(explore_words$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE))
      {(df$explore_words[i] <- df$explore_words[i] + 1)}}
    for (f in 1:length(employ_words$WORDS)) {
      if (grepl(employ_words$WORDS[f], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE))
      {(df$employ_words[i] <- df$employ_words[i] + 1)}}
    for (j in 1:length(social_words$WORDS)) {
      if (grepl(social_words$WORDS[j], tolower(df$text[i]), perl = TRUE, ignore.case = TRUE))
        (df$social_words[i] <- df$social_words[i] + 1)}
    for (j in 1:length(mental_verbs$WORDS)) {
      if (grepl(mental_verbs$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE))
      {(df$mental_verbs[i] <- df$mental_verbs[i] + 1)}
    }}
  df$explore_words <- df$explore_words/ df$WC
  df$employ_words <- df$employ_words / df$WC
  df$social_words <- df$social_words / df$WC
  df$mental_verbs <- df$mental_verbs / df$WC

  #Psycholingustic features
  psy_ling_df <- dplyr::inner_join(tidy_norms_clean, psy_ling_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  psy_ling_scores <- plyr::ddply(psy_ling_df,.(ID),plyr::summarize,
                                 AoA = sum(AoA, na.rm = TRUE),
                                 Image = sum(Imagery, na.rm = TRUE))
  df <- dplyr::left_join(df, psy_ling_scores, by = c("ID" = "ID"))
  df$AoA <- df$AoA/ df$WC
  df$Image <- df$Image/ df$WC


  ##Discourse Markers
  polysemic <- qdapDictionaries::discourse.markers.alemany$marker[which(qdapDictionaries::discourse.markers.alemany$type == "polysemic")]
  tidy_norms_clean$polysemic <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tidy_norms_clean$word[i] %in% polysemic)
    {tidy_norms_clean$polysemic[i] =  1}
  }
  discourse_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                                  polysemic = sum(polysemic, na.rm = TRUE))
  ref <- as.data.frame(df$ID)
  names(ref)[names(ref) == 'df$ID'] <- 'ID'
  ref$polysemic2 <- 0
  for (j in 1:length(polysemic)) {
    if (sapply(strsplit(polysemic[j], "\\s+"), length) > 1) {
      for (i in 1:nrow(df)) {
        if (grepl(polysemic[j], tolower(df$text[i]), ignore.case = TRUE))
        {(ref$polysemic2[i] <- ref$polysemic2[i] + 1)}
      }
    }
  }
  discourse_scores <- dplyr::inner_join(ref, discourse_scores, by = c("ID" = "ID"))
  discourse_scores$polysemic_ALL <- discourse_scores$polysemic + discourse_scores$polysemic2
  vars <- c("ID",  "polysemic_ALL")
  discourse_scores_short <- discourse_scores[vars]
  df <- dplyr::left_join(df, discourse_scores_short, by = c("ID" = "ID"))
  df$polysemic_ALL <- df$polysemic_ALL / df$WC

  ##LabMT
  labMT_values <- dplyr::inner_join(tidy_norms_clean, qdapDictionaries::labMT, by = c("word" = "word"), ignore_case = TRUE)
  labMT_values <- plyr::ddply(labMT_values,.(ID),plyr::summarize,
                              happiness_rank = sum(happiness_rank, na.rm = TRUE))
  df <- dplyr::left_join(df, labMT_values, by = c("ID" = "ID"))
  df$happiness_rank <- df$happiness_rank / df$WC


  #Readability
  readability <- quanteda.textstats::textstat_readability(df_corpus, measure = "Linsear.Write", remove_hyphens = TRUE,intermediate = FALSE)
  readability$document <- NULL
  df <- cbind(df, readability)

  #politeness
  df_politeness <- politeness::politeness(df$text, parser="spacy", drop_blank = FALSE, metric = "average")
  df$Swearing <- if (!is.null(df_politeness$Swearing)) {df$Negation <- df_politeness$Swearing} else {df$Swearing <- 0}
  df$Hello.x <- if (!is.null(df_politeness$Hello)) {df$Hello <- df_politeness$Hello} else {df$Hello <- 0}

  #sentence level spacy features
  suppressWarnings(spacy_new2A <- plyr::ddply(try, .(doc_id, sentence_id), dplyr::summarise,
                                              pre_UH_adv2_subj = (length(token_id[tag == 'UH' & token_id < token_id[dep_rel == "nsubj"]])/ length(token_id)),
                                              post_PRP_adv1_subj = (length(token_id[tag == 'PRP' & token_id > token_id[dep_rel == "nsubj"]])/ length(token_id[tag == 'PRP'])),
                                              post_adj1_ROOT = (length(token_id[pos == 'ADJ' & token_id > token_id[dep_rel == "ROOT"]])/ length(token_id[pos == 'ADJ'])),
                                              post_NNS_NOUN1_subj = (length(token_id[tag == 'NNS' & token_id > token_id[dep_rel == "nsubj"]])/ length(token_id[pos == 'NOUN'])),
                                              VB_VERB = (length(token_id[tag == 'VB'])/ length(token_id[pos == 'VERB']))
  ))
  options(dplyr.summarise.inform = FALSE)
  spacy_new2B <- spacy_new2A %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(dplyr::across(pre_UH_adv2_subj:VB_VERB, mean, na.rm = T))
  df <- dplyr::left_join(df, spacy_new2B, by = c("ID" = "doc_id"))

  #message level spacy features
  spacy_counts2 <- plyr::ddply(try, .(doc_id), plyr::summarise,
                               poss = sum(dep_rel == 'poss'),
                               VBG = sum(tag == 'VBG'))
  df <- dplyr::left_join(df, spacy_counts2, by = c("ID" = "doc_id"))
  df$poss <- df$poss / df$WC
  df$VBG.x <- df$VBG / df$WC

  # Emotion
  emotion <-  suppressWarnings(sentimentr::emotion_by(df$text, emotion_dt = lexicon::hash_nrc_emotions,
                                                      valence_shifters_dt = lexicon::hash_valence_shifters,
                                                      drop.unused.emotions = FALSE, un.as.negation = TRUE,
                                                      n.before = 5, n.after = 2))
  emotion_long <- subset(emotion, select = c("element_id", "emotion_type", "ave_emotion"))
  emotion_wide <- tidyr::spread(emotion_long, emotion_type, ave_emotion)
  emotion_wide$element_id <- NULL
  emotion_wide$anger_difference <- emotion_wide$anger - emotion_wide$anger_negated
  emotion_wide$joy_difference <- emotion_wide$joy - emotion_wide$joy
  emotion_cols <- emotion_wide[,c("disgust_negated","anger_difference", "joy_difference")]
  df <- cbind(df, emotion_cols)

  #Norms
  single_df <- dplyr::inner_join(tidy_norms_clean, single_words_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  single_scores <- plyr::ddply(single_df,.(ID),plyr::summarize, HAL = sum(HAL, na.rm = TRUE))
  df <- dplyr::left_join(df, single_scores, by = c("ID" = "ID"))

  norms_dic <- norms_dic[,c("Symbol", "Concreteness")]
  norms_df <- dplyr::inner_join(tidy_norms_clean, norms_dic, by = c("word" = "Symbol"), ignore_case = TRUE)

  norms_scores <- plyr::ddply(norms_df,.(ID),plyr::summarize,Concreteness = sum(Concreteness,  na.rm = TRUE))
  df <- dplyr::left_join(df, norms_scores, by = c("ID" = "ID"))
  df$Concreteness <- df$Concreteness/ df$WC

  ##Warmth Codings
  W_C_df <- dplyr::inner_join(tidy_norms_clean, W_C_ratings, by = c("word" = "Word"), ignore_case = TRUE)
  Positive_Warm <- W_C_df[W_C_df$`Warmth Rating` == '1',]
  Positive_Warm_Scores <- plyr::ddply(Positive_Warm,.(ID),plyr::summarize,Positive_Warm = sum(`Warmth Rating`, na.rm = TRUE))
  df <- dplyr::left_join(df, Positive_Warm_Scores, by = c("ID" = "ID"))
  df$Positive_Warm <- df$Positive_Warm/ df$WC

  # diversity index
  diversity <- qdap::diversity(df$text, grouping.var = df$ID)
  collision <- diversity[,c("ID", "collision")]
  df <- dplyr::left_join(df, collision, by = c("ID" = "ID"))


  # Running pre-trained model
  warmth_features <- df[,c("Courage_words", "explore_words", "polysemic_ALL", "Warmth_words", "Linsear.Write", "Swearing",
                           "pre_UH_adv2_subj", "post_PRP_adv1_subj", "post_adj1_ROOT", "finance_words", "VB_VERB", "anger_difference",
                           "employ_words", "happiness_rank", "social_words", "strong_words", "mental_verbs", "Image",
                           "post_NNS_NOUN1_subj", "HAL", "Hello.x", "poss", "Positive_Warm", "VBG.x", "disgust_negated",
                           "Concreteness", "joy_difference", "AoA", "collision")]
  warmth_features <-  raster::as.matrix(warmth_features)
  warmth_features[is.infinite(warmth_features)] <- 0
  warmth_features[is.na(warmth_features)] <- 0
  suppressWarnings(preprocessParams1<-caret::preProcess(warmth_features, method = c("center", "scale")))
  warmth_features1 <- stats::predict(preprocessParams1, warmth_features)
  warmth_predictions <- warmth_enet_model %>% raster::predict(warmth_features1)
  warmth_features_output <- warmth_features1
  colnames(warmth_features_output) <- c("Competence_Adjectives", "Exploration_Words", "Polysemic_Discourse_Markers", "Warmth_Orientation", "Readability", "Swearing",
                                        "Interjections_Before_Subject", "Personal_Pronouns_After_Subject", "Adjectives_After_Main_Verb", "Finance_Words", "Base_Form_Verbs", "Anger_Emotion",
                                        "Employment_Language", "Happiness_Emotion", "Social_Orientation", "Strength_Language", "Mental_Verbs", "Imagery",
                                        "Plural_Nouns_After_Subject", "Free_Association_Norms", "Hello", "Possession_Modifers", "Warmth_Language", "Present_Participle_Verbs", "Negated_Disgust",
                                        "Concreteness", "Joy_Emotion", "Word_Complexity", "Lexical_Diversity")
  df$warmth_predictions <- warmth_predictions
  # return
  if(metrics[1] == "features"){
    dataout=cbind(ID = df$ID, as.data.frame(warmth_features1))
  }
  if(metrics[1] == "all"){
    dataout=cbind(ID = df$ID, warmth_predictions = df$warmth_predictions, as.data.frame(warmth_features1))
  }
  if(metrics[1] == "scores"){
    dataout=df[, c("ID", "warmth_predictions")]
  }

  return(dataout)
}
