#' Warmth Detector
#'
#' @description Assesses warmth perceptions in self-presentational natural language.
#'     This function is one of the main two functions of the  \code{warmthcompetence} package.
#'     It takes an N-length vector of self-presentational text documents and N-length vector of document IDs and returns a warmth perception score that represents how much warmth
#'     others attribute the individual who wrote the self-presentational text.
#'     The function also contains a metrics argument that enables users to also return the raw features used to assess warmth perceptions.
#' @import plyr, magrittr
#' @param text character A vector of texts, each of which will be assessed for warmth.
#' @param ID character A vector of IDs that will be used to identify the warmth scores.
#' @param metrics character An argument that allows users to decide what metrics to return. Users can return the warmth scores (metrics = "scores"),
#'     the features that underlie the warmth scores (metrics = "features"), or both the warmth scores and the features (metrics = "all).
#'     The default choice is to return the warmth scores.
#' @details Some features depend Spacyr which must be installed seperately in Python.
#' @return The default is to return a data.frame with each row containing the document identifier and the warmth score.
#'     Users can also customize what is returned through the metrics argument. If metrics = "features", then a dataframe of warmth features will be
#'     returned where each document is represented by a row. If metrics = "all", then both the warmth scores and features will be returned in a data.frame.
#' @references
#' TO ADD


# k2 <- warmth(mit$Message, mit$ResponseId)
# summary(k)
#
# warmth(mit$Message, mit$ResponseId, "all")

##default for metrics is score
warmth <- function(text, ID, metrics = c("scores", "features", "all")){
  #word count and text objects
  df <- data.frame(text, ID)
  df$WC <- apply(df %>% dplyr::select(text), 1, ngram::wordcount)
  try <- spacy_tbl(text, ID)
  words_clean <- function(text, ID){
    allData <- tibble::tibble(text, ID)
    allData$text_clean <- tm::stripWhitespace(tm::removePunctuation(qdap::replace_symbol(qdap::replace_abbreviation(qdap::replace_contraction(qdap::clean(text))))))
    tidy_norms_clean <- allData %>% dplyr::select(text_clean, ID)  %>%
      tidytext::unnest_tokens("word", text_clean, to_lower = FALSE)
    return(tidy_norms_clean)
  }
  tidy_norms_clean <- words_clean(text, ID)
  df_corpus <- quanteda::corpus(df$text, docnames = df$ID)
  df_dfm <- quanteda::dfm(df_corpus, tolower = TRUE, stem = FALSE, select = NULL, remove = NULL, dictionary = NULL,
                thesaurus = NULL, valuetype = c("glob", "regex", "fixed"))
  #politeness features
  df_politeness <- politeness::politeness(df$text, parser="spacy",drop_blank = TRUE, metric = "average")
  df$Negation <- if (!is.null(df_politeness$Negation)) {df$Negation <- df_politeness$Negation} else {df$Negation <- 0}
  df$Swearing <- if (!is.null(df_politeness$Swearing)) {df$Swearing <- df_politeness$Swearing} else {df$Swearing <- 0}
  df$Hello.y <- if (!is.null(df_politeness$Hello)) {df$Hello.y <- df_politeness$Hello} else {df$Hello.y <- 0}
  df$Hedges <- if (!is.null(df_politeness$Hedges)) {df$Hedges <- df_politeness$Hedges} else {df$Hedges <- 0}
  df$Please <- if (!is.null(df_politeness$Please)) {df$Please <- df_politeness$Please} else {df$Please <- 0}
  df$Bare.Command <- if (!is.null(df_politeness$Bare.Command)) {df$Bare.Command <- df_politeness$Bare.Command} else {df$Bare.Command <- 0}
  df$Reassurance <- if (!is.null(df_politeness$Reassurance)) {df$Reassurance <- df_politeness$Reassurance} else {df$Reassurance <- 0}
  df$Gratitude <- if (!is.null(df_politeness$Gratitude)) {df$Gratitude <- df_politeness$Gratitude} else {df$Gratitude <- 0}
  #sentence level spacy features
  suppressWarnings(spacy_new2A <- plyr::ddply(try, .(doc_id, sentence_id), plyr::summarize,
                     pre_UH_adv2_subj = (length(token_id[tag == 'UH' & token_id < token_id[dep_rel == "nsubj"]])/ length(token_id)),
                     pre_adv2_ROOT = (length(token_id[pos == 'ADV' & token_id < token_id[dep_rel == "ROOT"]])/ length(token_id)),
                     pre_adv2_main = (length(token_id[pos == 'ADV' & token_id < head_token_id])/ length(token_id)),
                     post_NNS_NOUN1_main = (length(token_id[tag == 'NNS' & token_id > head_token_id])/ length(token_id[pos == 'NOUN'])),
                     VBG_VERB = (length(token_id[tag == 'VBG'])/ length(token_id[pos == 'VERB'])),
                     VBZ.x = sum(tag == 'VBZ')/ length(token_id)))
  options(dplyr.summarise.inform = FALSE)
  spacy_new2B <- spacy_new2A %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(dplyr::across(pre_UH_adv2_subj:VBZ.x, mean, na.rm = T))
  df <- dplyr::left_join(df, spacy_new2B, by = c("ID" = "doc_id"))
  #regressive imagery features, social word feature, mental verbs feature
  wanted <- c("regex")
  cold <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "cold",][wanted]
  cold <- as.vector(cold$regex)
  cold <- cold[!is.na(cold)]
  taste <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "taste",][wanted]
  taste <- as.vector(taste$regex)
  taste <- taste[!is.na(taste)]
  ascend <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "ascend",][wanted]
  ascend <- as.vector(ascend$regex)
  ascend <- ascend[!is.na(ascend)]
  chaos <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "chaos",][wanted]
  chaos <- as.vector(chaos$regex)
  chaos <- chaos[!is.na(chaos)]
  df$cold<- 0
  df$taste<- 0
  df$ascend<- 0
  df$chaos<- 0
  df$social_words <- 0
  df$mental_verbs <- 0
  for (i in 1:nrow(df)) {
    for (j in 1:length(cold)) {
      if (grepl(cold[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$cold[i] <- df$cold[i] + 1) }
    for (j in 1:length(taste)) {
      if (grepl(taste[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$taste[i] <- df$taste[i] + 1) }
    for (j in 1:length(ascend)) {
      if (grepl(ascend[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$ascend[i] <- df$ascend[i] + 1) }
    for (j in 1:length(chaos)) {
      if (grepl(chaos[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$chaos[i] <- df$chaos[i] + 1) }
  for (j in 1:length(social_words$WORDS)) {
    if (grepl(social_words$WORDS[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$social_words[i] <- df$social_words[i] + 1) }
  for (j in 1:length(mental_verbs$WORDS)) {
    if (grepl(mental_verbs$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$mental_verbs[i] <- df$mental_verbs[i] + 1)} }}
  df$cold <- df$cold/ df$WC
  df$ascend <- df$ascend/ df$WC
  df$taste <- df$taste/ df$WC
  df$chaos <- df$chaos/ df$WC
  df$social_words <- df$social_words/ df$WC
  df$mental_verbs <- df$mental_verbs/ df$WC
  #message level spacy features
  spacy_counts2 <- plyr::ddply(try, .(doc_id), plyr::summarize,
                         RBR = sum(tag == 'RBR'),
                         XX = sum(tag == 'XX'),
                         ADD = sum(tag == 'ADD'),
                         agency_target300 = (length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine') & (dep_rel == 'dobj' | dep_rel == 'pobj')]))/(length(token_id[(tag == 'PRP')])),
                         agency_target200 = (length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine') & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )]))/(length(token_id[(tag == 'PRP')])),
                         target13 = (length(token_id[(token == 'us' | token == 'we')]))/ (length(token_id[(pos == 'PRON' | pos == 'NOUN')])),
                         poss = sum(dep_rel == 'poss'),
                         predet = sum(dep_rel == 'predet'),
                         SYM = sum(pos == 'SYM'),
                         acl = sum(dep_rel == 'acl'),
                         parataxis = sum(dep_rel == 'parataxis'))
  df <- dplyr::left_join(df, spacy_counts2, by = c("ID" = "doc_id"))
  df$RBR <- df$RBR/ df$WC
  df$XX <- df$XX/ df$WC
  df$ADD <- df$ADD/ df$WC
  df$poss <- df$poss/ df$WC
  df$predet <- df$predet/ df$WC
  df$SYM <- df$SYM/ df$WC
  df$acl <- df$acl/ df$WC
  df$parataxis <- df$parataxis/ df$WC
  #single word norms
  single_df <- dplyr::inner_join(tidy_norms_clean, single_words_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  single_scores <- plyr::ddply(single_df,.(ID),plyr::summarize, HAL = sum(HAL, na.rm = TRUE))
  df <- dplyr::left_join(df, single_scores, by = c("ID" = "ID"))
  df$HAL <- df$HAL/ df$WC
  #qdap dictionaries & bundles
  bundle_5 <- c("hope", "love", "like", "support", "enjoy")
  bundle_7 <- c("am","be","were","is","seem","was","appear","are","have","has","had","do","did")
  bundle_8 <- c("concern","doubt","hate","impress","like","mind","remember","understand","promise","decide","feel",
                "prefer","know","look","notice","realize","see","recognize","think","dislike","forgot","hope","learn","love","own")
  tidy_norms_clean$submit_words <- 0
  tidy_norms_clean$power_words <- 0
  tidy_norms_clean$strong_words <- 0
  tidy_norms_clean$bundle_5 <- 0
  tidy_norms_clean$bundle_7 <- 0
  tidy_norms_clean$bundle_8 <- 0
  tidy_norms_clean$revision <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tolower(tidy_norms_clean$word[i]) %in% qdapDictionaries::submit.words) (tidy_norms_clean$submit_words[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% qdapDictionaries::power.words) (tidy_norms_clean$power_words[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% qdapDictionaries::strong.words) (tidy_norms_clean$strong_words[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_5) (tidy_norms_clean$bundle_5[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_7) (tidy_norms_clean$bundle_7[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_8) (tidy_norms_clean$bundle_8[i] =  1)}
  words_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                               submit_words = sum(submit_words, na.rm = TRUE),
                               power_words = sum(power_words, na.rm = TRUE),
                               strong_words = sum(strong_words, na.rm = TRUE),
                              bundle_5C = sum(bundle_5, na.rm = TRUE)/nrow(tidy_norms_clean),
                            bundle_7C = sum(bundle_7, na.rm = TRUE)/nrow(tidy_norms_clean),
                            bundle_8C = sum(bundle_8, na.rm = TRUE)/nrow(tidy_norms_clean))
  df <- dplyr::left_join(df, words_scores, by = c("ID" = "ID"))
  df$submit_words <- df$submit_words/ df$WC
  df$power_words <- df$power_words/ df$WC
  df$strong_words <- df$strong_words/ df$WC
  # Adjective Modality Norms
  adj_mod_df <- dplyr::inner_join(tidy_norms_clean, adj_mod_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  adj_mod_scores <- ddply(adj_mod_df,.(ID),summarise, GustatoryStrengthMean = sum(GustatoryStrengthMean, na.rm = TRUE))
  df <- dplyr::left_join(df, adj_mod_scores, by = c("ID" = "ID"))
  df$GustatoryStrengthMean <- df$GustatoryStrengthMean/ df$WC
  # Emotion
  emotion <- sentimentr::emotion_by(df$text, emotion_dt = lexicon::hash_nrc_emotions,
                        valence_shifters_dt = lexicon::hash_valence_shifters,
                        drop.unused.emotions = FALSE, un.as.negation = TRUE,
                        n.before = 5, n.after = 2)
  emotion_long <- subset(emotion, select = c("element_id", "emotion_type", "ave_emotion"))
  emotion_wide <- tidyr::spread(emotion_long, emotion_type, ave_emotion)
  emotion_wide$element_id <- NULL
  names(emotion_wide)[names(emotion_wide) == 'anger'] <- 'sentiment_anger'
  emotion_cols <- emotion_wide[,c("trust_negated", "disgust_negated", "sentiment_anger")]
  df <- cbind(df, emotion_cols)
  #sentiment
  sentiment <- sentimentr::sentiment_by(df$text)
  df$ave_sentiment <- sentiment$ave_sentiment
  #Readability
  readability <- quanteda::textstat_readability(df_corpus, measure = "Dale.Chall.PSK", remove_hyphens = TRUE, intermediate = FALSE)
  df$Dale.Chall.PSK <- readability$Dale.Chall.PSK
  #Lexical Diversity
  suppressWarnings(lexdiv <- quanteda::textstat_lexdiv(df_dfm,measure = c("R", "S"), remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,
                            remove_hyphens = FALSE, log.base = 10, MATTR_window = 100L, MSTTR_segment = 100L))
  lexdiv$document <- NULL
  df <- cbind(df, lexdiv)
  # Discourse Markers
  revision <- qdapDictionaries::discourse.markers.alemany$marker[which(qdapDictionaries::discourse.markers.alemany$type == "revision")]
  tidy_norms_clean$revision <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tidy_norms_clean$word[i] %in% revision) (tidy_norms_clean$revision[i] =  1)
  }
  discourse_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                              revision = sum(revision, na.rm = TRUE))
  ref <- as.data.frame(df$ID)
  names(ref)[names(ref) == 'df$ID'] <- 'ID'
  ref$revision2 <- 0
  for (j in 1:length(revision)) {
    if (sapply(strsplit(revision[j], "\\s+"), length) > 1) {
      for (i in 1:nrow(df)) {
        if (grepl(revision[j], tolower(df$text[i]), ignore.case = TRUE)) {(ref$revision2[i] <- ref$revision2[i] + 1)}}}}
  discourse_scores <- dplyr::inner_join(ref, discourse_scores, by = c("ID" = "ID"))
  discourse_scores$revision_ALL <- discourse_scores$revision + discourse_scores$revision2
  vars <- c("ID",  "revision_ALL")
  discourse_scores_short <- discourse_scores[vars]
  df <- dplyr::left_join(df, discourse_scores_short, by = c("ID" = "ID"))
  df$revision_ALL <- df$revision_ALL / df$WC
  # Running pre-trained model
  warmth_features <- df %>% dplyr::select(-(text:WC))
  warmth_features <-  raster::as.matrix(warmth_features)
  warmth_features[is.infinite(warmth_features)] <- 0
  warmth_features[is.na(warmth_features)] <- 0
  suppressWarnings(preprocessParams1<-caret::preProcess(warmth_features, method = c("center", "scale")))
  warmth_features1 <- stats::predict(preprocessParams1, warmth_features)
  warmth_predictions <- warmth_model %>% stats::predict(warmth_features1)
  df$warmth_predictions <- warmth_predictions
  # return
  if(metrics[1] == "features") (return(df %>% dplyr::select(-(c("text", "WC", "warmth_predictions")))))
  if(metrics[1] == "all") (return(df %>% dplyr::select(-(c("text", "WC")))))
  if(metrics[1] == "scores") (return(df[, c("ID", "warmth_predictions")]))}


