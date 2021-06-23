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
#' Yeomans, M., Kantor, A., & Tingley, D. (2018). The politeness Package: Detecting Politeness in Natural Language. R Journal, 10(2).
#' Rinker, T. W. (2018). lexicon: Lexicon Data version 1.2.1. http://github.com/trinker/lexicon
#' Moss, T. W., Renko, M., Block, E., & Meyskens, M. (2018). Funding the story of hybrid ventures: Crowdfunder lending preferences and linguistic hybridity. Journal of Business Venturing, 33(5), 643-659.
#' Buchanan, E. M., Valentine, K. D., & Maxwell, N. P. (2018). LAB: Linguistic Annotated Bibliography - Shiny Application. Retrieved from http://aggieerin.com/shiny/lab_table.
#' Rinker, T. W. (2013). qdapDictionaries: Dictionaries to Accompany the qdap Package. 1.0.7. University at Buffalo. Buffalo, New York. http://github.com/trinker/qdapDictionaries
#' Rinker, T. W. (2019). sentimentr: Calculate Text Polarity Sentiment version 2.7.1. http://github.com/trinker/sentimentr
#' Boyd, R. L. (2017). TAPA: Textual Affective Properties Analyzer (v.1.1.0) [Software]. Available from https://www.ryanboyd.io/software/tapa


##default for metrics is score
warmth <- function(text, ID, metrics = c("scores", "features", "all")){
  #word count and text objects
  df <- data.frame(text, ID)
  df$WC <- apply(df %>% dplyr::select(text), 1, ngram::wordcount)
  try <- spacy_tbl(text, ID)
  tidy_norms_clean <- words_clean(text, ID)
  df_corpus <- quanteda::corpus(df$text, docnames = df$ID)
  df_dfm <- quanteda::dfm(df_corpus, tolower = TRUE, stem = FALSE, select = NULL, remove = NULL, dictionary = NULL,
                          thesaurus = NULL, valuetype = c("glob", "regex", "fixed"))

  #politeness features
  df_politeness <- politeness::politeness(df$text, parser="spacy",drop_blank = TRUE, metric = "average")
  df$For.Me <- if (!is.null(df_politeness$For.Me)) {df$For.Me <- df_politeness$For.Me} else {df$For.Me <- 0}
  df$Please <- if (!is.null(df_politeness$Please)) {df$Please <- df_politeness$Please} else {df$Please <- 0}
  df$Hello.y <- if (!is.null(df_politeness$Hello)) {df$Hello.y <- df_politeness$Hello} else {df$Hello.y <- 0}

  #sentence level spacy features
  suppressWarnings(spacy_new2A <- plyr::ddply(try, .(doc_id, sentence_id), plyr::summarize,
                                              post_PRON1_main = (length(token_id[pos == 'PRON' & token_id > head_token_id])/ length(token_id[pos == 'PRON'])),
                                              post_VBD_VERB2_main = (length(token_id[tag == 'VBD' & token_id > head_token_id])/ length(token_id)),
                                              post_CCONJ2_nsubj = (length(token_id[pos == 'CCONJ' & token_id > token_id[dep_rel == "nsubj"]])/ length(token_id)),
                                              rel_dist3a = mean (token_id[dep_rel == 'nsubj'] - token_id[dep_rel == 'advcl']),
                                              n_before_subj = mean(token_id[dep_rel == 'nsubj']),
                                              rel_dist36a = mean (token_id[dep_rel == 'nsubj'] - token_id[dep_rel == 'poss']),
                                              pre_PART1_main = (length(token_id[pos == 'PART' & token_id < head_token_id])/ length(token_id[pos == 'PART'])),
                                              pre_DT_adv2_subj = (length(token_id[tag == 'DT' & token_id < token_id[dep_rel == "nsubj"]])/ length(token_id)),
                                              post_ADP1_ROOT = (length(token_id[pos == 'ADP' & token_id > token_id[dep_rel == "ROOT"]])/ length(token_id[pos == 'ADP'])),
                                              post_adj2_ROOT = (length(token_id[pos == 'ADJ' & token_id > token_id[dep_rel == "ROOT"]])/ length(token_id)),
                                              post_DT_adv1_main = (length(token_id[tag == 'DT' & token_id > head_token_id])/ length(token_id[tag == 'DT'])),
                                              pre_PRP_adv2_subj = (length(token_id[tag == 'PRP' & token_id < token_id[dep_rel == "nsubj"]])/ length(token_id)),
                                              post_NNS_NOUN1_ROOT = (length(token_id[tag == 'NNS' & token_id > token_id[dep_rel == "ROOT"]])/ length(token_id[pos == 'NOUN'])),
                                              phrase_length = mean((token_id - head_token_id)/ max(token_id)),
                                              post_adj1_ROOT = (length(token_id[pos == 'ADJ' & token_id > token_id[dep_rel == "ROOT"]])/ length(token_id[pos == 'ADJ'])),
                                              VBZ.y = sum(tag == 'VBZ')/ length(token_id),
                                              pron_first = (length(token_id[lemma == "-PRON-" & token_id == 1]))/ (length(token_id[lemma == "-PRON-"])),
                                              pre_PRON2_ROOT = (length(token_id[pos == 'PRON' & token_id < token_id[dep_rel == "ROOT"]])/ length(token_id)),
                                              pre_adv2_main = (length(token_id[pos == 'ADV' & token_id < head_token_id])/ length(token_id)),
                                              pre_TO_PART1_main = (length(token_id[tag == 'TO' & token_id < head_token_id])/ length(token_id[pos == 'PART']))))
  options(dplyr.summarise.inform = FALSE)
  spacy_new2B <- spacy_new2A %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(dplyr::across(post_PRON1_main:pre_TO_PART1_main, mean, na.rm = T))
  df <- dplyr::left_join(df, spacy_new2B, by = c("ID" = "doc_id"))

  #message level spacy features
  spacy_counts2 <- plyr::ddply(try, .(doc_id), plyr::summarize,
                               RBR = sum(tag == 'RBR'),
                               nummod = sum(dep_rel == 'nummod'),
                               PDT = sum(tag == 'PDT'),
                               PROPN = sum(pos == 'PROPN'),
                               intj = sum(dep_rel == 'intj'),
                               INTJ.x = sum(pos == 'INTJ'),
                               target13 = ((length(token_id[(token == 'us' | token == 'we')])))/ ((length(token_id[(pos == 'PRON' | pos == 'NOUN')]))),
                               agency_target1000 = ((length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine' | token == 'you'| token == 'yours')
                                                                    & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])))/
                                ((length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine' | token == 'you'| token == 'yours')
                                                  & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])) + (length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine'| token == 'you'| token == 'yours')
                                                                                                                                                                       & (dep_rel == 'dobj' | dep_rel == 'pobj')]))),
                               agency_target3000 = ((length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine'| token == 'you'| token == 'yours')
                                                                     & (dep_rel == 'dobj' | dep_rel == 'pobj')])))/((length(token_id[(tag == 'PRP' ) & (dep_rel == 'dobj' | dep_rel == 'pobj')]))/(length(token_id[(tag == 'PRP')]))),
                               targetK2 = ((length(token_id[(token == 'you'| token == 'yours' | token == 'us' | token == 'we')
                                                            & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])))/ ((length(token_id[(pos == 'PRON')]))),
                               agency_try000 = (length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine') & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])))
  df <- dplyr::left_join(df, spacy_counts2, by = c("ID" = "doc_id"))
  df$RBR <- df$RBR/ df$WC
  df$nummod <- df$nummod/ df$WC
  df$PDT <- df$PDT/ df$WC
  df$PROPN <- df$PROPN/ df$WC
  df$intj <- df$intj/ df$WC
  df$INTJ.x <- df$INTJ.x/ df$WC

  #regressive imagery features, social word feature, verb features
  wanted <- c("regex")
  cold <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "cold",][wanted]
  cold <- as.vector(cold$regex)
  cold <- cold[!is.na(cold)]
  passivity <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "passivity",][wanted]
  passivity <- as.vector(passivity$regex)
  passivity <- passivity[!is.na(passivity)]
  df$cold<- 0
  df$passivity <- 0
  df$social_words <- 0
  df$environmental_words <- 0
  df$education_words <- 0
  df$mental_verbs <- 0
  df$helping_verbs <- 0
  df$linking_verbs <- 0
  for (i in 1:nrow(df)) {
    for (j in 1:length(cold)) {
      if (grepl(cold[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$cold[i] <- df$cold[i] + 1) }
    for (j in 1:length(passivity)) {
      if (grepl(passivity[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$passivity[i] <- df$passivity[i] + 1) }
  for (j in 1:length(social_words$WORDS)) {
    if (grepl(social_words$WORDS[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$social_words[i] <- df$social_words[i] + 1) }
  for (j in 1:length(environmental_words$WORDS)) {
   if (grepl(environmental_words$WORDS[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$environmental_words[i] <- df$environmental_words[i] + 1) }
  for (j in 1:length(education_words$WORDS)) {
   if (grepl(education_words$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$education_words[i] <- df$education_words[i] + 1)}}
  for (j in 1:length(mental_verbs$WORDS)) {
    if (grepl(mental_verbs$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$mental_verbs[i] <- df$mental_verbs[i] + 1)} }
  for (j in 1:length(linking_verbs$WORDS)) {
    if (grepl(linking_verbs$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$linking_verbs[i] <- df$linking_verbs[i] + 1)} }
  for (j in 1:length(helping_verbs$WORDS)) {
   if (grepl(helping_verbs$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$helping_verbs[i] <- df$helping_verbs[i] + 1)} }}
  df$cold <- df$cold/ df$WC
  df$social_words <- df$social_words/ df$WC
  df$environmental_words <- df$environmental_words/ df$WC
  df$mental_verbs <- df$mental_verbs/ df$WC
  df$helping_verbs <- df$helping_verbs/ df$WC
  df$linking_verbs <- df$linking_verbs/ df$WC
  df$education_words <- df$education_words / df$WC

  #single word norms
  single_df <- dplyr::inner_join(tidy_norms_clean, single_words_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  single_scores <- plyr::ddply(single_df,.(ID),plyr::summarize, HAL = sum(HAL, na.rm = TRUE))
  df <- dplyr::left_join(df, single_scores, by = c("ID" = "ID"))
  df$HAL <- df$HAL/ df$WC

  #bundles
  bundle_5 <- c("hope", "love", "like", "support", "enjoy")
  bundle_2 <- c("years", "year", "name")
  bundle_qualifiers <- c("pretty", "less", "least", "just",
                         "somewhat", "more", "too", "so", "just", "enough",
                         "indeed", "still", "almost", "fairly",  "even",
                         "bit", "little")
  tidy_norms_clean$bundle_5 <- 0
  tidy_norms_clean$bundle_2 <- 0
  tidy_norms_clean$bundle_qualifiers <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_5) (tidy_norms_clean$bundle_5[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_qualifiers) (tidy_norms_clean$bundle_qualifiers[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_2) (tidy_norms_clean$bundle_2[i] =  1)}
  words_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                            bundle_5C = sum(bundle_5, na.rm = TRUE),
                            qualifiersC = sum(bundle_qualifiers, na.rm = TRUE),
                            bundle_2C = sum(bundle_2, na.rm = TRUE))
  words_scores$bundle_5C <- words_scores$bundle_5C/ nrow(tidy_norms_clean)
  words_scores$bundle_2C <- words_scores$bundle_2C/ nrow(tidy_norms_clean)
  words_scores$qualifiersC <- words_scores$qualifiersC/ nrow(tidy_norms_clean)
  df <- dplyr::left_join(df, words_scores, by = c("ID" = "ID"))

  ## Authenticity
  auth_df <- dplyr::inner_join(tidy_norms_clean, auth_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  auth_df <- auth_df %>% dplyr::select(-(Type))
  auth_scores <- plyr::ddply(auth_df,.(ID),plyr::summarize, Authenticity = sum(Authenticity, na.rm = TRUE))
  df <- dplyr::left_join(df, auth_scores, by = c("ID" = "ID"))
  df$Authenticity[is.na(df$Authenticity)] <- 0
  df$Authenticity <- df$Authenticity / df$WC

  # Emotion
  emotion <- sentimentr::emotion_by(df$text, emotion_dt = lexicon::hash_nrc_emotions,
                        valence_shifters_dt = lexicon::hash_valence_shifters,
                        drop.unused.emotions = FALSE, un.as.negation = TRUE,
                        n.before = 5, n.after = 2)
  emotion_long <- subset(emotion, select = c("element_id", "emotion_type", "ave_emotion"))
  emotion_wide <- tidyr::spread(emotion_long, emotion_type, ave_emotion)
  emotion_wide$element_id <- NULL
  emotion_cols <- emotion_wide[,c("fear","fear_negated", "surprise","surprise_negated")]
  df <- cbind(df, emotion_cols)
  df$fear_difference <- df$fear - df$fear_negated
  df$surprise_difference <- df$surprise - df$surprise_negated

  # polarity
  temp_pol <- qdapDictionaries::key.pol
  colnames(temp_pol)[2] <- "y_pol"
  key_pol <- dplyr::inner_join(tidy_norms_clean, temp_pol, by = c("word" = "x"), ignore_case = TRUE)
  key_pol <- plyr::ddply(key_pol,.(ID, y_pol),plyr::summarize, key_pol = sum(y_pol, na.rm = TRUE))

  negative_pol <- key_pol[key_pol$y == '-1',]
  wanted <- c("ID", "key_pol")
  negative_pol <- negative_pol[wanted]
  colnames(negative_pol)[2] <- "negative_pol"
  summary(negative_pol)
  df <- dplyr::left_join(df, negative_pol, by = c("ID" = "ID"))
  df$negative_pol <- df$negative_pol/ df$WC

  # Discourse Markers
  revision <- qdapDictionaries::discourse.markers.alemany$marker[which(qdapDictionaries::discourse.markers.alemany$type == "revision")]
  equality <- qdapDictionaries::discourse.markers.alemany$marker[which(qdapDictionaries::discourse.markers.alemany$type == "equality")]
  tidy_norms_clean$revision <- 0
  tidy_norms_clean$equality <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tidy_norms_clean$word[i] %in% revision) (tidy_norms_clean$revision[i] =  1)
    if (tidy_norms_clean$word[i] %in% equality) (tidy_norms_clean$equality[i] =  1)
  }
  discourse_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                              revision = sum(revision, na.rm = TRUE),
                              equality = sum(equality, na.rm = TRUE))
  ref <- as.data.frame(df$ID)
  names(ref)[names(ref) == 'df$ID'] <- 'ID'
  ref$revision2 <- 0
  ref$equality2 <- 0
  for (j in 1:length(revision)) {
    if (sapply(strsplit(revision[j], "\\s+"), length) > 1) {
      for (i in 1:nrow(df)) {
        if (grepl(revision[j], tolower(df$text[i]), ignore.case = TRUE)) {(ref$revision2[i] <- ref$revision2[i] + 1)}}}}
  for (j in 1:length(equality)) {
    if (sapply(strsplit(equality[j], "\\s+"), length) > 1) {
      for (i in 1:nrow(df)) {
        if (grepl(equality[j], tolower(df$text[i]), ignore.case = TRUE)) {(ref$equality2[i] <- ref$equality2[i] + 1)}}}}
  discourse_scores <- dplyr::inner_join(ref, discourse_scores, by = c("ID" = "ID"))
  discourse_scores$revision_ALL <- discourse_scores$revision + discourse_scores$revision2
  discourse_scores$equality_ALL <- discourse_scores$equality + discourse_scores$equality2
  vars <- c("ID",  "revision_ALL", "equality_ALL")
  discourse_scores_short <- discourse_scores[vars]
  df <- dplyr::left_join(df, discourse_scores_short, by = c("ID" = "ID"))
  df$revision_ALL <- df$revision_ALL / df$WC
  df$equality_ALL <- df$equality_ALL / df$WC

  # Running pre-trained model
  warmth_features <- df %>% dplyr::select(-(text:WC))
  warmth_features <-  raster::as.matrix(warmth_features)
  warmth_features[is.infinite(warmth_features)] <- 0
  warmth_features[is.na(warmth_features)] <- 0
  suppressWarnings(preprocessParams1<-caret::preProcess(warmth_features, method = c("center", "scale")))
  warmth_features1 <- stats::predict(preprocessParams1, warmth_features)
  warmth_predictions <- warmth_enet_final %>% stats::predict(warmth_features1)
  df$warmth_predictions <- warmth_predictions
  # return
  if(metrics[1] == "features") (return(df %>% dplyr::select(-(c("text", "WC", "warmth_predictions")))))
  if(metrics[1] == "all") (return(df %>% dplyr::select(-(c("text", "WC")))))
  if(metrics[1] == "scores") (return(df[, c("ID", "warmth_predictions")]))}
