#' Competence Detector
#'
#' @description Assesses competence perceptions in self-presentational natural language.
#'     This function is one of the main two functions of the  \code{warmthcompetence} package.
#'     It takes an N-length vector of self-presentational text documents and N-length vector of document IDs and returns a competence perception score that represents how much competence
#'     others attribute the individual who wrote the self-presentational text.
#'     The function also contains a metrics argument that enables users to also return the raw features used to assess competence perceptions.
#' @import plyr, magrittr, dplyr
#' @param text character A vector of texts, each of which will be assessed for competence.
#' @param ID character A vector of IDs that will be used to identify the competence scores.
#' @param metrics character An argument that allows users to decide what metrics to return. Users can return the competence scores (metrics = "scores"),
#'     the features that underlie the competence scores (metrics = "features"), or both the competence scores and the features (metrics = "all).
#'     The default choice is to return the competence scores.
#' @details Some features depend Spacyr which must be installed seperately in Python.
#' @return The default is to return a data.frame with each row containing the document identifier and the competence score.
#'     Users can also customize what is returned through the metrics argument. If metrics = "features", then a dataframe of competence features will be
#'     returned where each document is represented by a row. If metrics = "all", then both the competence scores and features will be returned in a data.frame.
#' @references
#' TO ADD

# library(magrittr)
# library(plyr)

# k <- competence(mit$Message, mit$ResponseId)
# summary(k)

# k <-competence(mit$Message, mit$ResponseId)
# dre <- cbind(k, k2)
# plot(dre)
# cor(k, k2)

##default for metrics is score
competence<- function(text, ID, metrics = c("scores", "features", "all")){
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
  df$YesNo.Questions <- if (!is.null(df_politeness$YesNo.Questions)) {df$YesNo.Questions <- df_politeness$YesNo.Questions} else {df$YesNo.Questions <- 0}
  df$Let.Me.Know <- if (!is.null(df_politeness$Let.Me.Know)) {df$Let.Me.Know <- df_politeness$Let.Me.Know} else {df$Let.Me.Know <- 0}
  df$Informal.Title <- if (!is.null(df_politeness$Informal.Title)) {df$Informal.Title <- df_politeness$Informal.Title} else {df$Informal.Title <- 0}
  df$For.Me <- if (!is.null(df_politeness$For.Me)) {df$For.Me <- df_politeness$For.Me} else {df$For.Me <- 0}
  df$Conjunction.Start <- if (!is.null(df_politeness$Conjunction.Start)) {df$Conjunction.Start <- df_politeness$Conjunction.Start} else {df$Conjunction.Start <- 0}
  df$Reasoning <- if (!is.null(df_politeness$Reasoning)) {df$Reasoning <- df_politeness$Reasoning} else {df$Reasoning <- 0}
  #sentence level spacy features
  suppressWarnings(spacy_new2A <- plyr::ddply(try, .(doc_id, sentence_id), plyr::summarize,
                             NNS.y = sum(tag == 'NNS') / length(token_id),
                             VB.y = sum(tag == 'VB')/ length(token_id),
                             VBZ.y = sum(tag == 'VBZ')/ length(token_id),
                             post_CC_adv1_main = (length(token_id[tag == 'CC' & token_id > head_token_id])/ length(token_id[tag == 'CC'])),
                             pre_PRON1_main = (length(token_id[pos == 'PRON' & token_id < head_token_id])/ length(token_id[pos == 'PRON']))))
  options(dplyr.summarise.inform = FALSE)
  spacy_new2B <- spacy_new2A %>%
    dplyr::group_by(doc_id) %>%
  dplyr::summarise(dplyr::across(NNS.y:pre_PRON1_main, mean, na.rm = T))
  df <- dplyr::left_join(df, spacy_new2B, by = c("ID" = "doc_id"))
  #message level spacy features
  spacy_counts2 <- plyr::ddply(try, .(doc_id), plyr::summarize, count_sent = max(sentence_id),
                               agency_target3 = (length(token_id[(tag == 'PRP' ) & (dep_rel == 'dobj' | dep_rel == 'pobj')]))/(length(token_id[(tag == 'PRP')])),
                               agency_target6 = (length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine') & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )]))
                               / ((length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine') & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )]))
                                  + (length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine') & (dep_rel == 'dobj' | dep_rel == 'pobj')]))),
                               HYPH = sum(tag == 'HYPH'),
                               nummod = sum(dep_rel == 'nummod'),
                               dobj = sum(dep_rel == 'dobj'),
                               neg = sum(dep_rel == 'neg'),
                               ADV = sum(pos == 'ADV'))
  df <- dplyr::left_join(df, spacy_counts2, by = c("ID" = "doc_id"))
  df$agency_target3 <- df$agency_target3/ df$WC
  df$agency_target6 <- df$agency_target6/ df$WC
  df$HYPH <- df$HYPH/ df$WC
  df$nummod <- df$nummod/ df$WC
  df$dobj <- df$dobj/ df$WC
  df$neg <- df$neg/ df$WC
  df$ADV <- df$ADV/ df$WC
  #regressive imagery features, education words feature, and employment words feature
  wanted <- c("regex")
  depth <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "depth",][wanted]
  depth <- as.vector(depth$regex)
  depth <- depth[!is.na(depth)]
  narcissism <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "narcissism",][wanted]
  narcissism <- as.vector(narcissism$regex)
  narcissism <- narcissism[!is.na(narcissism)]
  timelessnes <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "timelessnes",][wanted]
  timelessnes <- as.vector(timelessnes$regex)
  timelessnes <- timelessnes[!is.na(timelessnes)]
  df$depth<- 0
  df$narcissism<- 0
  df$timelessnes<- 0
  df$education_words <- 0
  df$employ_words <- 0
  for (i in 1:nrow(df)) {
    for (j in 1:length(depth)) {
      if (grepl(depth[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$depth[i] <- df$depth[i] + 1) }
    for (j in 1:length(narcissism)) {
      if (grepl(narcissism[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$narcissism[i] <- df$narcissism[i] + 1) }
    for (j in 1:length(timelessnes)) {
      if (grepl(timelessnes[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$timelessnes[i] <- df$timelessnes[i] + 1) }
    for (j in 1:length(education_words$WORDS)) {
      if (grepl(education_words$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$education_words[i] <- df$education_words[i] + 1)}}
    for (f in 1:length(employ_words$WORDS)) {
      if (grepl(employ_words$WORDS[f], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$employ_words[i] <- df$employ_words[i] + 1)}}}
  df$depth <- df$depth/ df$WC
  df$narcissism <- df$narcissism/ df$WC
  df$timelessnes <- df$timelessnes/ df$WC
  df$education_words <- df$education_words / df$WC
  df$employ_words <- df$employ_words / df$WC
  #single word norms
  single_df <- dplyr::inner_join(tidy_norms_clean, single_words_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  single_scores <- plyr::ddply(single_df,.(ID),plyr::summarize, Subtlex = sum(Subtlex, na.rm = TRUE))
  df <- dplyr::left_join(df, single_scores, by = c("ID" = "ID"))
  #qdap dictionaries & bundles
  bundle_1 <- c("as", "an", "while", "in")
  tidy_norms_clean$bundle_1 <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
      if (tolower(tidy_norms_clean$word[i]) %in% bundle_1) (tidy_norms_clean$bundle_1[i] =  1)}
  words_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                              bundle_1C = sum(bundle_1, na.rm = TRUE))
  words_scores$bundle_1C <- words_scores$bundle_1C/ nrow(tidy_norms_clean)
  df <- dplyr::left_join(df, words_scores, by = c("ID" = "ID"))
  # Adjective Modality Norms
  adj_mod_df <- dplyr::inner_join(tidy_norms_clean, adj_mod_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  adj_mod_scores <- ddply(adj_mod_df,.(ID),summarise, OlfactoryStrengthMean = sum(OlfactoryStrengthMean, na.rm = TRUE))
  df <- dplyr::left_join(df, adj_mod_scores, by = c("ID" = "ID"))
  df$OlfactoryStrengthMean <- df$OlfactoryStrengthMean/ df$WC
  # Emotion
  emotion <- sentimentr::emotion_by(df$text, emotion_dt = lexicon::hash_nrc_emotions,
                                    valence_shifters_dt = lexicon::hash_valence_shifters,
                                    drop.unused.emotions = FALSE, un.as.negation = TRUE,
                                    n.before = 5, n.after = 2)
  emotion_long <- subset(emotion, select = c("element_id", "emotion_type", "ave_emotion"))
  emotion_wide <- tidyr::spread(emotion_long, emotion_type, ave_emotion)
  emotion_wide$element_id <- NULL
  emotion_cols <- emotion_wide[,c("anticipation_negated", "joy")]
  df <- cbind(df, emotion_cols)
  #Readability
  readability <- quanteda::textstat_readability(df_corpus, measure = c("Dale.Chall.old", "meanWordSyllables", "meanSentenceLength", "FOG.NRI"), remove_hyphens = TRUE, intermediate = FALSE)
  df$Dale.Chall.old <- readability$Dale.Chall.old
  df$meanWordSyllables <- readability$meanWordSyllables
  df$meanSentenceLength <- readability$meanSentenceLength
  df$FOG.NRI <- readability$FOG.NRI
  #Lexical Diversity
  suppressWarnings(lexdiv <- quanteda::textstat_lexdiv(df_dfm,measure = c("I", "K", "CTTR"), remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,
                                      remove_hyphens = FALSE, log.base = 10, MATTR_window = 100L, MSTTR_segment = 100L))
  lexdiv$document <- NULL
  names(lexdiv)[names(lexdiv) == 'I'] <- 'lexdiv_I'
  df <- cbind(df, lexdiv)
  diversity <- qdap::diversity(df$text, grouping.var = df$ID)
  diversity <- diversity[, c("ID" ,"brillouin")]
  df <- dplyr::left_join(df, diversity, by = c("ID" = "ID"))
  #sentiment
  sentiment_jockers_rinker <- dplyr::inner_join(tidy_norms_clean, lexicon::hash_sentiment_jockers_rinker, by = c("word" = "x"), ignore_case = TRUE)
  suppressWarnings(sentiment_jockers_rinker$sign <- NA)
  for (j in 1:nrow(sentiment_jockers_rinker)) {
    if (sentiment_jockers_rinker$y[j] > 0) {sentiment_jockers_rinker$sign[j] <- 1}
    if (sentiment_jockers_rinker$y[j] < 0) {sentiment_jockers_rinker$sign[j] <- -1}}
  sentiment_jockers_rinker <- plyr::ddply(sentiment_jockers_rinker,.(ID, sign),plyr::summarize, value = sum(y, na.rm = TRUE))
  negative_sentiment_2 <- sentiment_jockers_rinker[sentiment_jockers_rinker$sign == -1,]
  wanted <- c("ID", "value")
  negative_sentiment_2 <- negative_sentiment_2[wanted]
  colnames(negative_sentiment_2)[2] <- "negative_sentiment_2"
  df <- dplyr::left_join(df, negative_sentiment_2, by = c("ID" = "ID"))
  df$negative_sentiment_2 <- df$negative_sentiment_2/ df$WC
  # Misspelled Words
  n_misspelled <- sapply(df$text, function(x){
    length(qdap::which_misspelled(x, suggest = FALSE))
  })
  misspellings <- data.frame(df$text, n_misspelled, row.names = NULL)
  df <- cbind(df,misspellings$n_misspelled)
  names(df)[names(df) == 'misspellings$n_misspelled'] <- 'misspelled'
  df$misspelled <- df$misspelled/ df$WC
  # Running pre-trained model
  competence_features <- df %>% dplyr::select(-(text:WC))
  competence_features <-  raster::as.matrix(competence_features)
  competence_features[is.infinite(competence_features)] <- 0
  competence_features[is.na(competence_features)] <- 0
  suppressWarnings(preprocessParams1<-caret::preProcess(competence_features, method = c("center", "scale")))
  competence_features1 <- stats::predict(preprocessParams1, competence_features)
  competence_predictions <- competence_model %>% stats::predict(competence_features1)
  df$competence_predictions <- competence_predictions
  if(metrics[1] == "features") (return(competence_features))
  if(metrics[1] == "all") (return(df))
  if(metrics[1] == "scores") (return(competence_predictions))}


