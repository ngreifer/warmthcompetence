#' Competence Detector
#'
#' @description Assesses competence perceptions in self-presentational natural language.
#'     This function is one of the main two functions of the  \code{warmthcompetence} package.
#'     It takes an N-length vector of self-presentational text documents and N-length vector of document IDs and returns a competence perception score that represents how much competence
#'     others attribute the individual who wrote the self-presentational text.
#'     The function also contains a metrics argument that enables users to also return the raw features used to assess competence perceptions.
#' @param text character A vector of texts, each of which will be assessed for competence.
#' @param ID character A vector of IDs that will be used to identify the competence scores.
#' @param metrics character An argument that allows users to decide what metrics to return. Users can return the competence scores (metrics = "scores"),
#'     the features that underlie the competence scores (metrics = "features"), or both the competence scores and the features (metrics = "all).
#'     The default choice is to return the competence scores.
#' @details Some features depend Spacyr which must be installed seperately in Python.
#' @return The default is to return a data.frame with each row containing the document identifier and the competence score.
#'     Users can also customize what is returned through the metrics argument. If metrics = "features", then a dataframe of competence features will be
#'     returned where each document is represented by a row. If metrics = "all", then both the competence scores and features will be returned in a data.frame.
#'
#' @importFrom plyr .
#' @importFrom magrittr %>%
#' @references
#' Rinker, T. W. (2018). lexicon: Lexicon Data version 1.2.1. http://github.com/trinker/lexicon
#' Moss, T. W., Renko, M., Block, E., & Meyskens, M. (2018). Funding the story of hybrid ventures: Crowdfunder lending preferences and linguistic hybridity. Journal of Business Venturing, 33(5), 643-659.
#' Buchanan, E. M., Valentine, K. D., & Maxwell, N. P. (2018). LAB: Linguistic Annotated Bibliography - Shiny Application. Retrieved from http://aggieerin.com/shiny/lab_table.
#' Rinker, T. W. (2013). qdapDictionaries: Dictionaries to Accompany the qdap Package. 1.0.7. University at Buffalo. Buffalo, New York. http://github.com/trinker/qdapDictionaries
#' Rinker, T. W. (2019). sentimentr: Calculate Text Polarity Sentiment version 2.7.1. http://github.com/trinker/sentimentr
#' Boyd, R. L. (2017). TAPA: Textual Affective Properties Analyzer (v.1.1.0) [Software]. Available from https://www.ryanboyd.io/software/tapa
#'
#' @examples
#'
#' data("vignette_data")
#'
#' competence_scores <- competence(vignette_data$Message, vignette_data$ResponseId, metrics = "all")
#'
#' vignette_data$competence_predictions <- competence_scores$competence_predictions
#'
#' competence_model2 <- lm(RA_comp_AVG  ~ competence_predictions, data = vignette_data)
#' summary(competence_model2)
#'
#'
#'@export
competence<- function(text, ID=NULL, metrics = c("scores", "features", "all")){
  if(is.null(ID)){
    ID=as.character(1:length(text))
  }
  #For CRAN check
  doc_id <- sentence_id<- tag<- token_id<- pos<- head_token_id<- dep_rel<- VBZ.y<- pre_TO_PART1_main<- nounphrase<- token<- NULL
  HAL<- amplification<- emotion_type<- ave_emotion<- y_pol<-  Warmth.Rating<- Hello<- Please<- NULL
  social_words = social_words
  environmental_words  = environmental_words
  education_words  = education_words
  linking_verbs  = linking_verbs
  helping_verbs  =  helping_verbs
  single_words_dic  = single_words_dic
  W_C_ratings  = W_C_ratings
  competence_enet_final = competence_enet_final

  #word count and text objects
  df <- data.frame(text, ID)
  df$WC <- apply(df %>% dplyr::select(text), 1, ngram::wordcount)
  tbl <- tibble::as_tibble(
    data.frame(doc_id = ID, text = text, stringsAsFactors = F)
  )
  try <- spacyr::spacy_parse(tbl, tag = TRUE, dependency = TRUE, nounphrase = TRUE,entity=FALSE)
  tidy_norms_clean <- words_clean(text, ID)
  df_corpus <- quanteda::corpus(df$text, docnames = df$ID)
  df_dfm <- quanteda::dfm(df_corpus, tolower = TRUE, stem = FALSE, select = NULL, remove = NULL, dictionary = NULL,
                          thesaurus = NULL, valuetype = c("glob", "regex", "fixed"))

  df$For.Me <- (stringr::str_count(tolower(df$text), "for me")
                +stringr::str_count(tolower(df$text), "for us"))/df$WC

  #sentence level spacy features
  suppressWarnings(spacy_new2A <- plyr::ddply(try, .(doc_id, sentence_id), plyr::summarize,
                                              VBZ.y = sum(tag == 'VBZ')/ length(token_id),
                                              post_PRON1_main = (length(token_id[pos == 'PRON' & token_id > head_token_id])/ length(token_id[pos == 'PRON'])),
                                              pre_PRON2_ROOT = (length(token_id[pos == 'PRON' & token_id < token_id[dep_rel == "ROOT"]])/ length(token_id)),
                                              post_DT_adv1_main = (length(token_id[tag == 'DT' & token_id > head_token_id])/ length(token_id[tag == 'DT'])),
                                              rel_dist36a = mean (token_id[dep_rel == 'nsubj'] - token_id[dep_rel == 'poss']),
                                              pre_adv2_main = (length(token_id[pos == 'ADV' & token_id < head_token_id])/ length(token_id)),
                                              post_adj2_ROOT = (length(token_id[pos == 'ADJ' & token_id > token_id[dep_rel == "ROOT"]])/ length(token_id)),
                                              n_before_subj = mean(token_id[dep_rel == 'nsubj']),
                                              DT.y = (length(token_id[tag == 'DT'])/ length(token_id)),
                                              post_VERB1_subj = (length(token_id[pos == 'VERB' & token_id > token_id[dep_rel == "nsubj"]])/ length(token_id[pos == 'VERB'])),
                                              post_NNS_NOUN2_subj = (length(token_id[tag == 'NNS' & token_id > token_id[dep_rel == "nsubj"]])/ length(token_id)),
                                              post_NOUN2_ROOT = (length(token_id[pos == 'NOUN' & token_id > token_id[dep_rel == "ROOT"]])/ length(token_id)),
                                              pre_adv2_subj = (length(token_id[pos == 'ADV' & token_id < token_id[dep_rel == "nsubj"]])/ length(token_id)),
                                              pre_VBZ_VERB2_ROOT = (length(token_id[tag == 'VBZ' & token_id < token_id[dep_rel == "ROOT"]])/ length(token_id)),
                                              pre_DT_adv1_ROOT = (length(token_id[tag == 'DT' & token_id < token_id[dep_rel == "ROOT"]])/ length(token_id[tag == 'DT'])),
                                              pre_TO_PART1_main = (length(token_id[tag == 'TO' & token_id < head_token_id])/ length(token_id[pos == 'PART']))))
  options(dplyr.summarise.inform = FALSE)
  spacy_new2B <- spacy_new2A %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(dplyr::across(VBZ.y:pre_TO_PART1_main, mean, na.rm = T))
  df <- dplyr::left_join(df, spacy_new2B, by = c("ID" = "doc_id"))
  #message level spacy features
  spacy_counts2 <- plyr::ddply(try, .(doc_id), plyr::summarize,
                               nummod = sum(dep_rel == 'nummod'),
                               INTJ.x = sum(pos == 'INTJ'),
                               intj = sum(dep_rel == 'intj'),
                               PDT = sum(tag == 'PDT'),
                               PROPN = sum(pos == 'PROPN'),
                               RBR = sum(tag == 'RBR'),
                               appos = sum(dep_rel == 'appos'),
                               dative = sum(dep_rel == 'dative'),
                               nsubjpass = sum(dep_rel == 'nsubjpass'),
                               JJR = sum(tag == 'JJR'),
                               mid = sum(nounphrase == 'mid'),
                               targetK2 = ((length(token_id[(token == 'you'| token == 'yours' | token == 'us' | token == 'we')
                                                            & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])))/ ((length(token_id[(pos == 'PRON')]))),
                               target5 = (length(token_id[(tag == 'PRP')]))/(length(token_id)),
                               targetK = ((length(token_id[(token == 'you'| token == 'yours' | token == 'us' | token == 'we')
                                                           & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])))/ (((length(token_id[(token == 'you'| token == 'yours' | token == 'us' | token == 'we')
                                                                                                                                                                                  & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )]))) + (length(token_id[(token == 'you'| token == 'yours' | token == 'us' | token == 'we')
                                                                                                                                                                                                                                                                                                        & (dep_rel == 'dobj' | dep_rel == 'pobj')]))),
                               c_target2 = (length(token_id[(token == 'you'| token == 'yours' | token == 'us' | token == 'we')
                                                            & (dep_rel == 'dobj' | dep_rel == 'pobj')])) / (length(token_id[(tag == 'PRP' ) & (dep_rel == 'dobj' | dep_rel == 'pobj')])),
                               agency_try000 = (length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine') & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])))
  df <- dplyr::left_join(df, spacy_counts2, by = c("ID" = "doc_id"))
  df$RBR <- df$RBR/ df$WC
  df$nummod <- df$nummod/ df$WC
  df$PDT <- df$PDT/ df$WC
  df$JJR <- df$JJR/ df$WC
  df$appos <- df$appos/ df$WC
  df$PROPN <- df$PROPN/ df$WC
  df$intj <- df$intj/ df$WC
  df$INTJ.x <- df$INTJ.x/ df$WC
  df$dative <- df$dative/ df$WC
  df$nsubjpass <- df$nsubjpass/ df$WC
  df$mid <- df$mid/ df$WC

  #regressive imagery features, social word feature, verb features
  wanted <- c("regex")
  cold <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "cold",][wanted]
  cold <- as.vector(cold$regex)
  cold <- cold[!is.na(cold)]
  passivity <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "passivity",][wanted]
  passivity <- as.vector(passivity$regex)
  passivity <- passivity[!is.na(passivity)]
  descent <- lexicon::key_regressive_imagery[lexicon::key_regressive_imagery$subcategory == "descent",][wanted]
  descent <- as.vector(descent$regex)
  descent <- descent[!is.na(descent)]
  df$descent<- 0
  df$cold<- 0
  df$passivity <- 0
  df$linking_verbs <- 0
  df$helping_verbs <- 0
  df$environmental_words <- 0
  df$social_words <- 0
  df$education_words <- 0
  for (i in 1:nrow(df)) {
    for (j in 1:length(cold)) {
      if (grepl(cold[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$cold[i] <- df$cold[i] + 1) }
    for (j in 1:length(descent)) {
      if (grepl(descent[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$descent[i] <- df$descent[i] + 1) }
    for (j in 1:length(passivity)) {
      if (grepl(passivity[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$passivity[i] <- df$passivity[i] + 1) }
    for (j in 1:length(social_words$WORDS)) {
      if (grepl(social_words$WORDS[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$social_words[i] <- df$social_words[i] + 1) }
    for (j in 1:length(environmental_words$WORDS)) {
      if (grepl(environmental_words$WORDS[j], df$text[i], perl = TRUE, ignore.case = TRUE)) (df$environmental_words[i] <- df$environmental_words[i] + 1) }
    for (j in 1:length(education_words$WORDS)) {
      if (grepl(education_words$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$education_words[i] <- df$education_words[i] + 1)}}
    for (j in 1:length(linking_verbs$WORDS)) {
      if (grepl(linking_verbs$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$linking_verbs[i] <- df$linking_verbs[i] + 1)} }
    for (j in 1:length(helping_verbs$WORDS)) {
      if (grepl(helping_verbs$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$helping_verbs[i] <- df$helping_verbs[i] + 1)} }}
  df$cold <- df$cold/ df$WC
  df$descent <- df$descent/ df$WC
  df$passivity <- df$passivity/ df$WC
  df$social_words <- df$social_words/ df$WC
  df$helping_verbs <- df$helping_verbs/ df$WC
  df$linking_verbs <- df$linking_verbs/ df$WC
  df$education_words <- df$education_words / df$WC

  #single word norms
  single_df <- dplyr::inner_join(tidy_norms_clean, single_words_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  single_scores <- plyr::ddply(single_df,.(ID),plyr::summarize, HAL = sum(HAL, na.rm = TRUE))
  df <- dplyr::left_join(df, single_scores, by = c("ID" = "ID"))


  #bundles & qdap dictionaries
  bundle_2 <- c("years", "year", "name")
  bundle_qualifiers <- c("pretty", "less", "least", "just",
                         "somewhat", "more", "too", "so", "just", "enough",
                         "indeed", "still", "almost", "fairly",  "even",
                         "bit", "little")
  tidy_norms_clean$bundle_2 <- 0
  tidy_norms_clean$bundle_qualifiers <- 0
  tidy_norms_clean$amplification <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_qualifiers) (tidy_norms_clean$bundle_qualifiers[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% qdapDictionaries::amplification.words) (tidy_norms_clean$amplification[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_2) (tidy_norms_clean$bundle_2[i] =  1)}
  words_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                              qualifiersC = sum(bundle_qualifiers, na.rm = TRUE),
                              amplification = sum(amplification, na.rm = TRUE),
                              bundle_2C = sum(bundle_2, na.rm = TRUE))
  words_scores$bundle_2C <- words_scores$bundle_2C/ nrow(tidy_norms_clean)
  words_scores$qualifiersC <- words_scores$qualifiersC/ nrow(tidy_norms_clean)
  df <- dplyr::left_join(df, words_scores, by = c("ID" = "ID"))
  df$amplification <- df$amplification/ df$WC

  # Emotion
  emotion <- sentimentr::emotion_by(df$text, emotion_dt = lexicon::hash_nrc_emotions,
                                    valence_shifters_dt = lexicon::hash_valence_shifters,
                                    drop.unused.emotions = FALSE, un.as.negation = TRUE,
                                    n.before = 5, n.after = 2)
  emotion_long <- subset(emotion, select = c("element_id", "emotion_type", "ave_emotion"))
  emotion_wide <- tidyr::spread(emotion_long, emotion_type, ave_emotion)
  emotion_wide$element_id <- NULL
  emotion_cols <- emotion_wide[,c("disgust","disgust_negated", "surprise","surprise_negated")]
  df <- cbind(df, emotion_cols)
  df$disgust_difference <- df$disgust - df$disgust_negated
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
  equality <- qdapDictionaries::discourse.markers.alemany$marker[which(qdapDictionaries::discourse.markers.alemany$type == "equality")]
  tidy_norms_clean$equality <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tidy_norms_clean$word[i] %in% equality) (tidy_norms_clean$equality[i] =  1)
  }
  discourse_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                                  equality = sum(equality, na.rm = TRUE))
  ref <- as.data.frame(df$ID)
  names(ref)[names(ref) == 'df$ID'] <- 'ID'
  ref$equality2 <- 0
  for (j in 1:length(equality)) {
    if (sapply(strsplit(equality[j], "\\s+"), length) > 1) {
      for (i in 1:nrow(df)) {
        if (grepl(equality[j], tolower(df$text[i]), ignore.case = TRUE)) {(ref$equality2[i] <- ref$equality2[i] + 1)}}}}
  discourse_scores <- dplyr::inner_join(ref, discourse_scores, by = c("ID" = "ID"))
  discourse_scores$equality_ALL <- discourse_scores$equality + discourse_scores$equality2
  vars <- c("ID", "equality_ALL")
  discourse_scores_short <- discourse_scores[vars]
  df <- dplyr::left_join(df, discourse_scores_short, by = c("ID" = "ID"))
  df$equality_ALL <- df$equality_ALL / df$WC

  # The Warmth and Competence Word codings by RA
  W_C_df <- dplyr::inner_join(tidy_norms_clean, W_C_ratings, by = c("word" = "Word"), ignore_case = TRUE)
  Negative_Warm <- W_C_df[W_C_df$Warmth.Rating == '-1',]
  Negative_Warm_Scores <- plyr::ddply(Negative_Warm,.(ID),plyr::summarize,Negative_Warm = sum(Warmth.Rating, na.rm = TRUE))
  df <- dplyr::left_join(df, Negative_Warm_Scores, by = c("ID" = "ID"))
  df$Negative_Warm <- df$Negative_Warm/ df$WC

  #Hello and Please
  Hello_words <- c("hi","hello","hey")
  Please_words <- c("please")
  tidy_norms_clean$Hello <- 0
  tidy_norms_clean$Please <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tolower(tidy_norms_clean$word[i]) %in% Hello_words) (tidy_norms_clean$Hello[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% Please_words) (tidy_norms_clean$Please[i] =  1)
  }
  Hello_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                              Hello.y = sum(Hello, na.rm = TRUE),
                              Please = sum(Please, na.rm = TRUE))
  df <- dplyr::left_join(df, Hello_scores, by = c("ID" = "ID"))

  # Running pre-trained model
  competence_features <- df[,c("agency_try000", "nummod", "qualifiersC", "VBZ.y", "INTJ.x", "target5", "intj", "pre_adv2_subj", "DT.y",
                               "post_PRON1_main", "linking_verbs", "PDT", "pre_PRON2_ROOT", "surprise_difference", "appos", "amplification",
                               "post_DT_adv1_main", "PROPN", "rel_dist36a", "Please", "descent", "pre_VBZ_VERB2_ROOT", "RBR", "pre_adv2_main",
                               "disgust_difference", "pre_DT_adv1_ROOT", "post_adj2_ROOT", "dative", "nsubjpass", "targetK", "JJR", "For.Me",
                               "post_VERB1_subj", "helping_verbs", "equality_ALL", "environmental_words", "cold", "c_target2",
                               "post_NNS_NOUN2_subj", "Negative_Warm", "bundle_2C", "targetK2", "social_words", "mid", "passivity",
                               "pre_TO_PART1_main", "negative_pol", "n_before_subj", "Hello.y", "post_NOUN2_ROOT", "HAL", "education_words")]
  y <- as.data.frame(colnames(competence_features))
  competence_features <-  raster::as.matrix(competence_features)
  competence_features[is.infinite(competence_features)] <- 0
  competence_features[is.na(competence_features)] <- 0
  suppressWarnings(preprocessParams1<-caret::preProcess(competence_features, method = c("center", "scale")))
  competence_features1 <- stats::predict(preprocessParams1, competence_features)
  competence_predictions <- competence_enet_final %>% stats::predict(competence_features1)
  competence_features_output <-competence_features1
  colnames(competence_features_output) <- c("i_subject","num_mod","qualifiers","3rd_person_verb","interjections_total","personal_pronouns",
                                            "interjections_superfluous","pre_subject_adverbs","determiners","post_phrase_verb_pronouns","linking_verbs",
                                            "predeterminers","pre_sentence_verb_pronouns","surprise","appositional_modifiers","amplification_words",
                                            "post_phrase_verb_determiners","proper_nouns","difference_subject_possession","please","descent",
                                            "pre_sentence_verb_3rd_person_verbs","comparative_adverbs","pre_phrase_verb_adverbs","disgust",
                                            "pre_sentence_verb_determiners","post_sentence_verb_adjectives","datives","passive_nominal_subjects",
                                            "you_us_subject_total","comparative_adjectives","for_me","post_subject_verbs","helping_verbs",
                                            "equality_discourse","environmental_words","cold","you_us_subject_object","post_subject_plural_nouns",
                                            "negative_warmth","introduction_words","you_us_pronouns","svo_words","noun_phrases","passivity","infinitives",
                                            "negative_polarity","n_before_subj","hello","post_sentence_verb_nouns","cue_word_frequency","education_words")
  df$competence_predictions <- competence_predictions
  # return
  if(metrics[1] == "features"){
    dataout=cbind(ID = df$ID, as.data.frame(competence_features1))
  }
  if(metrics[1] == "all"){
    dataout=cbind(ID = df$ID, competence_predictions = df$competence_predictions, as.data.frame(competence_features1))
  }
  if(metrics[1] == "scores"){
    dataout=df[, c("ID", "competence_predictions")]
  }

   return(dataout)
}
