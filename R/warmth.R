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
#' Rinker, T. W. (2018). lexicon: Lexicon Data version 1.2.1. http://github.com/trinker/lexicon
#' Moss, T. W., Renko, M., Block, E., & Meyskens, M. (2018). Funding the story of hybrid ventures: Crowdfunder lending preferences and linguistic hybridity. Journal of Business Venturing, 33(5), 643-659.
#' Buchanan, E. M., Valentine, K. D., & Maxwell, N. P. (2018). LAB: Linguistic Annotated Bibliography - Shiny Application. Retrieved from http://aggieerin.com/shiny/lab_table.
#' Rinker, T. W. (2019). sentimentr: Calculate Text Polarity Sentiment version 2.7.1. http://github.com/trinker/sentimentr
#' Boyd, R. L. (2017). TAPA: Textual Affective Properties Analyzer (v.1.1.0) [Software]. Available from https://www.ryanboyd.io/software/tapa
#'
#'@examples
#'
# data("vignette_data")
#
# warmth_scores <- warmth(data$Message, metrics = "all")
#
# vignette_data2$warmth_predictions <- warmth_scores$warmth_predictions
#
# warmth_model1 <- lm(RA_warm_AVG  ~ warmth_predictions, data = vignette_data2)
# summary(warmth_model1)
# vignette_data2$condition <- as.factor(vignette_data2$condition)
# warmth_model2 <- glm(condition ~ warmth_predictions, family = "binomial", data = vignette_data2)
# summary(warmth_model2)
#'
#'@export
warmth <- function(text, ID=NULL, metrics = c("scores", "features", "all")){
  if(is.null(ID)){
    ID=as.character(1:length(text))
  }
  #For CRAN check
  doc_id<- sentence_id<- token_id<- head_token_id<- tag<- pos <-dep_rel<- token<- nounphrase <- agency_try1000<- agency_try3<- NULL
  HAL<- emotion_type<- ave_emotion<- Hello<- Please<- NULL
  mental_verbs = mental_verbs
  single_words_dic  = single_words_dic
  warmth_enet_final_reduced = warmth_enet_final_reduced

  #word count and text objects
  df <- data.frame(text, ID)
  df$WC <- apply(df %>% dplyr::select(text), 1, ngram::wordcount)
  tbl <- tibble::as_tibble(
    data.frame(doc_id = ID, text = text, stringsAsFactors = F)
  )
  try <- spacyr::spacy_parse(tbl, tag = TRUE, dependency = TRUE, nounphrase = TRUE,entity=FALSE)
  tidy_norms_clean <- words_clean(text, ID)
  df_corpus <- quanteda::corpus(df$text, docnames = df$ID)
  df_dfm <- dfm(
    tokens(df_corpus),
    tolower = TRUE,
    select = NULL,
    remove = NULL,
    dictionary = NULL,
    thesaurus = NULL)

  #politeness
  df_politeness <- politeness::politeness(df$text, parser="spacy", drop_blank = TRUE, metric = "average")
  df$Negation <- if (!is.null(df_politeness$Negation)) {df$Negation <- df_politeness$Negation} else {df$Negation <- 0}
  df$Positive.Emotion <- if (!is.null(df_politeness$Positive.Emotion)) {df$Positive.Emotion <- df_politeness$Positive.Emotion} else {df$Positive.Emotion <- 0}
  df$Gratitude <- if (!is.null(df_politeness$Gratitude)) {df$Gratitude <- df_politeness$Gratitude} else {df$Gratitude <- 0}

  #sentiment
  sentiment <- sentimentr::sentiment_by(df$text)
  df$ave_sentiment <- sentiment$ave_sentiment

  #sentence level spacy features
  suppressWarnings(spacy_new2A <- plyr::ddply(try, .(doc_id, sentence_id), dplyr::summarise,
                                              DT.y = (length(token_id[tag == 'DT'])/ length(token_id)),
                                              phrase_length = mean((token_id - head_token_id)/ max(token_id)),
                                              post_NNS_NOUN1_ROOT = (length(token_id[tag == 'NNS' & token_id > token_id[dep_rel == "ROOT"]])/ length(token_id[pos == 'NOUN']))))
  options(dplyr.summarise.inform = FALSE)
  spacy_new2B <- spacy_new2A %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(dplyr::across(DT.y:post_NNS_NOUN1_ROOT, mean, na.rm = T))
  df <- dplyr::left_join(df, spacy_new2B, by = c("ID" = "doc_id"))

  #message level spacy features
  spacy_counts2 <- plyr::ddply(try, .(doc_id), plyr::summarize,
                               agency_try1000 = (length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine'| token == 'you'| token == 'yours')
                                                                 & (dep_rel == 'dobj' | dep_rel == 'pobj')])),
                               agency_try3 = (length(token_id[(tag == 'PRP')])),
                               agency_target3000 = agency_try1000/agency_try3,
                               INTJ.x = sum(pos == 'INTJ'),
                               agency_try000 = (length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine') & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])))
  df <- dplyr::left_join(df, spacy_counts2, by = c("ID" = "doc_id"))
  df$INTJ.x <- df$INTJ.x/ df$WC


  #verb features
  df$mental_verbs <- 0
  for (i in 1:nrow(df)) {
    for (j in 1:length(mental_verbs$WORDS)) {
      if (grepl(mental_verbs$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$mental_verbs[i] <- df$mental_verbs[i] + 1)} }}
  df$mental_verbs <- df$mental_verbs/ df$WC

  #single word norms
  single_df <- dplyr::inner_join(tidy_norms_clean, single_words_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  single_scores <- plyr::ddply(single_df,.(ID),plyr::summarize, HAL = sum(HAL, na.rm = TRUE))
  df <- dplyr::left_join(df, single_scores, by = c("ID" = "ID"))

  #bundles
  bundle_5 <- c("hope", "love", "like", "support", "enjoy")
  bundle_qualifiers <- c("pretty", "less", "least", "just",
                         "somewhat", "more", "too", "so", "just", "enough",
                         "indeed", "still", "almost", "fairly",  "even",
                         "bit", "little")
  tidy_norms_clean$bundle_5 <- 0
  tidy_norms_clean$bundle_qualifiers <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_5) (tidy_norms_clean$bundle_5[i] =  1)
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_qualifiers) (tidy_norms_clean$bundle_qualifiers[i] =  1)}
  words_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                              bundle_5C = sum(bundle_5, na.rm = TRUE),
                              qualifiersC = sum(bundle_qualifiers, na.rm = TRUE))
  words_scores$bundle_5C <- words_scores$bundle_5C/ nrow(tidy_norms_clean)
  words_scores$qualifiersC <- words_scores$qualifiersC/ nrow(tidy_norms_clean)
  df <- dplyr::left_join(df, words_scores, by = c("ID" = "ID"))

  # Emotion
  emotion <- sentimentr::emotion_by(df$text, emotion_dt = lexicon::hash_nrc_emotions,
                                    valence_shifters_dt = lexicon::hash_valence_shifters,
                                    drop.unused.emotions = FALSE, un.as.negation = TRUE,
                                    n.before = 5, n.after = 2)
  emotion_long <- subset(emotion, select = c("element_id", "emotion_type", "ave_emotion"))
  emotion_wide <- tidyr::spread(emotion_long, emotion_type, ave_emotion)
  emotion_wide$element_id <- NULL
  names(emotion_wide)[names(emotion_wide) == 'anger'] <- 'sentiment_anger'
  emotion_cols <- emotion_wide[,c("fear","fear_negated", "sentiment_anger")]
  df <- cbind(df, emotion_cols)
  df$fear_difference <- df$fear - df$fear_negated

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

  #Readability
  readability <- quanteda.textstats::textstat_readability(df_corpus, measure = "Dale.Chall.old")
   # remove_hyphens = TRUE,intermediate = FALSE)
  readability$document <- NULL
  df <- cbind(df, readability)

  # Lexical Diversity
  lexdiv <- quanteda.textstats::textstat_lexdiv(df_dfm,
                                                measure = "all",
                                                remove_numbers = TRUE,
                                                remove_punct = TRUE,
                                                remove_symbols = TRUE,
                                                remove_hyphens = FALSE,
                                                log.base = 10,
                                                MATTR_window = 100L,
                                                MSTTR_segment = 100L)
  lexdiv$document <- NULL
  df <- cbind(df, lexdiv)

  # Running pre-trained model
  warmth_features <- df[,c("mental_verbs","agency_target3000","Dale.Chall.old","INTJ.x","phrase_length","post_NNS_NOUN1_ROOT",
                           "fear_difference","HAL","qualifiersC","agency_try000","Please","DT.y","Hello.y","Positive.Emotion","Gratitude",
                           "bundle_5C","ave_sentiment","R","sentiment_anger","Negation")]
  warmth_features <-  raster::as.matrix(warmth_features)
  warmth_features[is.infinite(warmth_features)] <- 0
  warmth_features[is.na(warmth_features)] <- 0
  suppressWarnings(preprocessParams1<-caret::preProcess(warmth_features, method = c("center", "scale")))
  warmth_features1 <- stats::predict(preprocessParams1, warmth_features)
  warmth_predictions <- warmth_enet_final_reduced %>% raster::predict(warmth_features1)
  warmth_features_output <- warmth_features1
  colnames(warmth_features_output) <- c("mental_verbs","dale_chall_readability","you_me_objects","interjections_total",
                                        "phrase_length","post_sentence_verb_plural_nouns","fear","i_subject","determiners",
                                        "cue_word_frequency","please","hello","qualifiers","positive_emotion","gratitude",
                                        "postive_verbs","sentiment","lexical_diversity","anger","negation")
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




