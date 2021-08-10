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
#' Rinker, T. W. (2019). sentimentr: Calculate Text Polarity Sentiment version 2.7.1. http://github.com/trinker/sentimentr
#' Boyd, R. L. (2017). TAPA: Textual Affective Properties Analyzer (v.1.1.0) [Software]. Available from https://www.ryanboyd.io/software/tapa
#'
#' @examples
#'
# data("vignette_data")
#
# competence_scores <- competence(data$Message, metrics = "all")
#
# data$competence_predictions <- competence_scores$competence_predictions
#
# competence_model1 <- lm(RA_comp_AVG  ~ competence_predictions, data = vignette_data2)
# summary(competence_model1)
# vignette_data2$condition <- as.factor(vignette_data2$condition)
# competence_model2 <- glm(condition ~ competence_predictions, family = "binomial", data = vignette_data2)
# summary(competence_model2)
#'
#'@export
competence<- function(text, ID=NULL, metrics = c("scores", "features", "all")){
  if(is.null(ID)){
    ID=as.character(1:length(text))
  }
  #For CRAN check
  doc_id <- sentence_id<- tag<- token_id<- pos<- head_token_id<- dep_rel<- VBZ.y<- nounphrase<- token<- NULL
  HAL<- emotion_type<- ave_emotion<- Warmth.Rating<- Hello<- NULL
  education_words  = education_words
  single_words_dic  = single_words_dic
  W_C_ratings  = W_C_ratings
  competence_enet_final_reduced = competence_enet_final_reduced

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

  #sentence level spacy features
  suppressWarnings(spacy_new2A <- plyr::ddply(try, .(doc_id, sentence_id), plyr::summarize,
                                              VBZ.y = sum(tag == 'VBZ')/ length(token_id),
                                              pre_adv2_subj = (length(token_id[pos == 'ADV' & token_id < token_id[dep_rel == "nsubj"]])/ length(token_id))))
  options(dplyr.summarise.inform = FALSE)
  spacy_new2B <- spacy_new2A %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(dplyr::across(VBZ.y:pre_adv2_subj, mean, na.rm = T))
  df <- dplyr::left_join(df, spacy_new2B, by = c("ID" = "doc_id"))
  #message level spacy features
  spacy_counts2 <- plyr::ddply(try, .(doc_id), plyr::summarize,
                               nummod = sum(dep_rel == 'nummod'),
                               INTJ.x = sum(pos == 'INTJ'),
                               PROPN = sum(pos == 'PROPN'),
                               agency_try000 = (length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine') & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])))
  df <- dplyr::left_join(df, spacy_counts2, by = c("ID" = "doc_id"))
  df$nummod <- df$nummod/ df$WC
  df$PROPN <- df$PROPN/ df$WC
  df$INTJ.x <- df$INTJ.x/ df$WC

  #education words
  df$education_words <- 0
  for (i in 1:nrow(df)) {
    for (j in 1:length(education_words$WORDS)) {
      if (grepl(education_words$WORDS[j], tolower(df$text[i]), ignore.case = TRUE, perl = TRUE)) {(df$education_words[i] <- df$education_words[i] + 1)}}}
  df$education_words <- df$education_words / df$WC

  #single word norms
  single_df <- dplyr::inner_join(tidy_norms_clean, single_words_dic, by = c("word" = "Symbol"), ignore_case = TRUE)
  single_scores <- plyr::ddply(single_df,.(ID),plyr::summarize, HAL = sum(HAL, na.rm = TRUE))
  df <- dplyr::left_join(df, single_scores, by = c("ID" = "ID"))


  #bundles
  bundle_qualifiers <- c("pretty", "less", "least", "just",
                         "somewhat", "more", "too", "so", "just", "enough",
                         "indeed", "still", "almost", "fairly",  "even",
                         "bit", "little")
  tidy_norms_clean$bundle_qualifiers <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_qualifiers) (tidy_norms_clean$bundle_qualifiers[i] =  1)}
  words_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                              qualifiersC = sum(bundle_qualifiers, na.rm = TRUE))
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
  emotion_cols <- emotion_wide[,c("anger_negated", "sentiment_anger")]
  df <- cbind(df, emotion_cols)

  # The Warmth and Competence Word codings by RA
  W_C_df <- dplyr::inner_join(tidy_norms_clean, W_C_ratings, by = c("word" = "Word"), ignore_case = TRUE)
  Negative_Warm <- W_C_df[W_C_df$Warmth.Rating == '-1',]
  Negative_Warm_Scores <- plyr::ddply(Negative_Warm,.(ID),plyr::summarize,Negative_Warm = sum(Warmth.Rating, na.rm = TRUE))
  df <- dplyr::left_join(df, Negative_Warm_Scores, by = c("ID" = "ID"))
  df$Negative_Warm <- df$Negative_Warm/ df$WC

  #Hello
  Hello_words <- c("hi","hello","hey")
  tidy_norms_clean$Hello <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tolower(tidy_norms_clean$word[i]) %in% Hello_words) (tidy_norms_clean$Hello[i] =  1)
  }
  Hello_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                              Hello.y = sum(Hello, na.rm = TRUE))
  df <- dplyr::left_join(df, Hello_scores, by = c("ID" = "ID"))

  #Readability
  readability <- quanteda.textstats::textstat_readability(df_corpus, measure = c("Coleman.C2", "Wheeler.Smith"))
                                                  #        remove_hyphens = TRUE,intermediate = FALSE)
  readability$document <- NULL
  df <- cbind(df, readability)

  readability $Coleman.C2[8]

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
  competence_features <- df[,c("anger_negated","sentiment_anger","Wheeler.Smith","Coleman.C2","R","Negation","education_words","HAL",
                               "Hello.y","Negative_Warm", "PROPN","pre_adv2_subj","INTJ.x","VBZ.y", "qualifiersC","nummod","agency_try000")]
  y <- as.data.frame(colnames(competence_features))
  competence_features <-  raster::as.matrix(competence_features)
  competence_features[is.infinite(competence_features)] <- 0
  competence_features[is.na(competence_features)] <- 0
  suppressWarnings(preprocessParams1<-caret::preProcess(competence_features, method = c("center", "scale")))
  competence_features1 <- stats::predict(preprocessParams1, competence_features)
  competence_predictions <- competence_enet_final_reduced %>% stats::predict(competence_features1)
  competence_features_output <-competence_features1
  colnames(competence_features_output) <- c("anger_negated","anger","wheeler_smith_readability","coleman_readability","lexical_diversity","negation",
                                            "education_words","cue_word_frequency","hello","negative_warmth","proper_nouns","pre_subject_adverbs","interjections_total",
                                            "3rd_person_verb","qualifiers","num_mod","i_subject")
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

