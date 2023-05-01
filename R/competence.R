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
#' @details Some features depend Spacyr which must be installed separately in Python.
#' @return The default is to return a data.frame with each row containing the document identifier and the competence score.
#'     Users can also customize what is returned through the metrics argument. If metrics = "features", then a dataframe of competence features will be
#'     returned where each document is represented by a row. If metrics = "all", then both the competence scores and features will be returned in a data.frame.
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
#' @examples
#' data("example_data")
#'
#' competence_scores <- competence(example_data$bio, metrics = "all")
#'
#' example_data$competence_predictions <- competence_scores$competence_predictions
#' competence_model1 <- lm(RA_comp_AVG  ~ competence_predictions, data = example_data)
#' summary(competence_model1)
#
#'
#'@export
competence<- function(text, ID=NULL, metrics = c("scores", "features", "all")){
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
  df_dfm <- quanteda::dfm(
    quanteda::tokens(df_corpus),
    tolower = TRUE,
    select = NULL,
    remove = NULL,
    dictionary = NULL,
    thesaurus = NULL)


  #politeness
  df_politeness <- politeness::politeness(df$text, parser="spacy", drop_blank = TRUE, metric = "average")
  df$First.Person.Single <- if (!is.null(df_politeness$First.Person.Single)) {df$First.Person.Single <- df_politeness$First.Person.Single} else {df$First.Person.Single <- 0}
  df$Negative.Emotion <- if (!is.null(df_politeness$Negative.Emotion)) {df$Negative.Emotion <- df_politeness$Negative.Emotion} else {df$Negative.Emotion <- 0}


  ## Word Lists by Tidy
  tidy_norms_clean$tone_neg_words <- 0
  tidy_norms_clean$Prevention_words <- 0
  tidy_norms_clean$forward_words <- 0

  bundle_1 <- c("as", "an", "while", "in")
  tidy_norms_clean$bundle_1 <- 0

  for (i in 1:nrow(tidy_norms_clean)) {
    if (tolower(tidy_norms_clean$word[i]) %in% tone_neg_words$WORDS)
      tidy_norms_clean$tone_neg_words[i] =  1
    if (tolower(tidy_norms_clean$word[i]) %in% Prevention_words$WORDS)
      tidy_norms_clean$Prevention_words[i] =  1
    if (tolower(tidy_norms_clean$word[i]) %in% forward_words$WORDS)
      tidy_norms_clean$forward_words[i] =  1
    if (tolower(tidy_norms_clean$word[i]) %in% bundle_1)
      tidy_norms_clean$bundle_1[i] =  1
  }


  tidy_words_scores <- plyr::ddply(tidy_norms_clean,.(ID),dplyr::summarise,
                                   tone_neg_words = sum(tone_neg_words, na.rm = TRUE),
                                   Prevention_words = sum(Prevention_words, na.rm = TRUE),
                                   forward_words = sum(forward_words, na.rm = TRUE),
                                   bundle_1C = sum(bundle_1, na.rm = TRUE)/(nrow(tidy_norms_clean))
  )
  df <- dplyr::left_join(df, tidy_words_scores, by = c("ID" = "ID"))
  ##purposefully not controlling prevention by WC
  df$tone_neg_words <- df$tone_neg_words/ df$WC
  df$forward_words <- df$forward_words/ df$WC

  #message level spacy features
  spacy_counts2 <- plyr::ddply(try, .(doc_id), plyr::summarize,
                               advmod = sum(dep_rel == 'advmod'),
                               punct = sum(dep_rel == 'punct'),
                               TO = sum(tag == 'TO'),
                               ADP = sum(pos == 'ADP'),
                               ROOT = sum(dep_rel == 'ROOT'),
                               agency_target2 = ((length(token_id[(tag == 'PRP') & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])))/(length(token_id[(tag == 'PRP')])),
                               agency_target200 = ((length(token_id[(token == 'i' | token == 'i\'m' | token == 'me'|  token == 'mine') & (dep_rel == 'nsubj' | dep_rel == 'csubj' | dep_rel == 'csubjpass' | dep_rel == 'nsubjpass' )])))/(length(token_id[(tag == 'PRP')]))
  )


  df <- dplyr::left_join(df, spacy_counts2, by = c("ID" = "doc_id"))
  df$advmod <- df$advmod / df$WC
  df$punct <- df$punct / df$WC
  df$TO <- df$TO / df$WC
  df$ADP <- df$ADP/ df$WC
  df$ROOT <- df$ROOT / df$WC

  #sentence level spacy features
  suppressWarnings(spacy_new2A <- plyr::ddply(try, .(doc_id, sentence_id), plyr::summarize,
                                              post_JJS_ADJ2_subj = (length(token_id[tag == 'JJS' & token_id > token_id[dep_rel == "nsubj"]])/ length(token_id)),
                                              pre_NOUN2_ROOT = (length(token_id[pos == 'NOUN' & token_id < token_id[dep_rel == "ROOT"]])/ length(token_id))
  ))
  options(dplyr.summarise.inform = FALSE)
  spacy_new2B <- spacy_new2A %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(dplyr::across(post_JJS_ADJ2_subj:pre_NOUN2_ROOT, mean, na.rm = T))
  df <- dplyr::left_join(df, spacy_new2B, by = c("ID" = "doc_id"))

  ##Competence Codings
  W_C_df <- dplyr::inner_join(tidy_norms_clean, W_C_ratings, by = c("word" = "Word"), relationship = "many-to-many")
  Positive_Comp <- W_C_df[W_C_df$'Competence Rating' == '1',]
  Positive_Comp$Competence_Rating <- Positive_Comp$'Competence Rating'
  Positive_Comp_Scores <- plyr::ddply(Positive_Comp,.(ID),plyr::summarize,Positive_Comp = sum(Competence_Rating, na.rm = TRUE))
  df <- dplyr::left_join(df, Positive_Comp_Scores, by = c("ID" = "ID"))
  df$Positive_Comp <- df$Positive_Comp/ df$WC

  # Emotion
  emotion <-  suppressWarnings(sentimentr::emotion_by(df$text, emotion_dt = lexicon::hash_nrc_emotions,
                                                      valence_shifters_dt = lexicon::hash_valence_shifters,
                                                      drop.unused.emotions = FALSE, un.as.negation = TRUE,
                                                      n.before = 5, n.after = 2))
  emotion_long <- subset(emotion, select = c("element_id", "emotion_type", "ave_emotion"))
  emotion_wide <- tidyr::spread(emotion_long, emotion_type, ave_emotion)
  emotion_wide$element_id <- NULL
  emotion_wide$anger_difference <- emotion_wide$anger - emotion_wide$anger_negated
  emotion_cols <- emotion_wide[,c("surprise_negated","anger_difference")]
  df <- cbind(df, emotion_cols)

  ##Norms
  AoA_df <- dplyr::inner_join(tidy_norms_clean, AoA_dic, by = c("word" = "Symbol"))
  AoA_scores <- plyr::ddply(AoA_df,.(ID),plyr::summarize,
                            AoA_Rating = sum(AoA_Rating, na.rm = TRUE))
  df <- dplyr::left_join(df, AoA_scores, by = c("ID" = "ID"))
  df$AoA_Rating <- df$AoA_Rating/ df$WC

  single_df <- dplyr::inner_join(tidy_norms_clean, single_words_dic, by = c("word" = "Symbol"))
  single_scores <- plyr::ddply(single_df,.(ID),plyr::summarize, Ortho = sum(Ortho, na.rm = TRUE))
  df <- dplyr::left_join(df, single_scores, by = c("ID" = "ID"))
  df$Ortho <- df$Ortho/ df$WC

  ##LabMT
  labMT_values <- dplyr::inner_join(tidy_norms_clean, qdapDictionaries::labMT, by = c("word" = "word"))
  labMT_values <- plyr::ddply(labMT_values,.(ID),plyr::summarize,
                              happiness_rank = sum(happiness_rank, na.rm = TRUE))
  df <- dplyr::left_join(df, labMT_values, by = c("ID" = "ID"))
  df$happiness_rank <- df$happiness_rank / df$WC

  ##Discourse Markers
  vague <- qdapDictionaries::discourse.markers.alemany$marker[which(qdapDictionaries::discourse.markers.alemany$type == "vague")]
  tidy_norms_clean$vague <- 0
  for (i in 1:nrow(tidy_norms_clean)) {
    if (tidy_norms_clean$word[i] %in% vague)
    {tidy_norms_clean$vague[i] =  1}
  }
  discourse_scores <- plyr::ddply(tidy_norms_clean,.(ID),plyr::summarize,
                                  vague = sum(vague, na.rm = TRUE))

  ref <- as.data.frame(df$ID)
  names(ref)[names(ref) == 'df$ID'] <- 'ID'
  ref$vague2 <- 0
  for (j in 1:length(vague)) {
    if (sapply(strsplit(vague[j], "\\s+"), length) > 1) {
      for (i in 1:nrow(df)) {
        if (grepl(vague[j], tolower(df$text[i]), ignore.case = TRUE))
        {(ref$vague2[i] <- ref$vague2[i] + 1)}
      }
    }
  }
  discourse_scores <- dplyr::inner_join(ref, discourse_scores, by = c("ID" = "ID"))
  discourse_scores$vague_ALL <- discourse_scores$vague + discourse_scores$vague2
  vars <- c("ID",  "vague_ALL")
  discourse_scores_short <- discourse_scores[vars]
  df <- dplyr::left_join(df, discourse_scores_short, by = c("ID" = "ID"))
  df$vague_ALL <- df$vague_ALL / df$WC

  #Power
  temp_power <- qdapDictionaries::key.power
  colnames(temp_power)[2] <- "y_power"
  key_power <- dplyr::inner_join(tidy_norms_clean, temp_power, by = c("word" = "x"))
  key_power <- plyr::ddply(key_power,.(ID, y_power),plyr::summarize, key_power = sum(y_power, na.rm = TRUE))
  positive_power <- key_power[key_power$y == '1',]
  wanted <- c("ID", "key_power")
  positive_power <- positive_power[wanted]
  colnames(positive_power)[2] <- "positive_power"
  df <- dplyr::left_join(df, positive_power, by = c("ID" = "ID"))
  df$positive_power <- df$positive_power/ df$WC

  # Sentiment
  sentiment <-  suppressWarnings(sentimentr::sentiment_by(df$text))
  df$ave_sentiment <- sentiment$ave_sentiment

  #Readability
  readability <- quanteda.textstats::textstat_readability(df_corpus, measure = c("Coleman.C2", "Fucks"),
                                                          remove_hyphens = TRUE,intermediate = FALSE)
  readability$document <- NULL
  df <- cbind(df, readability)

  # Lexical Diversity
  df_short <- df[which(df$text != ""),]
  diversity <- qdap::diversity(df_short$text, grouping.var = df_short$ID)
  shannon <- diversity[,c("ID", "shannon")]
  df <- dplyr::left_join(df, shannon, by = c("ID" = "ID"))

  lexdiv <- quanteda.textstats::textstat_lexdiv(df_dfm,
                                                measure = "D",
                                                remove_numbers = TRUE,
                                                remove_punct = TRUE,
                                                remove_symbols = TRUE,
                                                remove_hyphens = FALSE,
                                                log.base = 10,
                                                MATTR_window = 100L,
                                                MSTTR_segment = 100L)
  df <- dplyr::left_join(df, lexdiv, by = c("ID" = "document"))

  #TFIDF-based Bundle
  message_dfm_tfidf<- quanteda::dfm_tfidf(df_dfm)
  bundle_2 <- c("years", "year", "name")
  feat <- quanteda::dfm_select(message_dfm_tfidf, bundle_2)
  feat2 <- quanteda::convert(feat, to = 'data.frame')
  feat2$years <- ifelse("years" %in% colnames(feat2), feat2$years, 0 )
  feat2$year <- ifelse("year" %in% colnames(feat2), feat2$year, 0 )
  feat2$name <- ifelse("name" %in% colnames(feat2), feat2$name, 0 )
  feat2$bundle_2 <- rowMeans(feat2[,bundle_2])
  feat3 <- feat2[,c("doc_id", "bundle_2")]
  df <- dplyr::left_join(df, feat3, by = c('ID' = 'doc_id'))


  # Running pre-trained model
  competence_features <- df[,c("tone_neg_words","First.Person.Single","agency_target2","Prevention_words","advmod","surprise_negated","punct","AoA_Rating","post_JJS_ADJ2_subj","pre_NOUN2_ROOT","TO",
                               "happiness_rank","ADP","Ortho","vague_ALL","forward_words","Positive_Comp","positive_power","agency_target200","ave_sentiment","anger_difference","ROOT","Coleman.C2",
                               "Negative.Emotion","D","shannon","Fucks","bundle_1C","bundle_2")]
  y <- as.data.frame(colnames(competence_features))
  competence_features <-  raster::as.matrix(competence_features)
  competence_features[is.infinite(competence_features)] <- 0
  competence_features[is.na(competence_features)] <- 0
  suppressWarnings(preprocessParams1<-caret::preProcess(competence_features, method = c("center", "scale")))
  competence_features1 <- stats::predict(preprocessParams1, competence_features)
  competence_predictions <- competence_enet_model %>% stats::predict(competence_features1)
  competence_features_output <-competence_features1
  colnames(competence_features_output) <- c("Negative_Tone","First_Person_Singular_Pronouns","Personal_Pronouns_as_Subjects","Prevention_Words","Adverbial_Modifiers","Negated_Surprise","Punctuation",
                                            "Word_Complexity","Superlative_Adjectives_After_Subject","Nouns_Before_Main_Verb","Infinitival_To",
                                            "Happiness_Emotion","Adpositions","Orthographic_Neighborhood","Vague_Discourse_Markers","Future_Oriented_Language","Competence_Language",
                                            "Power_Language","First_Person_as_Subject","Average_Sentiment","Anger_Emotion","Sentence_Length","Readability_Measure1",
                                            "Negative_Emotion","Lexical_Diversity_Measure1","Lexical_Diversity_Measure2","Readability_Measure2","Adverbs","Simple_Introductions")
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
