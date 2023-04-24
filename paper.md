---
title: 'Assessing Warmth and Competence with the warmthcompetence R Package'
tags:
- R
- psycholinguistics
- warmth
- competence
date: "24 April 2023"
output: pdf_document
authors:
- name: Bushra Guenoun
  orcid: 0000-0003-0975-1620
  affiliation: '1'
- name: Julian Zlatev
  orcid: 0000-0002-9427-9887
  affiliation: '1'
bibliography: paper.bib
csl: apa.csl
affiliations:
- name: Harvard Business School, Boston, MA
  index: 1
---

# Summary

Warmth and competence are widely viewed as the two fundamental dimensions of social cognition. Yet existing methods of assessing warmth and competence perceptions in natural language are either too time-intensive or too limited. In particular, previous research has relied either on employing human coders to read and rate text [e.g., @abele_2007; @diehl_2004] or quantifying word usage to assess warmth and competence in natural language [e.g, @decter_2016; @pietraszkiewicz_2019]. We introduce the warmthcompetence package, an R package with easy-to-use functions that quantify warmth and competence perceptions in self-presentational written language. The warmthcompetence package moves beyond prior approaches and uses natural language processing (NLP) to evaluate both the content and structure of text to predict warmth and competence perceptions. 

The models used to evaluate warmth and competence were developed through an exploratory approach based in machine learning. In this paper, we evaluate the underlying models’ performance and walk through an application of the package that illustrates how it can be utilized by researchers across a variety of fields including social psychology, developmental psychology, neuroscience, management, and human-computer interaction. We hope the warmthcompetence package and this tutorial are useful to researchers and inspire the development of similar tools for other important psychological constructs. 


# Statement of Need

When meeting or learning about someone new, research suggests that individuals make two critical judgements [@cuddy_2008]: (1) How warm is this person? (2) How competent is this person? These two perceptions then go on to influence individuals’ emotional and behavioral reactions to the new person [@fiske_2002; @fiske_2007]. 

Often described as the two universal dimensions of social cognition, the constructs of warmth (i.e., perceived friendliness and trustworthiness) and competence (i.e., perceived effectiveness and intellect) are foundational to research in the psychological sciences [@cuddy_2008; @fiske_2018]. These constructs are utilized by a wide range of researchers across different branches of psychology, including interpersonal relations [e.g., @holoien_2013; @judd_2005; @kervyn_2009], intergroup relations [e.g., @becker_2012; @fiske_2002; @nicolas_2021], neuroscience [e.g., @harris_2010; @li_2021; @simon_2020], and developmental psychology [e.g., @altschul_2016; @chen_2000; @roberts_1987]. Warmth and competence are also integral constructs in adjacent fields including management [e.g., @cuddy_2011; @jeong_2019], marketing [e.g., @kim_2019; @hyoyeon_2019], medicine [e.g., @drevs_2013; @kraft_2017], law [e.g., @de_2020; @neal_2012] and computer-human interaction [e.g., @nguyen_2015; @peters_2017].

Yet, existing methods to assess warmth and competence perceptions are either too demanding or too limited. For example, many studies use human coders blind to the hypotheses to read and rate text for warmth and competence [e.g., @abele_2007; @diehl_2004], an approach that requires significant time and resources. Other research has developed more automatic text-analysis methods, where warmth and competence are assessed by counting the instances of particular words in a text [e.g, @decter_2016; @pietraszkiewicz_2019]. For example, a text is judged higher in warmth if it contains words like “friend” or “family.” While this approach is a step in the right direction, it is incomplete. Individuals not only use the content of text to assess warmth and competence, but also its structure. For instance, individuals’ assessment of competence is likely influenced by the length and complexity of sentence construction. 

In this paper, we introduce the warmthcompetence package, an R package with easy-to-use functions that quantify warmth and competence perceptions in self-presentational natural language using both the content and structure of text. The package takes written self-presentational natural language as input and uses elastic net models to generate scores that represent how readers of the text would evaluate the writer’s warmth and competence. Elastic net regression was selected over methods that allow for non-linear associations, such as random forests or neutral nets, to allow for interpretability of the output (see GitHub repository for more details on the features).

We focus this paper on validating the underlying models’ abilities to predict warmth and competence perceptions and illustrating how the package can be utilized by social science researchers. Readers interested in learning more about how the package’s underlying machine learning models were developed can find additional information on GitHub.


# Model Evaluation

To evaluate the performance of the warmthcompetence package, we took two steps. First, we examined how the models performed on additional datasets that better represented real-world contexts. To test the warmth detector, we use a dataset in which participants were instructed to negotiate in a “warm and friendly” manner versus a “tough and firm” manner [@jeong_2019]. To test the competence detector, we use a dataset in which conversational agents were trained to either share knowledge or show empathy in their speech [@smith_2020].

We then compared the predictive ability of the models with two established methods that have been used to assess warmth and competence in natural language. The first is the Big Two Dictionaries, which uses count-based dictionaries to quantify warmth and competence in text [@pietraszkiewicz_2019]. The second is based on the popular Linguistic Inquiry and Word Count (LIWC) dictionaries [@pennebaker_2015; @decter_2016].

## Testing the Warmth Detector 
**Dataset.** We used a dataset of 355 negotiation messages from Study 1 of @jeong_2019. Participants were randomly assigned to one of two conditions: warm or tough. Those in the warm condition were told by the authors that warm and friendly negotiators get better results while those in the tough condition were told that tough and firm negotiators get better results. 

**Model Performance.** We ran logistic models predicting participant condition (tough versus warmth) using the warmth and competence detectors. The warmth detector significantly predicted condition (M~Warm~ = 0.223 [95% CI: 0.155, 0.292], M~Tough~ = -0.195 [95% CI: -0.267, -0.123], t(353) = 8.302, p < 0.001; Cohen’s d = 0.879). It also performed significantly better compared to the Linguistic Inquiry and Word Count (LIWC) dictionaries (M~Warm~ = 1.309 [95% CI: 1.279, 1.339], M~Tough~ = 1.172 [95% CI: 1.136, 1.208], t(345) = 5.759, p < 0.001; Cohen’s d = 0.607) and the Big Two Dictionaries (M~Warm~ = 0.102 [95% CI: 0.095, 0.109], M~Tough~ = 0.088 [95% CI: 0.082, 0.095], t(349) = 2.893, p = 0.005; Cohen’s d = 0.308).

## Testing the Competence Detector 
**Dataset.** We used 6,808 sets of conversation utterance suggestions generated by two conversational agents to test our competence detector [@smith_2020]. One of these conversational agents was trained on the Wizards of Wikipedia (WoW) dataset and was designed to express knowledge in conversations. The other conversational agent was trained on the EmpathicDialogues (ED) dataset and designed to express empathy [@smith_2020].

**Model Performance.** We collapsed all of the conversational suggestions provided by each conversational agent for each conversation into a single message, resulting in 13,616 messages (an aggregate message of knowledge suggestions and an aggregate message of empathic suggestions for each of the 6,808 conversations). We then ran the competence detector on these aggregate messages and examined whether the competence detector could predict which conversational agent produced each message. Our competence detector successfully reported that the conversational suggestions generated by the WoW agent expressed more competence relative to the suggestions generated by the ED agent (M~WoW~ = 0.193 [95% CI: 0.183, 0.203], M~ED~ = -0.163 [95% CI: -0.176, -0.151], t(13,216) = 42.794, p < 0.001; Cohen’s d = 0.733). It performed significantly better than the Big Two Dictionaries (M~WoW~ = 0.081 [95% CI: 0.080, 0.082], M~ED~ = 0.097 [95% CI: 0.096, 0.098], t(12,816) = 18.542, p < 0.001; Cohen’s d = 0.318) and the Linguistic Inquiry and Word Count (LIWC) dictionaries (M~WoW~ = 1.185 [95% CI: 1.180, 1.190], M~ED~ = 1.380 [95% CI: 1.374, 1.387], t(13,065) = 46.119, p < 0.001; Cohen’s d = 0.790).

# Application of Package

To illustrate how the warmthcompetence package can be applied, we use data from a study in which 393 participants were instructed to write messages introducing themselves as either warm or competent. These messages were also assessed for warmth and competence by three independent judges, who were blind to condition. Data from this study is included in the warmthcompetence package, allowing the analyses and results shown in this paper to be reproduced by anyone using the package. 

To start, use the following code to import the package from GitHub into your R session:

```{R}
install_github("bushraguenoun/warmthcompetence")
library("warmthcompetence")
```
Once the package is loaded, the study data can be accessed by calling `vignette_data`. As discussed earlier, the warmthcompetence package contains two main functions: `warmth()`, a function to assess warmth predictions, and `competence()`, a function to assess competence predictions. The only required argument for both of these functions is the vector of text to be analyzed. However, these functions also include two optional arguments: a vector of participant identifiers ("ID") and an indication of the user’s desired output ("metrics"). If the "metrics" argument is “scores,” then the function will return the warmth or competence scores. If the "metrics" argument is “features,” then then the argument will return the features that underlie scores. If the "metrics" argument is “all,” then both the scores and features will be returned. If no "metrics" argument is provided, the default is to only return the scores.
	
In this tutorial, we will use the following code to return and join dataframes with scores for both the warmth and competence functions:

```{R}
competence_scores <- competence(vignette_data$bio, vignette_data$ResponseId)
warmth_scores <- warmth(vignette_data$bio, vignette_data$ResponseId)
both_scores <- dplyr::inner_join(competence_scores, warmth_scores, by = c("ID" = "ID"))
all_data <- dplyr::left_join(vignette_data, both_scores, by = c("ResponseId" = "ID"))
```

The resulting `all_data` dataframe contains a column for competence perception scores (`competence_predictions`) and a column for warmth perception scores (`warmth_predictions`). Now, users can use these predictions to run analyses, such as relating the predictions to participant condition or to the judge’s ratings of warmth and competence (see vignette on GitHub for these analyses).
	
# References

