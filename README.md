# Warmthcompetence R Package
Warmth and competence are the two main dimensions of social perception and judgement (Cuddy, Fiske & Glick, 2008). When individuals introduce or describe themselves, their audiences automatically make judgements about their warmth and competence. In this package, we provide tools that estimate warmth and competence social perceptions from natural self-presentational language. 

## Installation

This package has yet to be submitted to the Comprehensive R Archive Network (Hornik, 2021), so it is currently only available through Github. To install the package, use the following code in your R session:

``` r
install_github("bushraguenoun/warmthcompetence")
library("warmthcompetence")
``` 
Note that some features depend on spacyr which must be installed seperately through Python. To install spacyr, follow the instructions here: https://www.rdocumentation.org/packages/spacyr/versions/1.2.1

## Usage

This package contains two main functions: warmth and competence. These functions can be used as described below:

``` r
competence_scores <- competence(text_vector, ID_vector, metrics = c("scores", "features", "all"))
warmth_scores <- warmth(text_vector, ID_vector, metrics = c("scores", "features", "all"))
``` 
In the code above, text_vector is the vector of texts that will be assessed for warmth or competence. ID_vector is a vector of IDs that will be used to identify the warmth or competence scores. The metrics argument allows users to decide what metrics to return. Users can return the warmth or competence scores (metrics = "scores"), the features that underlie the warmth or competencee scores (metrics = "features"), or both the warmth or competencescores and the features (metrics = "all). The default choice is to return the warmth or competence scores.

## Getting Help

If you have any questions or encounter any problems with the package, please submit an issue through Github.

You can also contact me directly at bguenoun@hbs.edu.
