
<!-- README.md is generated from README.Rmd. Please edit that file -->

# warmthcompetence: Warmth and Competence Detectors

Warmth and competence are the two main dimensions of social perception
and judgment ([Cuddy, Fiske, and Glick 2008](#ref-cuddy2008)) . When
individuals introduce or describe themselves, their audiences
automatically make judgments about their warmth and competence. In the
*warmthcompetence* package, we provide tools that estimate warmth and
competence social perceptions from natural self-presentational language.
We use pre-trained enet regression models to provide numerical
representations of warmth and competence perceptions.

## Citing warmthcompetence

To generate a citation for *warmthcompetence*, please run

``` r
citation("warmthcompetence")
```

## Installation

To install *warmthcompetence* from CRAN, use the following code in your
R session:

``` r
install.packages("warmthcompetence")
```

To install the development version from GitHub, use the following code:

``` r
devtools::install_github("bushraguenoun/warmthcompetence")
```

Note that some features depend on *spacyr* which must be installed
separately through Python. To install *spacyr*, follow the instructions
[here](https://www.rdocumentation.org/packages/spacyr/).

## Usage

*warmthcompetence* contains two main functions: `warmth()` and
`competence()`. These functions can be used as described below:

``` r
competence_scores <- competence(text_vector, ID_vector, metrics = "scores")

warmth_scores <- warmth(text_vector, ID_vector, metrics = "scores")
```

In the code above, `text_vector` is the vector of texts that will be
assessed for warmth or competence. `ID_vector` is a vector of IDs that
will be used to identify the warmth or competence scores. The `metrics`
argument allows users to decide what metrics to return. Users can return
the warmth or competence scores (`metrics = "scores"`), the features
that underlie the warmth or competence scores (`metrics = "features"`),
or both the warmth or competence scores and the features
(`metrics = "all"`). The default choice is to return the warmth or
competence scores.

See `vignette("warmthcompetence")` for more details.

## Getting Help

If you have any questions or encounter any problems with the package,
please submit an issue through
[Github](https://github.com/bushraguenoun/warmthcompetence/issues).

You can also contact me directly at bushraguenoun@gmail.com.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-cuddy2008" class="csl-entry">

Cuddy, Amy J. C., Susan T. Fiske, and Peter Glick. 2008. “Warmth and
Competence as Universal Dimensions of Social Perception: The Stereotype
Content Model and the BIAS Map.” In *Advances in Experimental Social
Psychology*, 40:61–149. Elsevier.
<https://doi.org/10.1016/S0065-2601(07)00002-0>.

</div>

</div>
