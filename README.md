
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Pipeline
status](https://gitlab.com/r-packages/rock/badges/prod/pipeline.svg)](https://gitlab.com/lukas.novak/psychtoolbox/-/commits/master)
[![Coverage
status](https://codecov.io/gl/r-packages/psychtoolbox/branch/prod/graph/badge.svg)](https://codecov.io/gl/r-packages/psychtoolbox?branch=prod)
[![Version on
CRAN](https://www.r-pkg.org/badges/version/psychtoolbox?color=brightgreen)](https://cran.r-project.org/package=psychtoolbox)
<!-- [![Dependency status](https://tinyverse.netlify.com/badge/psychtoolbox)](https://CRAN.R-project.org/package=psychtoolbox) -->
<!-- badges: end -->

# <img src='images/psych.png' align="right" height="200" />

# psychtoolbox

## Tools for psychological research

The examples can be found on the personal website of [Lukas
Novak](https://lukasnovak.online/)

<!--------------------------------------------->
<!-- Start of a custom bit for every package -->
<!--------------------------------------------->

The Psychological tools repository (`psychtoolbox`) was developed to
facilitate reproducible and open coding, specifically geared towards
qualitative research methods. Although it is a general-purpose toolkit,
three specific applications have been implemented, specifically an
interface to the `rENA` package that implements Epistemic Network
Analysis (ENA), means to process notes from Cognitive Interviews (CIs),
and means to work with a decentralized construct taxonomy (DCT).

There are more example of usage available on the Open Science Framework
website.

<!--------------------------------------------->
<!--  End of a custom bit for every package  -->
<!--------------------------------------------->

## Installation

If you dont have devtools package installed, you can do that from by
using the following code

``` r
if(!'devtools' %in% rownames(installed.packages()))install.packages('devtools')
```

You can install the stable version of `psychtoolbox` from
[GitLab](https://about.gitlab.com/) with:

``` r
devtools::install_gitlab('lukas.novak/psychtoolbox')
```

You can install the development version of `psychtoolbox` from
[GitLab](https://about.gitlab.com/) with:

``` r
devtools::install_gitlab('lukas.novak/psychtoolbox@devel')
```

<!--------------------------------------------->
<!-- Start of a custom bit for every package -->
<!--------------------------------------------->
<!-- ## References -->

# Citation

Novak, L. (2021). psychtoolbox: tools for psychology and psychometrics.
R package version 0.0.0.9000.
<https://gitlab.com/lukas.novak/psychtools>

# References

Novak, L., Malinakova, K., Mikoska, P., van Dijk, J. P., Dechterenko,
F., Ptacek, R., & Tavel, P. (2021). Psychometric Analysis of the Czech
Version of the Toronto Empathy Questionnaire. International Journal of
Environmental Research and Public Health, 18(10), 5343. MDPI AG.
Retrieved from <http://dx.doi.org/10.3390/ijerph18105343>

<!--------------------------------------------->
<!--  End of a custom bit for every package  -->
<!--------------------------------------------->
