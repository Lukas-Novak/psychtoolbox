---
output: github_document
Package: psychtoolbox
Title: Tools for psychology and psychometrics
Version: 0.0.0.9000
Authors@R: 
    person(given = "Lukas",
           family = "Novak",
           role = c("aut", "cre"),
           email = "lukasjirinovak@gmail.com",
           comment = c(ORCID = "0000-0002-7582-2098"))
Description: This package contains functions helping to analyse psychological data.
License: CC BY 4.0
URL: https://gitlab.com/lukas.novak/psychtoolbox
Encoding: UTF-8
LazyData: true
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.1.2
Imports: 
    coin,
    docxtractr,
    dplyr,
    foreign,
    magrittr,
    rmarkdown,
    rstatix,
    stats,
    tidyr
Suggests: 
    testthat (>= 3.0.0)
Config/testthat/edition: 3
---
<!-- toc -->

leden 29, 2022


# `clin_sig_chang`

Clinically significant change index


## Description

This easy function calculates Clinically significant change (CSCH) as defined by Jacobson and Truax (1991).


## Format

numeric vector of values


## Usage

```r
clin_sig_chang(SD_0, SD_1, M1_1, M0_0)
```


## Arguments

Argument      |Description
------------- |----------------
`SD_0`     |     standard deviation of the non-clinical population
`SD_1`     |     standard deviation of the clinical population
`M1_1`     |     mean of the clinical population
`M1_0`     |     mean of the non-clinical population


## Details

This function computes either Wilcox test or t-test depending on whether homogeneity of variances assumption is met or not.


## Value

numeric vector


## Author

Lukas Novak, lukasjirinovak@gmail.com


## References

Myles Hollander and Douglas A. Wolfe (1973). Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 27--33 (one-sample), 68--75 (two-sample).
 Or second edition (1999).


## Examples

```r
# data loading
clin.cut.off=clin_sig_chang(SD_0 = 3.5,
SD_1 = 2.1,
M0_0 = 4.2,
M1_1 = 12.1) %>% round(digits = 2)
```


# `dat`

DATASET_TITLE


## Description

DATASET_DESCRIPTION


## Format

A data frame with 835 rows and 2 variables:
 list("\n", list(list(list("Gender")), list("integer COLUMN_DESCRIPTION")), "\n", list(list(list("IRI_EC")), list("double COLUMN_DESCRIPTION")), "\n")


## Usage

```r
dat
```


## Details

DETAILS


# `%>%`

Pipe operator


## Description

See `magrittr::` for details.


## Usage

```r
lhs %>% rhs
```


## Arguments

Argument      |Description
------------- |----------------
`lhs`     |     A value or the magrittr placeholder.
`rhs`     |     A function call using the magrittr semantics.


## Value

The result of calling `rhs(lhs)` .


# `psychtoolbox-package`

psychtoolbox: Tools for psychology and psychometrics


## Description

This package contains functions helping to analyse psychological data.


## Seealso

Useful links:
   

*   [https://gitlab.com/lukas.novak/psychtoolbox](https://gitlab.com/lukas.novak/psychtoolbox)


## Author

Maintainer : Lukas Novak lukasjirinovak@gmail.com ( [ORCID](https://orcid.org/0000-0002-7582-2098) )


# `two.g.comp`

Automatic two-groups comparison


## Description

Automatic two-groups comparison


## Format

An object of class `"tibble"`


## Usage

```r
two.g.comp(df, y, group.var)
```


## Arguments

Argument      |Description
------------- |----------------
`df`     |     data frame or tibble with one socio-demographic variable and one continuous variable
`y`     |     continuous variable
`group.var`     |     binary grouping variable


## Details

This function computes either Wilcox test or t-test depending on whether homogeneity of variances assumption is met or not.


## Value

data frame


## Author

Lukas Novak, lukasjirinovak@gmail.com


## References

Myles Hollander and Douglas A. Wolfe (1973). Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 27--33 (one-sample), 68--75 (two-sample).
 Or second edition (1999).


## Examples

```r
# data loading
data(dat)
# running the function
two.g.comp.out.EC = two.g.comp(df = dat, y = "IRI_EC", group.var = "Gender")
# printing the output
print(two.g.comp.out.EC)
```


# `word2pdf`

word to pdf


## Description

Conversion of word document to pdf using either R Markdown package or Libre office. The latter represents higher quality approach - in general.


## Format

An object of class `"pdf"`


## Usage

```r
word2pdf(imp_file, out_file)
```


## Arguments

Argument      |Description
------------- |----------------
`imp_file`     |     name of the word document to convert - without docx suffix
`out_file`     |     name of output pdf file without - without pdf suffix


## Details

this function is currently running only on windows


## Value

pdf file


## Author

Lukas Novak, lukasjirinovak@gmail.com


## Examples

```r
# example from word do pdf
#word2pdf(imp_file = "example.docx",out_file = "example1.pdf")
```


