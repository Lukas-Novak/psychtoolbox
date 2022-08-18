# using devtools to create package
# nice elementary tutorial
# https://uoftcoders.github.io/studyGroup/lessons/r/packages/lesson/

# adds documentaion to package as a whole
# use_package_doc()

# in case of problems delete namespace file
# than do devtools::load_all()
# and than devtools::document()

# storing data in R package
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Data-in-packages
# example:
# https://kbroman.org/pkg_primer/pages/data.html
# POSSIBLY A BETTER SOLUTION IS DESCRIBED HERE:
# https://blog.methodsconsultants.com/posts/developing-r-packages-using-gitlab-ci-part-i/


# data can be added by usethis::use_data(x, mtcars)
# there is documentation to that:
# https://r-pkgs.org/data.html
# data documentation can be created via this function:
# sinew::makeOxygen(paq.validation.study) #

# to add a package:
# use_package("coin")

# if there is need to actualize
# devtools::load_all()
# roxygen2::roxygenise()

# licencing
# use_ccby_license() # or other licences

# to check example data functioning
# devtools::run_examples()

# store new function to the other functions and creates documentation:
# usethis::use_r("xxx") # "xxx" = R script containing function definition
# examples are here:
# https://blog.methodsconsultants.com/posts/developing-r-packages-using-gitlab-ci-part-i/

# to update documentation document, there is need to install MikTex
# https://miktex.org/howto/install-miktex
# than there is need just to use devtools:
# devtools::build_manual(path = getwd())
#......................................................
# Documentation
#' Automatic two-groups comparison
#'
#' @param df data frame or tibble with one socio-demographic variable and one continuous variable
#' @param y continuous variable
#' @param group.var binary grouping variable
#'
#' @return data frame
#  @value data frame
#'
#' @docType data
#'
#' @format An object of class \code{"tibble"}
#'
#' @keywords two group comparison, Wilcoxon test
#' @details This function computes either Wilcox test or t-test depending on whether homogeneity of variances assumption is met or not.
#' @references Myles Hollander and Douglas A. Wolfe (1973). Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 27--33 (one-sample), 68--75 (two-sample).
#' Or second edition (1999).
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#'
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stats t.test
#' @importFrom dplyr tibble
#' @importFrom stats fligner.test
#' @importFrom stats wilcox.test
#'
#' @examples
#' # data loading
#' data(dat)
# data=readRDS(file = "data/dat.Rds") # only when data are "exogenous"
#' # running the function
#' two.g.comp.out.EC = two.g.comp(df = dat, y = "IRI_EC", group.var = "Gender")
#' # printing the output
#' print(two.g.comp.out.EC)
#' @export
#......................................................

two.g.comp = function(df,y,group.var) {
  g = group.var
  df = df %>%
    select(y,g)
  colnames(df) <- c("y","g")
  if(fligner.test(y ~ g, data = df)$p.value > 0.05 & t.test(y ~ g, df)$p.value < 0.05) {
    return(
      tibble(
        "Variables" = group.var,
        "Compared var" = y,
        "Statistic" = t.test(y ~ g, df)$statistic,
        "parameters" = t.test(y ~ g, df)$parameter[[1]],
        "p.value" = t.test(y ~ g, df)$p.value))
  } else
    wilcox.test(y ~ g, df)
  if(wilcox.test(y ~ g, df)$p.value < 0.05) {
    tibble(
      "Variables" = group.var,
      "Compared var" = y,
      "Statistic" = wilcox.test(y ~ g, df)$statistic,
      "p.value" = wilcox.test(y ~ g, df)$p.value,
      "r" = rstatix::wilcox_effsize(y ~ g, data = df)$effsize)
  } else
    "No significnat differences"
}

##############################################xx[]


# Function testing


##############################################xx[]

# # testing of function
# set.seed(746841)
# test.dat = tibble("Group" = rbinom(1:100, size = 0:1, prob = .5),
#                   "y" = ifelse(Group == 0,
#                                rnorm(n = 1:100, mean = 50, sd = 10),
#                                rnorm(n = 1:100, mean = 10, sd = 25)))
#
#
# fligner.test(y ~ Group, data = test.dat)
# t.test(y ~ Group, data = test.dat)
# two.g.comp(df = test.dat, y = "y", group.var = "Group")
