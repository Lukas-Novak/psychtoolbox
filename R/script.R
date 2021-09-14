# using devtools to create package
# nice elementary tutorial
# https://uoftcoders.github.io/studyGroup/lessons/r/packages/lesson/

# adds documentaion to package as a whole
# use_package_doc()

# in caase of problems delete namespace file
# than do load_all()
# and than document()

# storing data in R package
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Data-in-packages
# example:
# https://kbroman.org/pkg_primer/pages/data.html
# POSSIBLY A BETTER SOLUTION IS DESCRIBED HERE:
# https://blog.methodsconsultants.com/posts/developing-r-packages-using-gitlab-ci-part-i/


# data can be added by usethis::use_data(x, mtcars)
# there is documentation to that:
# https://r-pkgs.org/data.html

# to add a package:
# use_package("coin")

# if there is need to actualise
# devtools::load_all()
# roxygen2::roxygenise()

# licencing
# use_ccby_license() # or other licences

# to check example data functioning
# run_examples()

# store new function to the other functions and creates documentation:
# usethis::use_r("xxxxx")
# examples are here:
# https://blog.methodsconsultants.com/posts/developing-r-packages-using-gitlab-ci-part-i/


#......................................................
# Documentation
#' Automatic post-hoc testing
#'
#' @param df frame with one socio-demographic variable and one continous
#' @param y varialbe (continous)
#' @param var.nam variable (Socio-demographic variable)
#'
#' @return data frame
#'
#' @docType data
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stats t.test
#' @importFrom dplyr tibble
#' @importFrom stats fligner.test
#' @importFrom stats wilcox.test
#'
#' @examples
#' data(dat)
#' # data loading
#  # data=readRDS(file = "data/dat.Rds")
#' # running the function
#' two.g.comp.out.EC = two.g.comp(df = dat, y = "IRI_EC", var.nam = "Gender")
#' # printing the output
#' print(two.g.comp.out.EC)
#' @export
#......................................................

two.g.comp = function(df,y,var.nam) {
  g = var.nam
  df = df %>%
    select(y,g)
  colnames(df) <- c("y","g")
  if(fligner.test(y ~ g, data = df)$p.value < 0.05 & t.test(y ~ g, df)$p.value < 0.05) {
    return(
      tibble(
        "Variables" = var.nam,
        "Compared var" = y,
        "Statistic" = t.test(y ~ g, df)$statistic,
        "parameters" = t.test(y ~ g, df)$parameter[[1]],
        "p.value" = t.test(y ~ g, df)$p.value))
  } else
    wilcox.test(y ~ g, df)
  if(wilcox.test(y ~ g, df)$p.value < 0.05) {
    tibble(
      "Variables" = var.nam,
      "Compared var" = y,
      "Statistic" = wilcox.test(y ~ g, df)$statistic,
      "p.value" = wilcox.test(y ~ g, df)$p.value,
      "r" = rstatix::wilcox_effsize(y ~ g, data = df)$effsize)
  } else
    "No significnat differences"
}


