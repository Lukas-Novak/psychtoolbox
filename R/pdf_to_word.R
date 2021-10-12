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
#' word to pdf
#'
#' Conversion of word document to pdf using either R Markdown package or Libre office. The latter represents higher quality approach - in general.
#'
#' @param imp_file name of the word document to convert - without docx suffix
#' @param out_file name of output pdf file without - without pdf suffix
#'
#' @return pdf file
#'
#' @keywords pdf,word
#'
#' @value pdf
#'
#' @format An object of class \code{"pdf"}
#'
#' @details this function is currently running only on windows
#'
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#'
#' @importFrom docxtractr convert_to_pdf
#' @importFrom rmarkdown pandoc_convert
#'
#' @examples
#' # example from word do pdf
#' #word2pdf(imp_file = "example.docx",out_file = "example1.pdf")
#' @export
#......................................................
word2pdf = function (imp_file, out_file) {
  if(grepl(".docx", imp_file, fixed = T) == FALSE)
     stop("there has to be .docx sufix")
  if(grepl(".pdf", out_file, fixed = T) == FALSE)
    stop("there has to be .pdf sufix")

  if(length(list.files("C:/Program Files", "soffice.exe", recursive=TRUE,
                       full.names=TRUE, include.dirs=TRUE)) == 0) {
    print("Libre office was not detected, thus  RMarkdown convertor is used insted")
    pandoc_convert(imp_file, to = "pdf", output = out_file)
  } else
    print("Libre office detected - using this software to convert word to pdf")
  {
    present.path <- paste0(getwd(),"/",imp_file)

    office.path <- list.files("C:/Program Files", "soffice.exe", recursive=TRUE,
                              full.names=TRUE, include.dirs=TRUE)
    # convert power-point to pdf
    convert_to_pdf(present.path,
                   pdf_file = paste0(getwd(),"/",out_file))
  }
}

# word2pdf(imp_file = "example_documents/example.docx",out_file = "example1.pdf")
