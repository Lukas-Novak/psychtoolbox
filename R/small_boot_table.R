# using devtools to create package
# nice elementary tutorial
# https://uoftcoders.github.io/studyGroup/lessons/r/packages/lesson/

# adds documentation to package as a whole
# use_package_doc()

# in case of problems delete namespace file
# then do devtools::load_all()
# and then devtools::document()

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

# licensing
# use_ccby_license() # or other licenses

# to check example data functioning
# devtools::run_examples()

# store new function to the other functions and creates documentation:
# usethis::use_r("small_boot_table") # This line has been edited to reflect the new function name
# examples are here:
# https://blog.methodsconsultants.com/posts/developing-r-packages-using-gitlab-ci-part-i/

# to update documentation document, there is need to install MikTex
# https://miktex.org/howto/install-miktex
# then there is need just to use devtools:
# devtools::build_manual(path = getwd())
#......................................................
# Documentation
#' Summarizing Bootstrapped Network Estimates from 'bootnet'
#'
#' @param bootnet_output output from the 'bootnet' package after bootstrapping network analysis.
#' @param predict_function_output optional, output from a predictability analysis function.
#'
#' @return A tibble summarizing edge weights and, optionally, node predictability.
#'
#' @docType methods
#'
#' @format An object of class \code{"tibble"}.
#'
#' @keywords bootstrapping, network analysis, 'bootnet'
#' @details This function provides a convenient summary of bootstrapped network estimates from the 'bootnet' package, including edge weights and their confidence intervals. Optionally, it can also include node predictability metrics if provided. It is tailored to enhance the usability of 'bootnet' output by summarizing critical metrics in a concise format. Importantly, it only summarise results in edges that were non-zero on sample level. Edges that had zero edge weight in a sample are filtered out.
#' @references Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Network analysis: An integrative approach to the structure of psychopathology. Annual review of clinical psychology, 14, 91-121.
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#'
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup
#' @importFrom tidyr tibble
#' @importFrom dplyr coalesce
#' @examples
#'   # Load required package
#'
#'   # Simulate data using base R
#'   set.seed(654899)
#'   Sigma <- matrix(0.5, nrow = 5, ncol = 5) + diag(0.5, 5)
#'   chol_decomp <- chol(Sigma)
#'   z <- matrix(rnorm(100 * 5), nrow = 100, ncol = 5)
#'   sim_data <- z %*% chol_decomp
#'   sim_data_df <- as.data.frame(sim_data)
#'
#'   # Estimate a network using 'EBICglasso'
#'   estimate <- bootnet::estimateNetwork(sim_data_df, default = "EBICglasso")
#'
#'   # Perform bootstrapping on the estimated network
#'   boot_results <- bootnet::bootnet(estimate, nBoots = 100, nCores = 1)
#'
#'   # Summarize the bootstrapped network
#'   summary <- small_boot_table(bootnet_output = boot_results)
#'   print(summary)
#' @export
#......................................................
small_boot_table <- function(bootnet_output, predict_function_output = NULL, include_sample_edge_weight = TRUE) {
  bootnet_summary <- summary(bootnet_output) %>%
    dplyr::filter(type == "edge" & sample != 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(id, sample, mean, CIlower, CIupper) %>%
    dplyr::mutate(across(c("sample", "mean", "CIlower", "CIupper"), ~round(., 2))) %>%
    dplyr::rename(Edge = id, 'Edge weight sample' = sample) %>%
    dplyr::mutate('Edge weight bootstrapped (95% CI)' = paste0(mean, " (", CIlower, ",", CIupper, ")")) %>%
    dplyr::select(-CIlower, -CIupper, -mean)

  if (!is.null(predict_function_output)) {
    predictability_info <- if (ncol(predict_function_output$errors) >= 3) {
      tidyr::tibble('Node predictability' = paste0(predict_function_output$errors$Variable, " = ", dplyr::coalesce(predict_function_output$errors$R2, predict_function_output$errors$nCC)))
    } else {
      tidyr::tibble('Node predictability' = paste0(predict_function_output$errors$Variable, " = ", predict_function_output$errors$R2))
    }

    edges_to_merge <- dplyr::select(bootnet_summary[1:nrow(predictability_info), ], Edge)
    predictability_to_merge <- cbind(predictability_info, edges_to_merge)
    bootnet_summary <- dplyr::full_join(bootnet_summary, predictability_to_merge, by = "Edge")

    if (include_sample_edge_weight == FALSE) {
      print("ok, you dont want to be sample edge weight to be included.")
      bootnet_summary <- bootnet_summary %>%
        select(!`Edge weight sample`)
    }

  }

  return(bootnet_summary)
}

##############################################xx[]

# Function testing

##############################################xx[]

# # Example testing of function with 'bootnet' output
# set.seed(746841)
# test.dat = tibble("Group" = rbinom(1:100, size = 0:1, prob = .5),
#                   "y" = ifelse(Group == 0,
#                                rnorm(n = 1:100, mean = 50, sd = 10),
#                                rnorm(n = 1:100, mean = 10, sd = 25)))
#
#
# # Example usage with 'bootnet'
# estimate <- bootnet::estimateNetwork(test.dat, default = "EBICglasso")
# test.boot <- bootnet::bootnet(estimate, nBoots = 1000, type = "case")
#
# small_boot_table(bootnet_output = test.boot) # Edited to reflect correct function call
