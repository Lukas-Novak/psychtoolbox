# Documentation
#' Summarizing Node Predictability
#'
#' @param predict_function_output output from a predictability analysis function.
#'
#' @return A tibble summarizing node predictability.
#'
#' @docType methods
#'
#' @format An object of class \code{"tibble"}.
#'
#' @keywords predictability, network analysis
#' @details This function provides a summary of node predictability metrics. It is designed to extract and format predictability information from the output of a predictability analysis function.
#' @references Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Network analysis: An integrative approach to the structure of psychopathology. Annual review of clinical psychology, 14, 91-121.
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#' @importFrom tidyr tibble
#' @importFrom dplyr coalesce
#' @importFrom dplyr pull
#' @examples
#'   # Example testing of function with 'bootnet' and predictability output
#'   set.seed(746841)
#'   library(tidyr)
#'   test_dat_1 = tibble("Group" = rbinom(1:100, size = 0:1, prob = .5),
#'                      "y" = ifelse(Group == 0,
#'                                   rnorm(n = 1:100, mean = 50, sd = 10),
#'                                   rnorm(n = 1:100, mean = 40, sd = 12)),
#'                      "z" = rnorm(n = 1:100, mean = 41, sd = 11),
#'                      "w" = rnorm(n = 1:100, mean = 40, sd = 10))
#'
#'   estimate_dat_1 <- bootnet::estimateNetwork(test_dat_1, default = "mgm")
#'   predict_output_dat_1  <- predict(estimate_dat_1$results, estimate_dat_1$data)
#'
#'   predictability_summary <- node_predictability_summary(predict_output_dat_1)
#'   print(predictability_summary)
#' @export
node_predictability_summary <- function(predict_function_output) {
  if (ncol(predict_function_output$errors) >= 3) {
    predictability_info <- tidyr::tibble('Node predictability' = paste0(predict_function_output$errors$Variable, " = ", dplyr::coalesce(predict_function_output$errors$R2, predict_function_output$errors$nCC)))
  } else {
    predictability_info <- tidyr::tibble('Node predictability' = paste0(predict_function_output$errors$Variable, " = ", predict_function_output$errors$R2))
  }

  return(predictability_info)
}

##############################################xx[]

# Function testing

##############################################xx[]

# library(bootnet)
# library(tidyverse)
#
# # Example testing of function with 'bootnet' and predictability output
# set.seed(746841)
# test_dat_1 <- tibble("Group" = rbinom(1:100, size = 0:1, prob = .5),
#                      "y" = ifelse(Group == 0,
#                                   rnorm(n = 1:100, mean = 50, sd = 10),
#                                   rnorm(n = 1:100, mean = 40, sd = 12)),
#                      "z" = rnorm(n = 1:100, mean = 41, sd = 11),
#                      "w" = rnorm(n = 1:100, mean = 40, sd = 10))
#
# estimate_dat_1 <- bootnet::estimateNetwork(test_dat_1, default = "mgm")
# predict_output_dat_1  <- predict(estimate_dat_1$results, estimate_dat_1$data)
#
# predictability_summary <- node_predictability_summary(predict_function_output = predict_output_dat_1)
# print(predictability_summary)
