# Documentation
#' Summarizing Bootstrapped Network Estimates from 'bootnet'
#'
#' @param bootnet_output output from the 'bootnet' package after bootstrapping network analysis.
#' @param include_sample_edge_weight logical, whether to include sample edge weight in the summary table.
#'
#' @return A tibble summarizing edge weights.
#'
#' @docType methods
#'
#' @format An object of class \code{"tibble"}.
#'
#' @keywords bootstrapping, network analysis, 'bootnet'
#' @details This function provides a convenient summary of bootstrapped network estimates from the 'bootnet' package, including edge weights and their confidence intervals. It is tailored to enhance the usability of 'bootnet' output by summarizing critical metrics in a concise format. Importantly, it only summarizes results in edges that were non-zero on sample level. Edges that had zero edge weight in a sample are filtered out.
#' @references Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Network analysis: An integrative approach to the structure of psychopathology. Annual review of clinical psychology, 14, 91-121.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup
#' @importFrom tidyr tibble
#' @examples
#'   # Example testing of function with 'bootnet' output
#'   set.seed(746841)
#'   test_dat_1 = tibble("Group" = rbinom(1:100, size = 0:1, prob = .5),
#'                      "y" = ifelse(Group == 0,
#'                                   rnorm(n = 1:100, mean = 50, sd = 10),
#'                                   rnorm(n = 1:100, mean = 40, sd = 12)),
#'                      "z" = rnorm(n = 1:100, mean = 41, sd = 11),
#'                      "w" = rnorm(n = 1:100, mean = 40, sd = 10))
#'
#'   estimate <- bootnet::estimateNetwork(test_dat_1, default = "EBICglasso")
#'   boot_results <- bootnet::bootnet(estimate, nBoots = 100, nCores = 1)
#'
#'   summary <- edge_weight_summary(bootnet_output = boot_results)
#'   print(summary)
#' @export
edge_weight_summary <- function(bootnet_output, include_sample_edge_weight = TRUE) {
  bootnet_summary <- summary(bootnet_output) %>%
    dplyr::filter(type == "edge") %>%
    dplyr::ungroup() %>%
    dplyr::select(id, sample, mean, CIlower, CIupper) %>%
    dplyr::mutate(across(c("sample", "mean", "CIlower", "CIupper"), ~round(., 2))) %>%
    dplyr::rename(Edge = id, 'Edge weight sample' = sample) %>%
    dplyr::mutate('Edge weight bootstrapped (95% CI)' = paste0(mean, " (", CIlower, ",", CIupper, ")")) %>%
    dplyr::select(-CIlower, -CIupper, -mean)

  if (include_sample_edge_weight == FALSE) {
    bootnet_summary <- bootnet_summary %>%
      dplyr::select(-`Edge weight sample`)
  }

  return(bootnet_summary)
}

##############################################xx[]

# Function testing

##############################################xx[]

library(bootnet)
library(tidyverse)

# Example testing of function with 'bootnet' output
set.seed(746841)
test_dat_1 <- tibble("Group" = rbinom(1:100, size = 0:1, prob = .5),
                     "y" = ifelse(Group == 0,
                                  rnorm(n = 1:100, mean = 50, sd = 10),
                                  rnorm(n = 1:100, mean = 40, sd = 12)),
                     "z" = rnorm(n = 1:100, mean = 41, sd = 11),
                     "w" = rnorm(n = 1:100, mean = 40, sd = 10))

estimate_dat_1 <- bootnet::estimateNetwork(test_dat_1, default = "mgm")
test_boot_non_para_dat_1 <- bootnet::bootnet(estimate_dat_1, nBoots = 10, type = "nonparametric")

summary <- edge_weight_summary(bootnet_output = test_boot_non_para_dat_1, include_sample_edge_weight = FALSE)
print(summary)
