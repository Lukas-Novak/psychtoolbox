#' Summarizing Bootstrapped Network Estimates from 'bootnet'
#'
#' @param bootnet_output Output from the 'bootnet' package after bootstrapping network analysis.
#' @param include_sample_edge_weight Logical, whether to include sample edge weight in the summary table.
#' @param include_p_values String, whether to include p-values as significance stars, add p-value column, or exclude p-values entirely. Options are "stars", "exact", or "none". Default is "none".
#'
#' @return A tibble summarizing edge weights.
#'
#' @docType methods
#'
#' @format An object of class \code{"tibble"}.
#'
#' @keywords bootstrapping, network analysis, 'bootnet'
#' @details This function provides a convenient summary of bootstrapped network estimates from the 'bootnet' package, including edge weights, their confidence intervals, and p-values alternatively. It is tailored to enhance the usability of 'bootnet' output by summarizing critical metrics in a concise format. Importantly, it only summarizes results in edges that were non-zero on sample level. Edges that had zero edge weight in a sample are filtered out.
#'
#' The empirical p-values are calculated using the following formula:
#' \deqn{p = \frac{\sum_{i=1}^{n} \mathbf{1}\{|x_i| \geq |x_0|\}}{n}}
#' where \eqn{x_0} is the original estimate and \eqn{x_i} are the bootstrap estimates.
#'
#' @references Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Network analysis: An integrative approach to the structure of psychopathology. Annual review of clinical psychology, 14, 91-121.
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr rowwise
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom tidyr tibble
#' @examples
#'   # Example testing of function with 'bootnet' output
#'   set.seed(746841)
#'   library(tidyr)
#'   test_dat_1 <- tibble("Group" = rbinom(100, size = 1, prob = .5),
#'                        "y" = ifelse(Group == 0,
#'                                     rnorm(100, mean = 50, sd = 10),
#'                                     rnorm(100, mean = 40, sd = 12)),
#'                        "z" = rnorm(100, mean = 41, sd = 11),
#'                        "w" = rnorm(100, mean = 40, sd = 10))
#'
#'   estimate <- bootnet::estimateNetwork(test_dat_1, default = "EBICglasso")
#'   boot_results <- bootnet::bootnet(estimate, nBoots = 100, nCores = 1)
#'
#'   summary <- edge_weight_summary(bootnet_output = boot_results)
#'   print(summary)
#' @export
edge_weight_summary <- function(bootnet_output, include_sample_edge_weight = TRUE, include_p_values = "none") {
  # Function to calculate empirical p-values
  calculate_empirical_p_values <- function(original_estimate, bootstrap_distribution) {
    p_value <- mean(abs(bootstrap_distribution) >= abs(original_estimate))
    return(p_value)
  }

  # Extract the original sample estimates
  sample_edges <- bootnet_output$sampleTable %>%
    filter(type == "edge")

  # Extract the bootstrapped estimates
  bootstrap_edges <- bootnet_output$bootTable %>%
    filter(type == "edge")

  # Calculate p-values for each edge
  p_values <- sample_edges %>%
    rowwise() %>%
    mutate(
      p_value = calculate_empirical_p_values(
        value,
        bootstrap_edges %>%
          filter(node1 == .data$node1, node2 == .data$node2) %>%
          pull(value)
      )
    ) %>%
    select(id, p_value) # Select 'id' to match with summary data frame

  # Summarize the bootstrapped results
  bootnet_summary <- summary(bootnet_output) %>%
    dplyr::filter(type == "edge") %>%
    dplyr::ungroup() %>%
    dplyr::select(id, sample, mean, CIlower, CIupper) %>%
    dplyr::mutate(across(c("sample", "mean", "CIlower", "CIupper"), ~round(., 2))) %>%
    dplyr::rename(Edge = id, 'Edge weight sample' = sample) %>%
    dplyr::mutate('Edge weight bootstrapped (95% CI)' = paste0(mean, " (", CIlower, ",", CIupper, ")")) %>%
    dplyr::select(-CIlower, -CIupper, -mean)

  # Add p-values to the summary
  bootnet_summary <- bootnet_summary %>%
    left_join(p_values, by = c("Edge" = "id"))

  # Add significance stars based on p-values
  if (include_p_values == "stars") {
    bootnet_summary <- bootnet_summary %>%
      mutate(
        Significance = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ ""
        ),
        `Edge weight bootstrapped (95% CI)` = paste0(`Edge weight bootstrapped (95% CI)`, " ", Significance)
      ) %>%
      select(-p_value, -Significance)
  } else if (include_p_values == "exact") {
    bootnet_summary <- bootnet_summary
  } else if (include_p_values == "none") {
    bootnet_summary <- bootnet_summary %>%
      select(-p_value)
  }

  if (include_sample_edge_weight == FALSE) {
    bootnet_summary <- bootnet_summary %>%
      dplyr::select(-`Edge weight sample`)
  }

  return(bootnet_summary)
}

##############################################
# Function testing
##############################################

# # Load necessary libraries
# library(bootnet)
# library(tidyverse)
#
# # Function to generate correlated data
# generate_correlated_data <- function(n, rho, num_vars) {
#   mu <- rep(0, num_vars)
#   Sigma <- matrix(rho, nrow = num_vars, ncol = num_vars)
#   diag(Sigma) <- 1
#   MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
# }
#
# # Simulate data and results
# set.seed(746841)
# independent_data <- tibble(
#   "Group" = rbinom(100, size = 1, prob = .5),
#   "y" = ifelse(Group == 0,
#                rnorm(100, mean = 50, sd = 10),
#                rnorm(100, mean = 40, sd = 12)),
#   "z" = rnorm(100, mean = 41, sd = 11),
#   "w" = rnorm(100, mean = 40, sd = 10),
#   "v" = rnorm(100, mean = 45, sd = 9)
# )
#
# correlated_data <- as_tibble(generate_correlated_data(n = 100, rho = 0.4, num_vars = 5))
# names(correlated_data) <- c("x1", "x2", "x3", "x4", "x5")
#
# test_dat_1 <- bind_cols(independent_data, correlated_data)
#
# estimate_dat_1 <- bootnet::estimateNetwork(test_dat_1, default = "mgm")
#
# test_boot_non_para_dat_1 <- bootnet::bootnet(estimate_dat_1, nBoots = 10, type = "nonparametric")
#
# summary <- edge_weight_summary(bootnet_output = test_boot_non_para_dat_1,
#                                include_sample_edge_weight = FALSE,
#                                include_p_values = "stars")
# print(summary) %>% view()
