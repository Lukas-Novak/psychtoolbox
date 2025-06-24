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
#' @param include_sample_edge_weight Logical, whether to include sample edge weight in the summary table.
#' @param include_p_values String, whether to include p-values as significance stars, add p-value column, or exclude p-values entirely. Options are "stars", "exact", or "none". Default is "none".
#'
#' @return A tibble summarizing edge weights and, optionally, node predictability.
#'
#' @docType methods
#'
#' @format An object of class \code{"tibble"}.
#'
#' @keywords bootstrapping, network analysis, 'bootnet'
#' @details This function provides a convenient summary of bootstrapped network estimates from the 'bootnet' package, including edge weights, their confidence intervals, and optionally p-values and node predictability metrics if provided. It is tailored to enhance the usability of 'bootnet' output by summarizing critical metrics in a concise format. Importantly, it only summarizes results in edges that were non-zero on sample level. Edges that had zero edge weight in a sample are filtered out.
#'
#' The empirical p-values are calculated using the following formula:
#' \deqn{p = \frac{\sum_{i=1}^{n} \mathbf{1}\{|x_i| \geq |x_0|\}}{n}}
#' where \eqn{x_0} is the original estimate and \eqn{x_i} are the bootstrap estimates.
#'
#' @references Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Network analysis: An integrative approach to the structure of psychopathology. Annual review of clinical psychology, 14, 91-121.
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
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
small_boot_table <- function(bootnet_output, predict_function_output = NULL, include_sample_edge_weight = TRUE, include_p_values = "none") {

  # Function to calculate empirical p-values
  calculate_empirical_p_values <- function(original_estimate, bootstrap_distribution) {
    p_value <- mean(abs(bootstrap_distribution) >= abs(original_estimate))
    return(p_value)
  }

  # Extract the original sample estimates
  sample_edges <- bootnet_output$sampleTable %>%
    dplyr::filter(type == "edge")

  # Extract the bootstrapped estimates
  bootstrap_edges <- bootnet_output$bootTable %>%
    dplyr::filter(type == "edge")

  # Calculate p-values for each edge
  p_values <- sample_edges %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      p_value = calculate_empirical_p_values(
        value,
        bootstrap_edges %>%
          dplyr::filter(node1 == .data$node1, node2 == .data$node2) %>%
          dplyr::pull(value)
      )
    ) %>%
    dplyr::select(id, p_value) # Select 'id' to match with summary data frame

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
    dplyr::left_join(p_values, by = c("Edge" = "id"))

  # Add significance stars based on p-values
  if (include_p_values == "stars") {
    bootnet_summary <- bootnet_summary %>%
      dplyr::mutate(
        Significance = dplyr::case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ ""
        ),
        `Edge weight bootstrapped (95% CI)` = paste0(`Edge weight bootstrapped (95% CI)`, " ", Significance)
      ) %>%
      dplyr::select(-p_value, -Significance)
  } else if (include_p_values == "exact") {
    bootnet_summary <- bootnet_summary
  } else if (include_p_values == "none") {
    bootnet_summary <- bootnet_summary %>%
      dplyr::select(-p_value)
  }

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
        dplyr::select(!`Edge weight sample`)
    }
  }

  return(bootnet_summary)
}

##############################################xx[]

# Function testing

##############################################xx[]

# # Example testing of function with 'bootnet' output
# set.seed(654)
# # Function to generate correlated data
# generate_correlated_data <- function(n, rho, num_vars) {
#   mu <- rep(0, num_vars)
#   Sigma <- matrix(rho, nrow = num_vars, ncol = num_vars)
#   diag(Sigma) <- 1
#   MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
# }
#
# # Simulate data and results
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
# correlated_data <- as_tibble(generate_correlated_data(n = 100, rho = 0.3, num_vars = 5))
# names(correlated_data) <- c("x1", "x2", "x3", "x4", "x5")
#
# test_dat <- bind_cols(independent_data, correlated_data)
#
#
# # Example usage with 'bootnet'
# estimate <- bootnet::estimateNetwork(test_dat, default = "mgm")
# predict_output <- predict(estimate$results, estimate$data)
# test_boot_non_para <- bootnet::bootnet(estimate, nBoots = 10)
#
# small_boot_table(bootnet_output = test_boot_non_para,
#                  include_sample_edge_weight = FALSE,
#                  predict_output
#                  ) # Edited to reflect correct function call
#
