# library(bootnet)
# library(tidyverse)
#
# # Define main categories, subcategories, measures, and result types
# main_categories <- c("total", "item")
# subcategories <- c("crude", "adjusted")
# measures <- c("OASIS", "ODSIS")
# result_types <- c("edge_predictability_tab", "edge_weight_sum_tab")
#
# # Generate combinations
# combinations <- expand.grid(main_category = main_categories,
#                             subcategory = subcategories,
#                             measure = measures,
#                             result_type = result_types)
#
# # Create names for the results
# combinations$result_name <- with(combinations,
#                                  paste(measure, subcategory, main_category, sep = "_"))
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
#
# simulate_results <- function() {
#   # Generate first 5 independent variables
#   independent_data <- tibble(
#     "Group" = rbinom(100, size = 1, prob = .5),
#     "y" = ifelse(Group == 0,
#                  rnorm(100, mean = 50, sd = 10),
#                  rnorm(100, mean = 40, sd = 12)),
#     "z" = rnorm(100, mean = 41, sd = 11),
#     "w" = rnorm(100, mean = 40, sd = 10),
#     "v" = rnorm(100, mean = 45, sd = 9)
#   )
#
#   # Generate last 5 correlated variables
#   correlated_data <- as_tibble(generate_correlated_data(n = 100, rho = 0.4, num_vars = 5))
#   names(correlated_data) <- c("x1", "x2", "x3", "x4", "x5")
#
#   # Combine all data
#   test_dat <- bind_cols(independent_data, correlated_data)
#
#   estimate <- bootnet::estimateNetwork(test_dat, default = "mgm")
#   boot_results <- bootnet::bootnet(estimate, nBoots = 3, type = "nonparametric")
#   predict_output <- predict(estimate$results, estimate$data)
#
#   edge_summary <- edge_weight_summary(bootnet_output = boot_results, include_sample_edge_weight = FALSE, include_p_values = "stars")
#   predictability_summary <- node_predictability_summary(predict_function_output = predict_output)
#
#   return(list(edge_summary = edge_summary, predictability_summary = predictability_summary))
# }
#
# # Iterate over combinations and save results to the environment
# for (i in 1:nrow(combinations)) {
#   combination_name <- combinations$result_name[i]
#   result <- simulate_results()
#
#   # Save edge weight summary and node predictability summary to the environment
#   edge_weight_var <- paste0(combination_name, "_edge_weight_sum_tab")
#   predictability_var <- paste0(combination_name, "_edge_predictability_tab")
#
#   assign(edge_weight_var, result$edge_summary)
#   assign(predictability_var, result$predictability_summary)
#
#   # Debugging print statements
#   print(paste0("Created variable: ", edge_weight_var))
#   print(paste0("Created variable: ", predictability_var))
# }
#
# # Example access to a specific result
# print(OASIS_crude_total_edge_weight_sum_tab)
# print(OASIS_crude_total_edge_predictability_tab)
#
# # # In my network analysis conducted in R, I created two categories of analysis: Total score level and Item level. Under these umbrella categories of analysis, I also estimated two subcategories: Crude and Adjusted. And finally, two subcategories (Anxiety and Depression). Anxiety was measured by the OASIS, and depression by ODSIS. In each of these combinations, abbreviated, for instance, as this: OASIS_crude_total, I calculated edge weight summary and node predictability. So output from nodes predictability looks like this: OASIS_crude_total_edge_predictability_tab and output from edge weight summary as this: OASIS_crude_total_edge_weight_sum_tab. Now, I want to write a code to create a table that would take all combinations of results and, using a flexible package, create a hierarchical table reflecting these hierarchical categories. but first, let's create a list of combinations of results just to check that we are on the right track.
#
#
# library(dplyr)
# library(tidyr)
# library(purrr)
#
# # Define the existing result names
# existing_results <- c(
#   "OASIS_crude_total_edge_weight_sum_tab",
#   "OASIS_adjusted_total_edge_weight_sum_tab",
#   "ODSIS_crude_total_edge_weight_sum_tab",
#   "ODSIS_adjusted_total_edge_weight_sum_tab",
#   "OASIS_crude_item_edge_weight_sum_tab",
#   "OASIS_adjusted_item_edge_weight_sum_tab",
#   "ODSIS_crude_item_edge_weight_sum_tab",
#   "ODSIS_adjusted_item_edge_weight_sum_tab",
#   "OASIS_crude_total_edge_predictability_tab",
#   "OASIS_adjusted_total_edge_predictability_tab",
#   "ODSIS_crude_total_edge_predictability_tab",
#   "ODSIS_adjusted_total_edge_predictability_tab",
#   "OASIS_crude_item_edge_predictability_tab",
#   "OASIS_adjusted_item_edge_predictability_tab",
#   "ODSIS_crude_item_edge_predictability_tab",
#   "ODSIS_adjusted_item_edge_predictability_tab"
# )
#
# # Function to safely bind rows, removing NULLs
# safe_bind_rows <- function(data_list) {
#   bind_rows(data_list[!sapply(data_list, is.null)])
# }
#
# # Extract and combine node predictability objects with metadata and extracted data
# extract_predictability_data <- function(data, source_name) {
#   data %>%
#     mutate(Source = source_name) %>%
#     separate(`Node predictability`, into = c("Node", "Predictability"), sep = " = ") %>%
#     mutate(Predictability = as.numeric(Predictability))
# }
#
# # Generate sorted list of objects based on the existing result names
# sorted_objects <- list(
#   total_edge_weight_sum = existing_results[grepl("total_edge_weight_sum_tab", existing_results)],
#   item_edge_weight_sum = existing_results[grepl("item_edge_weight_sum_tab", existing_results)],
#   total_edge_predictability = existing_results[grepl("total_edge_predictability_tab", existing_results)],
#   item_edge_predictability = existing_results[grepl("item_edge_predictability_tab", existing_results)]
# )
#
# # Combine edge weight sum objects
# combined_total_edge_weight_sum <- safe_bind_rows(
#   lapply(sorted_objects$total_edge_weight_sum, function(var_name) {
#     if (exists(var_name, envir = .GlobalEnv)) {
#       get(var_name, envir = .GlobalEnv) %>% mutate(Source = var_name)
#     } else {
#       print(paste("Variable", var_name, "does not exist"))
#       NULL
#     }
#   })
# )
#
# combined_item_edge_weight_sum <- safe_bind_rows(
#   lapply(sorted_objects$item_edge_weight_sum, function(var_name) {
#     if (exists(var_name, envir = .GlobalEnv)) {
#       get(var_name, envir = .GlobalEnv) %>% mutate(Source = var_name)
#     } else {
#       print(paste("Variable", var_name, "does not exist"))
#       NULL
#     }
#   })
# )
#
# combined_edge_weight_sum <- bind_rows(combined_total_edge_weight_sum, combined_item_edge_weight_sum)
#
# # Combine edge predictability objects
# combined_total_edge_predictability <- safe_bind_rows(
#   lapply(sorted_objects$total_edge_predictability, function(var_name) {
#     if (exists(var_name, envir = .GlobalEnv)) {
#       extract_predictability_data(get(var_name, envir = .GlobalEnv), var_name)
#     } else {
#       print(paste("Variable", var_name, "does not exist"))
#       NULL
#     }
#   })
# )
#
# combined_item_edge_predictability <- safe_bind_rows(
#   lapply(sorted_objects$item_edge_predictability, function(var_name) {
#     if (exists(var_name, envir = .GlobalEnv)) {
#       extract_predictability_data(get(var_name, envir = .GlobalEnv), var_name)
#     } else {
#       print(paste("Variable", var_name, "does not exist"))
#       NULL
#     }
#   })
# )
#
# combined_edge_predictability <- bind_rows(combined_total_edge_predictability, combined_item_edge_predictability)
#
# # Print combined tibbles
# print(combined_edge_weight_sum)
# print(combined_edge_predictability)
#
# # Ensure the data frames are not empty before reshaping
# if (nrow(combined_edge_weight_sum) > 0) {
#   reshaped_tibble_edge_weight <- combined_edge_weight_sum %>%
#     select(Edge, Source, starts_with("Edge weight bootstrapped (95% CI)")) %>%
#     pivot_wider(
#       names_from = Source,
#       values_from = `Edge weight bootstrapped (95% CI)`,
#       names_prefix = "Edge weight bootstrapped (95% CI) "
#     )
#   print(reshaped_tibble_edge_weight) %>% view()
# } else {
#   print("No data available for edge weight summary.")
# }
#
# if (nrow(combined_edge_predictability) > 0) {
#   reshaped_tibble_predictability <- combined_edge_predictability %>%
#     pivot_wider(
#       names_from = Source,
#       values_from = Predictability,
#       names_prefix = "Predictability "
#     )
#   print(reshaped_tibble_predictability) %>% view()
# } else {
#   print("No data available for node predictability.")
# }
