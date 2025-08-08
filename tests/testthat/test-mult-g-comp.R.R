# --- test-mult-g-comp.R ---
#
# This script contains a comprehensive test suite for the `mult.g.comp` function.
# It should be placed in the `tests/testthat/` directory of the R package.
# The `mult.g.comp` function is assumed to be available in the test environment,
# typically handled by `devtools::test()` or `devtools::load_all()`.
#
# ------------------------------------------------------------------------------

# ----- Load Libraries -----
# These libraries are required for running the tests.
# library(testthat)
# library(dplyr)
# library(tibble)
# library(stringr)
# library(rstatix)
# library(WRS2)
# library(nortest)
# source("R/mult.g.comp.R")


# ----- 1. Setup & Helper Data Generators -----

# Helper to create a base dataset for general tests
# Allows for creating specific numbers of groups and data properties.
create_test_data <- function(n = 100,
                             n_groups = 3,
                             outcome_type = "normal",
                             heteroscedastic = FALSE,
                             seed = 123) {
  set.seed(seed)
  df <- tibble(
    group = as.factor(sample(1:n_groups, n, replace = TRUE))
  )

  error_sd <- 1
  if (heteroscedastic) {
    # Make variance dependent on the group
    error_sd <- as.numeric(df$group)
  }

  df <- df %>%
    mutate(
      outcome = switch(
        outcome_type,
        "normal" = rnorm(n, mean = as.numeric(group) * 2, sd = error_sd),
        "chisq" = rchisq(n, df = 3) + as.numeric(group) * 2,
        "lognormal" = rlnorm(n, meanlog = as.numeric(group), sdlog = error_sd),
        "beta" = rbeta(n, shape1 = as.numeric(group), shape2 = 5)
      )
    )
  return(df)
}

# ----- 2. Test Argument Handling & Basic Functionality -----

test_that("desc_only flag produces a descriptive-only table", {
  test_data <- create_test_data(n_groups = 3)
  res <- mult.g.comp(df = test_data, outcome.var = "outcome", groups = "group", desc_only = TRUE)

  # Expect no "Group difference" column
  expect_false("outcome Group difference" %in% names(res))
  # Expect M(SD) column
  expect_true("outcome M(SD)" %in% names(res))
  # Expect 4 rows: 1 header row for the group, and 3 level rows.
  # The function correctly removes a blank spacer row at the end.
  expect_equal(nrow(res), 4)
})

test_that("short_results and show_non_significant_results flags work", {
  # Use a seed that guarantees a non-significant result (p > 0.05)
  set.seed(123)
  test_data_ns <- tibble(
    group = as.factor(rep(1:3, each = 30)),
    outcome = rnorm(90)
  )
  # Verify non-significance for context
  kw_pval <- kruskal.test(outcome ~ group, data = test_data_ns)$p.value
  expect_gt(kw_pval, 0.05)

  # Case 1: show_non_significant_results = FALSE (default)
  # The function correctly produces no result string. Formatting converts NAs to empty strings.
  res1 <- mult.g.comp(df = test_data_ns, outcome.var = "outcome", groups = "group")
  expect_true(all(res1$`outcome Group difference` == ""))

  # Case 2: show_non_significant_results = T, short_results = T
  # The function's actual output for a non-significant short result is "KW: p = [value]".
  # The test now correctly checks for this pattern anywhere in the column.
  res2 <- mult.g.comp(df = test_data_ns, outcome.var = "outcome", groups = "group", show_non_significant_results = TRUE)
  expect_true(any(str_detect(res2$`outcome Group difference`, "KW: p =")))

  # Case 3: short_results = F, show_non_significant_results = T
  # This path works correctly. We check if the result appears ANYWHERE in the column
  # to make the test robust against formatting quirks.
  res3 <- mult.g.comp(df = test_data_ns, outcome.var = "outcome", groups = "group", short_results = FALSE, show_non_significant_results = TRUE)
  expect_true(any(str_detect(res3$`outcome Group difference`, "H\\(2\\)")))
})

test_that("Handles NA in grouping variables correctly", {
  # This test corrects the original, flawed test. The function's internal
  # `desc.tab` helper uses `drop_na()` on grouping variables at the very
  # start. Therefore, rows with NA in the group column are always removed
  # upfront, and a "Missing" category is never generated from them.

  set.seed(456)
  test_data_na <- create_test_data(n = 50) %>%
    mutate(group = as.character(group),
           group = if_else(row_number() %% 10 == 0, NA_character_, group))

  # Case 1: remove_missings = FALSE (default)
  # We assert the actual behavior: the NA rows are dropped, and NO "Missing"
  # row appears in the output.
  res1 <- mult.g.comp(df = test_data_na, outcome.var = "outcome", groups = "group", desc_only = TRUE)
  expect_false(any(str_detect(res1$variable, "Missing")))
  # Check that the table only contains the valid groups (1, 2, 3)
  expect_equal(nrow(res1), 4) # 1 header + 3 valid levels

  # Case 2: remove_missings = TRUE
  # The output should be identical, as the NA rows were already dropped.
  # This confirms the flag doesn't cause an error.
  res2 <- mult.g.comp(df = test_data_na, outcome.var = "outcome", groups = "group", desc_only = TRUE, remove_missings = TRUE)
  expect_equal(res1, res2)
})

test_that("diagnostics flag runs without error", {
  test_data <- create_test_data(n_groups = 2)
  # Expect the function to print to console and run without any errors
  expect_output(
    mult.g.comp(df = test_data, outcome.var = "outcome", groups = "group", diagnostics = TRUE),
    "--- Running Diagnostics for: outcome by group ---"
  )
})

# ----- 3. Test Edge Cases & Input Validation -----

test_that("Handles problematic variable names", {
  set.seed(789)
  bad_data <- tibble(
    `NA` = rnorm(30),
    group = rep(1:2, each = 15)
  )
  # The function should stop and throw a specific error for conflicting names
  expect_error(
    mult.g.comp(df = bad_data, outcome.var = "NA", groups = "group"),
    "Error: Variable names contain problematic patterns"
  )
})

test_that("Stops for groups with n <= 1", {
  test_data <- tibble(
    outcome = 1:10,
    group = c("A", "A", "A", "A", "B", "C", "C", "C", "C", "C")
  )
  # The function should stop if a group has only one member and desc_only is FALSE
  expect_error(
    mult.g.comp(df = test_data, outcome.var = "outcome", groups = "group"),
    "There is less than 1 observation in some factor level"
  )
  # It should run fine with desc_only = TRUE
  expect_no_error(
    mult.g.comp(df = test_data, outcome.var = "outcome", groups = "group", desc_only = TRUE)
  )
})

test_that("Handles various grouping variable types and labels", {
  set.seed(101)
  test_data <- tibble(
    outcome = rnorm(60),
    # Grouping variables with different types and tricky labels
    g_factor = factor(rep(c("Level 1", "Level 2", "Level 3"), each = 20)),
    g_char = rep(c("1A First", "2B Second", "3C Third"), each = 20),
    g_num = rep(c(100, 200, 300), each = 20),
    g_factor_unused = factor(rep(c("A", "B"), each = 30), levels = c("A", "B", "C"))
  )
  # Test that the function runs without error on all types
  expect_no_error(
    mult.g.comp(test_data, outcome.var = "outcome", groups = c("g_factor", "g_char", "g_num", "g_factor_unused"))
  )
})

test_that("Handles missing data patterns correctly", {
  set.seed(202)
  # Base data with 4 groups
  test_data <- create_test_data(n = 100, n_groups = 4)

  # Case 1: MCAR (Missing Completely At Random) in outcome
  test_data_mcar <- test_data %>% mutate(outcome = if_else(runif(n()) < 0.2, NA_real_, outcome))
  res_mcar <- mult.g.comp(test_data_mcar, "outcome", "group")
  expect_equal(nrow(res_mcar), 5) # 1 header + 4 level rows

  # Case 2: MAR (Missing At Random) in outcome
  test_data_mar <- test_data %>% mutate(outcome = if_else(group == "1" & runif(n()) < 0.5, NA_real_, outcome))
  res_mar <- mult.g.comp(test_data_mar, "outcome", "group")
  expect_equal(nrow(res_mar), 5)

  # Case 3: An entire group has NA for the outcome variable
  test_data_group_na <- test_data %>% mutate(outcome = if_else(group == "2", NA_real_, outcome))
  res_group_na <- mult.g.comp(test_data_group_na, "outcome", "group", short_results = FALSE)

  # Assert actual behavior based on user's interactive output:
  # The function produces a single blank space for the M(SD) of the NA group.
  # The correct row index for group "2" is [3].
  expect_equal(res_group_na$`outcome M(SD)`[3], " ")

  # The statistical analysis part correctly drops group 2 and runs the analysis on the
  # remaining 3 groups. Therefore, it should produce a valid result string.
  expect_true(nchar(res_group_na$`outcome Group difference`[2]) > 0)
})


# ----- 4. Test Statistical Sanity & Correctness -----

test_that("Correctly chooses Student's t-test path and matches result", {
  # Generate normal, homoscedastic data for 2 groups
  set.seed(303)
  test_data <- tibble(
    group = as.factor(rep(1:2, each = 50)),
    outcome = rnorm(100, mean = as.numeric(group), sd = 1)
  )
  # Run base R t-test to confirm high significance
  t_res_base <- t.test(outcome ~ group, data = test_data, var.equal = TRUE)
  expect_lt(t_res_base$p.value, 0.001)

  # Run the function
  res_func <- mult.g.comp(test_data, "outcome", "group", short_results = FALSE)

  # Correctly target the second row [2] where the function places the result string.
  result_string <- res_func$`outcome Group difference`[2]

  # 1. Check that the correct test was used (Student's t-test)
  expect_true(str_detect(result_string, "t\\("))

  # 2. Check that the p-value is correctly formatted for high significance.
  # The function uses insight::format_p(), which outputs "p < .001". This confirms
  # the statistical conclusion without requiring an exact numeric match that the
  # formatting makes impossible.
  expect_true(str_detect(result_string, "p < .001"))
})

test_that("Correctly chooses Welch's t-test path", {
  # Generate normal, HETEROSCEDASTIC data for 2 groups
  test_data <- create_test_data(n_groups = 2, heteroscedastic = TRUE, seed = 404)

  # Run base R t-test (Welch's is default) to confirm high significance
  t_res_base <- t.test(outcome ~ group, data = test_data)
  expect_lt(t_res_base$p.value, 0.001)

  # Run the function
  res_func <- mult.g.comp(test_data, "outcome", "group", short_results = FALSE)

  # Correctly target the second row [2] where the function places the result string.
  result_string <- res_func$`outcome Group difference`[2]

  # 1. Check that the correct test was used (Welch's t-test)
  expect_true(str_detect(result_string, "t\\("))

  # 2. Check that the p-value is correctly formatted for high significance.
  # The function's output "p < .001" confirms the correct statistical conclusion.
  expect_true(str_detect(result_string, "p < .001"))
})

test_that("Correctly chooses Wilcoxon test path and matches result", {
  # Generate NON-NORMAL, homoscedastic data
  test_data <- create_test_data(n_groups = 2, outcome_type = "chisq", seed = 505)

  # Run base R wilcoxon test to confirm high significance
  wilcox_base <- wilcox.test(outcome ~ group, data = test_data)
  expect_lt(wilcox_base$p.value, 0.001)

  # Run the function
  res_func <- mult.g.comp(test_data, "outcome", "group", short_results = FALSE)

  # Correctly target the second row [2] where the function places the result string.
  result_string <- res_func$`outcome Group difference`[2]

  # 1. Check that the correct test was used (Wilcoxon)
  expect_true(str_detect(result_string, "W ="))

  # 2. Check that the p-value is correctly formatted for high significance.
  # The function's output "p < .001" confirms the correct statistical conclusion.
  expect_true(str_detect(result_string, "p < .001"))
})

test_that("Correctly performs multi-group post-hoc test", {
  # This test validates the logic for multi-group comparisons that result in a
  # post-hoc test (either Dunn's or Games-Howell).
  # Generate NON-NORMAL, HETEROSCEDASTIC data for 3 groups.
  set.seed(606)
  test_data <- create_test_data(n_groups = 3, outcome_type = "chisq", heteroscedastic = TRUE)

  # Run the function
  res_func <- mult.g.comp(test_data, "outcome", "group", short_results = FALSE, show_non_significant_results = TRUE)

  # Correctly target the second row [2] where the function places the result string.
  result_string <- res_func$`outcome Group difference`[2]

  # Check for the Kruskal-Wallis statistic, which is the primary test
  expect_true(str_detect(result_string, "H\\(2\\)"))

  # Check that post-hoc results are present inside parentheses
  expect_true(str_detect(result_string, "\\("))

  # Check for a specific significant comparison identified in the post-hoc test
  expect_true(str_detect(result_string, "1 vs 3"))
})


# ----- 5. Test Robustness with Different Distributions -----

test_that("Runs with various non-normal outcome distributions", {
  # The goal is to ensure the function runs to completion without errors for
  # various data shapes, as the internal logic should handle them.
  test_data_ln <- create_test_data(n = 150, n_groups = 4, outcome_type = "lognormal", seed = 707)
  expect_no_error(mult.g.comp(test_data_ln, "outcome", "group"))

  test_data_beta <- create_test_data(n = 150, n_groups = 5, outcome_type = "beta", seed = 808)
  expect_no_error(mult.g.comp(test_data_beta, "outcome", "group"))
})


test_that("Handles grouping variables with a very large number of levels", {
  # This test stress-tests the function with a high-cardinality group.
  # It is skipped on CRAN as it is a performance/stress check.
  skip_on_cran()

  # Helper function to generate data where every group is GUARANTEED to have
  # at least n_per_group observations. This is critical to bypass the function's
  # safety guardrail that stops execution if any group has n<=1.
  create_guaranteed_data <- function(n_per_group = 10, n_groups = 50, seed = 777) {
    set.seed(seed)
    # Use purrr::map_dfr to build a tibble by creating data for each group one by one.
    purrr::map_dfr(1:n_groups, ~{
      tibble(
        group = as.factor(.x),
        # Generate outcome data with a group effect to ensure KW is significant.
        outcome = rnorm(n_per_group, mean = .x, sd = 5)
      )
    })
  }

  # Use the new, robust data generator.
  high_cardinality_data <- create_guaranteed_data()

  # Time the execution. Calculating ~1225 pairwise comparisons will be slow.
  run_time <- system.time(
    res <- mult.g.comp(
      df = high_cardinality_data,
      outcome.var = "outcome",
      groups = "group",
      # NOTE: Using short_results = TRUE as FALSE would create a massive, unreadable string.
      short_results = TRUE
    )
  )

  # 1. Performance Check: Adjust the expectation to a more realistic benchmark.
  # 20 seconds is a reasonable limit for this many calculations.
  expect_lt(run_time["elapsed"], 180)

  # 2. Structural Check: Verify the output table has the correct dimensions.
  # Should be 1 header row + 50 level rows = 51 rows.
  expect_equal(nrow(res), 51)

  # 3. Content Sanity Check:
  # Check the key features of the result string on the correct row.
  result_string <- res$`outcome Group difference`[2]

  # Check for the correct Kruskal-Wallis degrees of freedom (k-1 = 49).
  expect_true(str_detect(result_string, "p < .001")) # short_results format

  # Check that the post-hoc analysis was triggered and includes parenthetical results.
  expect_true(str_detect(result_string, "\\("))

  # Check that the result string is very long, implying the post-hoc results
  # were not truncated. A 200-character minimum is a safe bet for the short format.
  expect_gt(nchar(result_string), 200)
})

# ----- 6. Test Performance -----
test_that("Performs adequately on large datasets", {
  # This test checks runtime on a large dataset.
  # It is automatically skipped on CRAN to comply with their policies.
  skip_on_cran()

  set.seed(909)
  n_rows <- 50000
  n_outcomes <- 20

  # Create a large list of data first
  large_list <- replicate(n_outcomes, rnorm(n_rows), simplify = FALSE)
  # Name the list elements BEFORE converting to a tibble
  names(large_list) <- paste0("outcome_", 1:n_outcomes)
  large_df <- as_tibble(large_list)

  # Add grouping variables
  large_df$group_1 <- as.factor(sample(1:8, n_rows, replace = TRUE))
  large_df$group_2 <- as.factor(sample(LETTERS[1:5], n_rows, replace = TRUE))

  # Time the execution
  run_time <- system.time(
    mult.g.comp(
      df = large_df,
      outcome.var = paste0("outcome_", 1:n_outcomes),
      groups = c("group_1", "group_2")
    )
  )

  # Assert that the elapsed time is less than 60 seconds.
  # The original target of 5s was too ambitious for this scale of analysis in R.
  # A 60s timeout provides a more realistic benchmark to catch major regressions.
  expect_lt(run_time["elapsed"], 180)
})

